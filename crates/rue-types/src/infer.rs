//! Generic inference: the candidates collected for the generics being inferred while comparing
//! (`compare_with_inference` and `infer_from_expected` in `compare.rs`), and how a type is picked for
//! each of them from those candidates.

mod bounds;
mod resolve;
mod solve;

pub use bounds::*;
pub use resolve::*;
pub use solve::*;

use std::mem;

use id_arena::Arena;
use indexmap::IndexMap;

use crate::{BuiltinTypes, Comparison, FxHashMap, Type, TypeId, compare};

/// The constraints collected for a generic type parameter during inference.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct InferenceCandidates {
    /// Types that flowed into the generic from a covariant position, so the generic must be able
    /// to hold all of them. For example, passing an `Int` to a parameter of type `T`.
    pub lower: Vec<TypeId>,

    /// Types that the generic flows into from a contravariant position (a function parameter),
    /// so the generic must be assignable to all of them. For example, passing a
    /// `fn(x: Bytes32) -> Bytes32` to a parameter of type `fn(x: T) -> Bytes32`.
    pub upper: Vec<TypeId>,

    /// Types that flowed into a union containing the generic, but that the union's other members
    /// already hold, such as `nil` when passing `Int | nil` to `T | nil`. They're only used if
    /// nothing else constrains the generic.
    pub held: Vec<TypeId>,
}

/// Tracks the candidates for the generic type parameters being inferred. Only the generics that
/// the inference was created with are inferred, so generics that are bound elsewhere (such as by
/// a generic function type in a parameter, or by an enclosing function) are left rigid.
#[derive(Debug, Default, Clone)]
pub struct Inference {
    pub(crate) generics: IndexMap<TypeId, InferenceCandidates>,
    /// The generics being inferred that were renamed from another generic, such as the generics of
    /// a function in a recursive call to it, which are renamed from the caller's (see
    /// `Inference::rename`).
    pub(crate) renamed: FxHashMap<TypeId, TypeId>,
    /// The union members that each candidate was collected under, parallel to the lists in
    /// `generics` (see `CandidateBranches`).
    branches: IndexMap<TypeId, CandidateBranches>,
    /// The unions that values were compared against while collecting the candidates, indexed by
    /// `BranchId::choice`.
    choices: Choices,
    /// The vars in `generics` that stand for an instance of a passed generic function's generic
    /// rather than for a generic of the function being called (`VarKind::Instance` in #362), with
    /// the instance's bounds as their candidates (see `Inference::add_instance`).
    instances: Vec<TypeId>,
    /// How those vars are resolved.
    policy: InstancePolicy,
    /// The vars added along with the generics (see `Inference::add_var`).
    added: Vec<TypeId>,
    /// Whether resolving leaves out the candidates of union members that another member's implied
    /// candidates make unneeded (`Inference::implied_members`). `CallSolver::solve` only does when
    /// the generics resolved without leaving them out break a bound the type expected of the call
    /// gives them.
    pub(crate) narrow: bool,
}

/// How the vars for passed generic functions' instances are resolved (see
/// `Inference::add_instance`).
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum InstancePolicy {
    /// Each is resolved on its own from its bounds as soon as the comparison that instantiated it
    /// has finished, and replaced in the candidates that reference it (see
    /// `Inference::resolve_instances`). This is for the inferences that are only a step of
    /// solving, such as the bounds implied by a call's expected type.
    #[default]
    Eager,
    /// They stay in the inference, and each solve passes on to them the bounds that the generics of
    /// the function being called imply, such as the type expected of the call, before they're
    /// instantiated. One that's only contravariant in a lower or held candidate, such as `T` in
    /// `fn(x: T) -> nil`, takes the greatest instantiation rather than the least (see
    /// `Inference::resolve_instances`).
    Joint,
}

/// One member of a union that a value was compared against while inferring, or the fallback that
/// the union's comparison falls back to when no member holds the value (`FALLBACK`). A candidate
/// collected while comparing the value with a member is only needed if the member matched.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct BranchId {
    pub(crate) choice: usize,
    pub(crate) member: usize,
}

/// A pair of bounds on an instance that didn't hold (and what comparing them gave), with the
/// union member each was collected under, if it's one of a union compared inside of the function
/// comparison whose bounds are being checked (see `Choices::attributed`).
pub(crate) type FailedPair = (Option<BranchId>, Option<BranchId>, Comparison);

/// A union member whose candidates aren't needed, with the generics and kinds of candidate that
/// are left out (see `Inference::implied_members`).
type LeftOut = (BranchId, Vec<(TypeId, CandidateKind)>);

/// The `BranchId::member` of a union comparison's fallback, such as comparing a function's runtime
/// structure with the union when no member holds the function (#349).
pub(crate) const FALLBACK: usize = usize::MAX;

/// The unions that values were compared against, indexed by `BranchId::choice`, and what each of
/// their members gave, which decides which members' candidates or bounds hold.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub(crate) struct Choices(Vec<Choice>);

/// A comparison of a value with the members of a union, any one of which can hold it.
#[derive(Debug, Clone, PartialEq, Eq)]
struct Choice {
    /// The member that the union itself was compared under, if any.
    parent: Option<BranchId>,
    /// What comparing the value with each member gave, once it has been compared.
    members: Vec<Option<Comparison>>,
    /// What the fallback gave, if it was compared.
    fallback: Option<Comparison>,
    /// The members (with the fallback last) that collected bounds for a generic being
    /// instantiated, which are only used by `Choices::matched_so_far`.
    bounded: Vec<bool>,
}

/// The union members that each of a generic's candidates was collected under, in the same order
/// as its `InferenceCandidates`. A candidate holds only if every one of them matched. Most are
/// collected under at most one (the innermost, whose choice records the ones it's nested in), and
/// one that's passed on from another holds only if the ones that both of them were collected under
/// matched too.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
struct CandidateBranches {
    lower: Vec<Vec<BranchId>>,
    upper: Vec<Vec<BranchId>>,
    held: Vec<Vec<BranchId>>,
}

impl CandidateBranches {
    fn list(&self, kind: CandidateKind) -> &Vec<Vec<BranchId>> {
        match kind {
            CandidateKind::Lower => &self.lower,
            CandidateKind::Upper => &self.upper,
            CandidateKind::Held => &self.held,
        }
    }

    fn list_mut(&mut self, kind: CandidateKind) -> &mut Vec<Vec<BranchId>> {
        match kind {
            CandidateKind::Lower => &mut self.lower,
            CandidateKind::Upper => &mut self.upper,
            CandidateKind::Held => &mut self.held,
        }
    }
}

impl Inference {
    pub fn new(generics: impl IntoIterator<Item = TypeId>) -> Self {
        let generics: IndexMap<TypeId, InferenceCandidates> = generics
            .into_iter()
            .map(|generic| (generic, InferenceCandidates::default()))
            .collect();

        Self {
            branches: generics
                .keys()
                .map(|&generic| (generic, CandidateBranches::default()))
                .collect(),
            generics,
            renamed: FxHashMap::default(),
            choices: Choices::default(),
            instances: Vec::new(),
            policy: InstancePolicy::Eager,
            added: Vec::new(),
            narrow: false,
        }
    }

    /// Whether the two hold the same candidates, in the same order, with the same union members and
    /// vars, so that anything computed from one gives the same result for the other.
    /// `IndexMap`'s own equality ignores the order, which the result can depend on.
    pub(crate) fn is_identical(&self, other: &Self) -> bool {
        self.generics.iter().eq(other.generics.iter())
            && self.branches.iter().eq(other.branches.iter())
            && self.renamed == other.renamed
            && self.choices == other.choices
            && self.instances == other.instances
            && self.policy == other.policy
            && self.added == other.added
            && self.narrow == other.narrow
    }

    /// Records that a generic being inferred was renamed from another generic. They're compared as
    /// the same type where neither is bound, since they were the same generic before, and the
    /// other one is a candidate of this one where they match. But a candidate that references this
    /// one is resolved through what it's inferred as, unlike one that references the other.
    pub fn rename(&mut self, generic: TypeId, from: TypeId) {
        self.renamed.insert(generic, from);
    }

    /// Adds a var to be inferred along with the generics, which is at most `upper`, such as the type
    /// of an untyped lambda parameter whose body was compiled with it as `upper` (#335).
    pub fn add_var(&mut self, var: TypeId, upper: TypeId) {
        self.generics.insert(var, InferenceCandidates::default());
        self.branches.insert(var, CandidateBranches::default());
        self.added.push(var);
        self.push(var, CandidateKind::Upper, upper, Vec::new());
    }

    /// Whether the generic is a var that stands for a type that isn't known until the bounds passed
    /// on to it decide it: a passed generic function's instance, or a var added along with the
    /// generics, such as an untyped lambda parameter's type. A bound can't be checked against a
    /// lower bound that references one (see `pass_upper_bounds`).
    pub(crate) fn is_undecided(&self, generic: TypeId) -> bool {
        self.is_instance(generic) || self.added.contains(&generic)
    }

    /// The vars added along with the generics (see `Inference::add_var`).
    pub(crate) fn added(&self) -> &[TypeId] {
        &self.added
    }

    pub(crate) fn has_undecided(&self) -> bool {
        !self.instances.is_empty() || !self.added.is_empty()
    }

    /// Sets how the vars for passed generic functions' instances are resolved.
    pub fn set_instance_policy(&mut self, policy: InstancePolicy) {
        self.policy = policy;
    }

    pub(crate) fn instance_policy(&self) -> InstancePolicy {
        self.policy
    }

    pub(crate) fn has_instances(&self) -> bool {
        !self.instances.is_empty()
    }

    /// Replaces the bounds of the vars for passed generic functions' instances with the ones they
    /// have in the other inference, which has the same candidates with more added.
    pub(crate) fn take_instance_bounds(&mut self, other: &Inference) {
        for &var in &self.instances {
            self.generics.insert(var, other.generics[&var].clone());
            self.branches.insert(var, other.branches[&var].clone());
        }
    }

    /// Whether the generic is a var for a passed generic function's instance.
    pub(crate) fn is_instance(&self, generic: TypeId) -> bool {
        self.instances.contains(&generic)
    }

    /// Every candidate collected for the generic, including those collected under a union member
    /// that didn't match the value.
    pub fn candidates(&self, generic: TypeId) -> Option<&InferenceCandidates> {
        self.generics.get(&generic)
    }

    /// The candidates collected for the generic that hold, leaving out those collected under a
    /// union member that didn't match the value (see `Choices::matched_with`). For example,
    /// passing a function to `T | (T, Int) | nil` matches the `T` member, so what comparing the
    /// function's runtime structure with `(T, Int)` collected for `T` is left out (#364).
    pub fn matched_candidates(&self, generic: TypeId) -> Option<InferenceCandidates> {
        self.candidates_without(generic, &[])
    }

    /// The candidates that weren't collected under any union member, with the generic and kind
    /// they're for, which every solution has to satisfy.
    pub(crate) fn unconditional(&self) -> Vec<(TypeId, CandidateKind, TypeId)> {
        let mut unconditional = Vec::new();

        for (&generic, candidates) in &self.generics {
            let branches = &self.branches[&generic];

            for kind in CandidateKind::ALL {
                for (&ty, branches) in candidates.list(kind).iter().zip(branches.list(kind)) {
                    if branches.is_empty() {
                        unconditional.push((generic, kind, ty));
                    }
                }
            }
        }

        unconditional
    }

    /// Like `Inference::matched_candidates`, but also leaving out the candidates of each kind in
    /// `left_out` for the generic that were collected under its union member, or in a union nested
    /// in it.
    fn candidates_without(
        &self,
        generic: TypeId,
        left_out: &[LeftOut],
    ) -> Option<InferenceCandidates> {
        let candidates = self.generics.get(&generic)?;
        let branches = &self.branches[&generic];
        let results = self.choices.results();

        let matched = |kind: CandidateKind, types: &[TypeId]| -> Vec<TypeId> {
            types
                .iter()
                .zip(branches.list(kind))
                .filter(|(_, branches)| {
                    branches.iter().all(|&branch| {
                        self.choices.matched_with(&results, branch)
                            && !left_out.iter().any(|(member, kinds)| {
                                kinds.contains(&(generic, kind))
                                    && self.choices.is_under(branch, *member)
                            })
                    })
                })
                .map(|(&ty, _)| ty)
                .collect()
        };

        Some(InferenceCandidates {
            lower: matched(CandidateKind::Lower, &candidates.lower),
            upper: matched(CandidateKind::Upper, &candidates.upper),
            held: matched(CandidateKind::Held, &candidates.held),
        })
    }

    /// The union members whose candidates aren't needed although they matched, since another
    /// member of the same union matched whose candidates are all implied by candidates that hold
    /// anyway (#362, §5, question 2). For example, `5 | (5, 5)` against `T | (T, T)` matches both
    /// `T` and `(T, T)` for `(5, 5)`, but `(T, T)`'s `T ≥ 5` is implied by the `T ≥ 5` that `5`
    /// gives, so `T ≥ (5, 5)` from the `T` member isn't needed, and `T` is `5`.
    ///
    /// - **Implied:** a lower bound `T ≥ c` is implied by `T ≥ l` with `c` assignable to `l`, and
    ///   an upper bound `T ≤ c` by `T ≤ u` with `u` assignable to `c`. A held candidate doesn't
    ///   constrain the generic, so it's implied.
    /// - **Hold anyway:** the candidates that matched and aren't under a member of a union where
    ///   several members have candidates, so no union leaves them out. Each union is decided from
    ///   these alone, so the order of the unions doesn't matter.
    /// - **Kept:** the first implied member keeps all of its candidates, so any solution holds the
    ///   value by it. Implication is what makes keeping it free: its lower and upper candidates add
    ///   nothing to those that hold anyway, and no member's held candidates are left out, so which
    ///   implied member is kept doesn't change the result.
    /// - **Left out:** the other members' lower and upper candidates of a generic and kind that has
    ///   a candidate outside of the union, so that a generic only the union's members constrain
    ///   keeps what they give it. A generic and kind that would be left with none, because the
    ///   unions left out each other's, keeps them all.
    ///
    /// This only runs for an inference that's `narrow` (see `CallSolver::solve`).
    fn implied_members(&self, arena: &mut Arena<Type>, builtins: &BuiltinTypes) -> Vec<LeftOut> {
        let results = self.choices.results();

        // Every candidate that matched, with the generic and kind it's for and the members it
        // was collected under
        let candidates: Vec<(TypeId, CandidateKind, TypeId, &Vec<BranchId>)> = self
            .generics
            .iter()
            .flat_map(|(&generic, candidates)| {
                let branches = &self.branches[&generic];
                CandidateKind::ALL.into_iter().flat_map(move |kind| {
                    candidates
                        .list(kind)
                        .iter()
                        .zip(branches.list(kind))
                        .map(move |(&ty, branches)| (generic, kind, ty, branches))
                })
            })
            .filter(|(_, _, _, branches)| {
                branches
                    .iter()
                    .all(|&branch| self.choices.matched_with(&results, branch))
            })
            .collect();

        // The members of each union that have candidates under them, and the candidates under
        // each of those (by index), along with the unions each candidate is in
        let mut members: Vec<Vec<usize>> = vec![Vec::new(); self.choices.len()];
        let mut under: FxHashMap<BranchId, Vec<usize>> = FxHashMap::default();
        let mut inside: Vec<Vec<usize>> = Vec::with_capacity(candidates.len());

        for (index, &(_, _, _, branches)) in candidates.iter().enumerate() {
            let mut unions: Vec<usize> = Vec::new();

            for &branch in branches {
                let mut current = Some(branch);

                while let Some(member) = current {
                    if !members[member.choice].contains(&member.member) {
                        members[member.choice].push(member.member);
                    }

                    let list = under.entry(member).or_default();

                    if list.last() != Some(&index) {
                        list.push(index);
                    }

                    if !unions.contains(&member.choice) {
                        unions.push(member.choice);
                    }

                    current = self.choices.parent(member);
                }
            }

            inside.push(unions);
        }

        // For each generic and kind, the unions that every one of its candidates is in, so that it
        // has a candidate outside of any other union
        let mut within: Vec<((TypeId, CandidateKind), Vec<usize>)> = Vec::new();

        for (index, &(generic, kind, _, _)) in candidates.iter().enumerate() {
            match within.iter_mut().find(|(pair, _)| *pair == (generic, kind)) {
                Some((_, unions)) => unions.retain(|choice| inside[index].contains(choice)),
                None => within.push(((generic, kind), inside[index].clone())),
            }
        }

        // The candidates that hold anyway, since they aren't under a member of a union with
        // several members that have candidates
        let anyway: Vec<(TypeId, CandidateKind, TypeId)> = candidates
            .iter()
            .filter(|(_, _, _, branches)| {
                branches.iter().all(|&branch| {
                    let mut current = Some(branch);

                    while let Some(member) = current {
                        if members[member.choice].len() >= 2 {
                            return false;
                        }

                        current = self.choices.parent(member);
                    }

                    true
                })
            })
            .map(|&(generic, kind, ty, _)| (generic, kind, ty))
            .collect();

        let mut left_out: Vec<LeftOut> = Vec::new();

        // Whether the type is assignable to the other, for each pair compared so far, since the
        // same candidates are compared for each union
        let mut assigns = FxHashMap::<(TypeId, TypeId), bool>::default();
        let mut assign = |from: TypeId, to: TypeId| {
            *assigns
                .entry((from, to))
                .or_insert_with(|| compare(arena, builtins, from, to) == Comparison::Assign)
        };

        for (choice, members) in members.iter_mut().enumerate() {
            if members.len() < 2 {
                continue;
            }

            members.sort_unstable();

            let implied = members.iter().copied().find(|&member| {
                under[&BranchId { choice, member }]
                    .iter()
                    .map(|&index| candidates[index])
                    .all(|(generic, kind, ty, _)| {
                        let mut anyway = anyway.iter().filter(|&&(other, other_kind, _)| {
                            other == generic && other_kind == kind
                        });

                        match kind {
                            CandidateKind::Held => true,
                            CandidateKind::Lower => anyway.any(|&(_, _, bound)| assign(ty, bound)),
                            CandidateKind::Upper => anyway.any(|&(_, _, bound)| assign(bound, ty)),
                        }
                    })
            });

            let Some(implied) = implied else {
                continue;
            };

            // Only the candidates of a generic and kind that has a candidate outside of the union
            // are left out, so that a generic that only its members constrain keeps what they
            // give it
            let outside: Vec<(TypeId, CandidateKind)> = within
                .iter()
                .filter(|(_, unions)| !unions.contains(&choice))
                .map(|&(pair, _)| pair)
                .collect();

            if outside.is_empty() {
                continue;
            }

            // Each other member leaves out only the generics and kinds it has candidates for
            for &member in members.iter().filter(|&&member| member != implied) {
                let member = BranchId { choice, member };
                let mut kinds: Vec<(TypeId, CandidateKind)> = Vec::new();

                for &index in &under[&member] {
                    let (generic, kind, _, _) = candidates[index];

                    // Held candidates are only used when nothing else constrains the generic, so
                    // which member's are kept would decide it
                    if kind != CandidateKind::Held
                        && outside.contains(&(generic, kind))
                        && !kinds.contains(&(generic, kind))
                    {
                        kinds.push((generic, kind));
                    }
                }

                if !kinds.is_empty() {
                    left_out.push((member, kinds));
                }
            }
        }

        // A generic and kind whose candidates outside of one union were all left out by others
        // keeps them all, since no kept member gives it one
        let mut checked: Vec<(TypeId, CandidateKind)> = Vec::new();
        let mut emptied: Vec<(TypeId, CandidateKind)> = Vec::new();

        for (_, kinds) in &left_out {
            for &(generic, kind) in kinds {
                if checked.contains(&(generic, kind)) {
                    continue;
                }

                checked.push((generic, kind));

                let kept = candidates.iter().any(|&(other, other_kind, _, branches)| {
                    other == generic
                        && other_kind == kind
                        && !branches.iter().any(|&branch| {
                            left_out.iter().any(|(member, kinds)| {
                                kinds.contains(&(generic, kind))
                                    && self.choices.is_under(branch, *member)
                            })
                        })
                });

                if !kept {
                    emptied.push((generic, kind));
                }
            }
        }

        for (_, kinds) in &mut left_out {
            kinds.retain(|pair| !emptied.contains(pair));
        }

        left_out
    }

    /// The inference with each generic's candidates replaced by those that hold
    /// (`Inference::matched_candidates`), without those of union members that another member's
    /// implied candidates make unneeded (`Inference::implied_members`), which is what the generics
    /// are resolved from. A generic whose candidates all come from union members that didn't
    /// match keeps every one of them, as in `pick(ident)` with
    /// `fn pick<U>(f: fn(x: Int) -> (Int, Int) | (U, Bytes))`, where `U`'s only candidate comes
    /// from the `(U, Bytes)` member.
    pub(crate) fn keep_matched(
        &self,
        arena: &mut Arena<Type>,
        builtins: &BuiltinTypes,
    ) -> Inference {
        let left_out = if self.narrow {
            self.implied_members(arena, builtins)
        } else {
            Vec::new()
        };
        let mut kept = self.clone();

        for (&generic, candidates) in &mut kept.generics {
            let matched = self.candidates_without(generic, &left_out).unwrap();

            if matched != InferenceCandidates::default() {
                *candidates = matched;
            }
        }

        kept
    }

    /// Drops every candidate of the generics that `keep` doesn't allow.
    pub(crate) fn retain_generics(&mut self, keep: impl Fn(TypeId) -> bool) {
        for (&generic, candidates) in &mut self.generics {
            if !keep(generic) {
                *candidates = InferenceCandidates::default();
                *self.branches.get_mut(&generic).unwrap() = CandidateBranches::default();
            }
        }
    }

    /// Keeps only the bounds that satisfy the predicate. Since the held candidates are only used
    /// if nothing else constrains the generic, they're dropped along with any bound.
    pub fn retain_bounds(&mut self, mut f: impl FnMut(TypeId) -> bool) {
        self.retain_bounds_of(|_, ty| f(ty));
    }

    /// Like `retain_bounds`, with the generic that each bound is a bound of.
    pub fn retain_bounds_of(&mut self, mut f: impl FnMut(TypeId, TypeId) -> bool) {
        for (generic, candidates) in &mut self.generics {
            let branches = self.branches.get_mut(generic).unwrap();
            let len = candidates.lower.len() + candidates.upper.len();

            for kind in [CandidateKind::Lower, CandidateKind::Upper] {
                let types = candidates.list_mut(kind);
                let lists = branches.list_mut(kind);
                (*types, *lists) = mem::take(types)
                    .into_iter()
                    .zip(mem::take(lists))
                    .filter(|&(ty, _)| f(*generic, ty))
                    .unzip();
            }

            if candidates.lower.len() + candidates.upper.len() < len {
                candidates.held.clear();
                branches.held.clear();
            }
        }
    }

    /// Adds another inference's candidates, for the same generics, after this one's, along with the
    /// union members they were collected under. A call's arguments each collect their candidates
    /// separately, so that they can be put in the order the arguments come in, whatever order
    /// they're compiled in.
    pub fn append(&mut self, other: &Inference) {
        let offset = self.choices.0.len();
        let shift = |branch: &BranchId| BranchId {
            choice: branch.choice + offset,
            member: branch.member,
        };

        self.choices
            .0
            .extend(other.choices.0.iter().map(|choice| Choice {
                parent: choice.parent.as_ref().map(shift),
                ..choice.clone()
            }));

        for (&generic, candidates) in &other.generics {
            let branches = &other.branches[&generic];

            // The vars for the other's instances, and those added to it, are its own
            if other.is_instance(generic) && !self.is_instance(generic) {
                self.add_instance(generic, &InferenceCandidates::default());
            } else if other.added.contains(&generic) && !self.added.contains(&generic) {
                self.generics
                    .insert(generic, InferenceCandidates::default());
                self.branches.insert(generic, CandidateBranches::default());
                self.added.push(generic);
            }

            for kind in CandidateKind::ALL {
                for (&ty, branches) in candidates.list(kind).iter().zip(branches.list(kind)) {
                    self.push(generic, kind, ty, branches.iter().map(shift).collect());
                }
            }
        }
    }

    /// Adds a candidate for a generic being inferred, collected under the given union members, and
    /// returns its index in the list.
    pub(crate) fn push(
        &mut self,
        generic: TypeId,
        kind: CandidateKind,
        ty: TypeId,
        branches: Vec<BranchId>,
    ) -> usize {
        let list = self.candidates_mut(generic).list_mut(kind);
        list.push(ty);
        let index = list.len() - 1;
        self.branches
            .get_mut(&generic)
            .unwrap()
            .list_mut(kind)
            .push(branches);
        index
    }

    /// The union members that the candidate at the index was collected under.
    pub(crate) fn branches(
        &self,
        generic: TypeId,
        kind: CandidateKind,
        index: usize,
    ) -> &[BranchId] {
        &self.branches[&generic].list(kind)[index]
    }

    /// Starts comparing a value with a union's members, under the given member of an enclosing
    /// union if there is one, and returns the index of the new choice.
    pub(crate) fn open_choice(&mut self, parent: Option<BranchId>, members: usize) -> usize {
        self.choices.open(parent, members)
    }

    /// Opens a choice that's already been compared (see `Choices::reopen`).
    pub(crate) fn reopen_choice(
        &mut self,
        parent: Option<BranchId>,
        members: &[Option<Comparison>],
        fallback: Option<Comparison>,
        bounded: &[bool],
    ) -> usize {
        self.choices.reopen(parent, members, fallback, bounded)
    }

    /// Records what comparing the value with a union's member (or its fallback) gave.
    pub(crate) fn record(&mut self, branch: BranchId, result: Comparison) {
        self.choices.record(branch, result);
    }

    /// Records that the union member (or the fallback) collected bounds for a generic being
    /// instantiated (see `Choices::mark_bounded`).
    pub(crate) fn mark_bounded(&mut self, branch: BranchId) {
        self.choices.mark_bounded(branch);
    }

    /// Whether the candidates collected under the union member hold (see `Choices::matched_with`).
    #[cfg(test)]
    pub(crate) fn matched(&self, branch: BranchId) -> bool {
        self.choices.matched_with(&self.choices.results(), branch)
    }

    /// What each choice gave (see `Choices::results`), for `Inference::holds`.
    pub(crate) fn choice_results(&self) -> Vec<Option<Comparison>> {
        self.choices.results()
    }

    /// Whether a candidate collected under the union members is needed, given `choice_results`: each
    /// member matched, and is the only one of its union that did. A candidate of a member that
    /// matched along with another isn't needed, since the value could be held by the other.
    pub(crate) fn required(&self, results: &[Option<Comparison>], branches: &[BranchId]) -> bool {
        branches
            .iter()
            .all(|&branch| self.choices.only_match(results, branch))
    }

    /// Whether a candidate collected under the union members holds, given `choice_results`.
    pub(crate) fn holds(&self, results: &[Option<Comparison>], branches: &[BranchId]) -> bool {
        branches
            .iter()
            .all(|&branch| self.choices.matched_with(results, branch))
    }

    /// Adds a var for the instance of a passed generic function's generic, once the comparison
    /// that instantiated it has finished, with the instance's bounds as its candidates. The
    /// candidates that reference the instance reference the var instead (see
    /// `ComparisonContext::finish`).
    pub(crate) fn add_instance(&mut self, var: TypeId, bounds: &InferenceCandidates) {
        self.generics.insert(var, InferenceCandidates::default());
        self.branches.insert(var, CandidateBranches::default());
        self.instances.push(var);

        for kind in [CandidateKind::Lower, CandidateKind::Upper] {
            for &ty in bounds.list(kind) {
                self.push(var, kind, ty, Vec::new());
            }
        }
    }

    pub(crate) fn is_target(&self, generic: TypeId) -> bool {
        self.generics.contains_key(&generic)
    }

    /// Whether upper bounds are passed on to the generic, and lower bounds expanded through it (see
    /// `pass_upper_bounds`). A renamed generic takes them too (#361): it's a fresh generic, and the
    /// generic it was renamed from is one of its candidates, which keeps a recursive call's types
    /// where they were.
    pub(crate) fn passes_bounds(&self, generic: TypeId) -> bool {
        self.is_target(generic)
    }

    /// The generic's candidates. Any change must keep them in step with the union members they
    /// were collected under, so a candidate is added with `Inference::push`.
    pub(crate) fn candidates_mut(&mut self, generic: TypeId) -> &mut InferenceCandidates {
        self.generics.get_mut(&generic).unwrap()
    }
}

/// The number of candidates of each kind collected for each generic, and the number of choices, so
/// that the candidates collected since can be undone (see `Inference::restore`).
#[derive(Debug, Clone)]
pub(crate) struct InferenceSnapshot {
    candidates: Vec<[usize; 3]>,
    choices: usize,
}

impl Inference {
    /// Records how many candidates and choices there are, for `Inference::restore`.
    pub(crate) fn snapshot(&self) -> InferenceSnapshot {
        InferenceSnapshot {
            candidates: self
                .generics
                .values()
                .map(|candidates| {
                    [
                        candidates.lower.len(),
                        candidates.upper.len(),
                        candidates.held.len(),
                    ]
                })
                .collect(),
            choices: self.choices.len(),
        }
    }

    /// Removes the candidates and choices collected since the snapshot, which was taken while the
    /// same comparison was in progress, so no generics have been added since.
    pub(crate) fn restore(&mut self, snapshot: &InferenceSnapshot) {
        for ((candidates, branches), lengths) in self
            .generics
            .values_mut()
            .zip(self.branches.values_mut())
            .zip(&snapshot.candidates)
        {
            for (kind, &length) in [
                CandidateKind::Lower,
                CandidateKind::Upper,
                CandidateKind::Held,
            ]
            .into_iter()
            .zip(lengths)
            {
                candidates.list_mut(kind).truncate(length);
                branches.list_mut(kind).truncate(length);
            }
        }

        self.choices.truncate(snapshot.choices);
    }
}

impl Choices {
    /// The number of choices opened so far.
    pub(crate) fn len(&self) -> usize {
        self.0.len()
    }

    /// Removes the choices opened since there were this many.
    pub(crate) fn truncate(&mut self, len: usize) {
        self.0.truncate(len);
    }

    /// Whether what's collected under the union member holds as far as the choices opened from
    /// index `from` on decide, once those have been compared: like `Choices::matched_so_far`, but
    /// the choices before `from` (in which the member is nested) are left out.
    pub(crate) fn matched_since(&self, branch: BranchId, from: usize) -> bool {
        if branch.choice < from {
            return true;
        }

        let choice = &self.0[branch.choice];

        let result = if branch.member == FALLBACK {
            choice.fallback
        } else {
            choice.members[branch.member]
        };

        let Some(result) = result else {
            return true;
        };

        let best = best_of(choice);

        result == best
            && !self.is_held(branch, best)
            && choice
                .parent
                .is_none_or(|parent| self.matched_since(parent, from))
    }

    /// Whether the union member, and every member it's nested in from the choice opened at index
    /// `from` on, either gave a result other than `Invalid` or hasn't given one yet. A member that
    /// gave `Invalid` never holds the value, whatever the bounds collected under it are, so they
    /// can't decide anything (see `Choices::attributed`).
    pub(crate) fn can_hold_since(&self, branch: BranchId, from: usize) -> bool {
        if branch.choice < from {
            return true;
        }

        let choice = &self.0[branch.choice];

        let result = if branch.member == FALLBACK {
            choice.fallback
        } else {
            choice.members[branch.member]
        };

        result != Some(Comparison::Invalid)
            && choice
                .parent
                .is_none_or(|parent| self.can_hold_since(parent, from))
    }

    /// Whether the bounds collected under the union member can decide anything, as far as the
    /// choices opened in the range do: no union it's nested in from there on (nor its own) is held
    /// by another member, one that gave `Assign` without collecting bounds. Picking that member
    /// gives `Assign` with nothing to rule it out, so no best pick of members needs what's under
    /// the others (see `Choices::attributed`), and pairs with it can be left out, which only saves
    /// picking. It has to be `Assign`: a member that gave the union's best result, `Cast`, wouldn't
    /// make a best pick when another member gives `Assign`.
    pub(crate) fn needed_in(&self, mut branch: BranchId, range: &std::ops::Range<usize>) -> bool {
        loop {
            if !range.contains(&branch.choice) {
                return true;
            }

            let choice = &self.0[branch.choice];
            let index = branch.member.min(choice.members.len());

            let held = choice
                .members
                .iter()
                .chain([&choice.fallback])
                .zip(&choice.bounded)
                .enumerate()
                .any(|(other, (&result, &bounded))| {
                    other != index && result == Some(Comparison::Assign) && !bounded
                });

            if held {
                return false;
            }

            match choice.parent {
                Some(parent) => branch = parent,
                None => return true,
            }
        }
    }

    /// The result of the unions opened in the range, given the pairs of bounds that failed (see
    /// `FailedPair`): the best, over each way of picking a member (or the fallback) of each union
    /// reached, of the worst of what the picked members gave and what the failed pairs whose
    /// bounds were both collected under picked members (or outside of the unions) gave. So the
    /// members of a union are alternatives, and a pair only rules out picking both of its members
    /// (#360). It's the best over picks of the worst of the results, so it's monotone in the
    /// members' results and the pairs', and it doesn't depend on how the summaries that the
    /// unions came from left out unions that decide nothing (see `Summary::canonicalize`). Only
    /// the unions that a failed pair's bound was collected under, and those they're nested in,
    /// are picked from, since the others' results are already in the comparison's. The picks
    /// tried are returned along with the result, and count as steps of comparing: picking is
    /// exponential in the number of unions, but they're few at a time, and a comparison that
    /// picks too many gives up. The search stops once it has tried more than `limit` picks (the
    /// steps the comparison has left), and then gives `Invalid`, since the comparison gives up.
    pub(crate) fn attributed(
        &self,
        range: std::ops::Range<usize>,
        pairs: &[FailedPair],
        limit: usize,
    ) -> (Comparison, usize) {
        if pairs.is_empty() {
            return (Comparison::Assign, 0);
        }

        let mut touched: Vec<bool> = vec![false; range.len()];

        for &(lhs, rhs, _) in pairs {
            for mut branch in [lhs, rhs].into_iter().flatten() {
                while range.contains(&branch.choice) && !touched[branch.choice - range.start] {
                    touched[branch.choice - range.start] = true;

                    match self.0[branch.choice].parent {
                        Some(parent) => branch = parent,
                        None => break,
                    }
                }
            }
        }

        // The touched unions in order, each after the one it's nested in, with the members (and
        // the fallback) that were compared and what each gave
        let unions: Vec<(usize, Vec<(usize, Comparison)>)> = range
            .clone()
            .filter(|&index| touched[index - range.start])
            .map(|index| {
                let choice = &self.0[index];
                let members = choice
                    .members
                    .iter()
                    .enumerate()
                    .chain([(FALLBACK, &choice.fallback)])
                    .filter_map(|(member, &result)| result.map(|result| (member, result)))
                    .collect();
                (index, members)
            })
            .collect();

        // Each pair is worked out once the last of the unions its members are nested in has been
        // picked from (or passed over, if it isn't reached), since what's picked after that
        // doesn't change whether both of its members are picked. Of the pairs with the same
        // members, only the worst counts. A pair under no union in the range always counts
        let mut position = vec![0; range.len()];

        for (index, &(union, _)) in unions.iter().enumerate() {
            position[union - range.start] = index;
        }

        let mut worst_of: FxHashMap<(Option<BranchId>, Option<BranchId>), Comparison> =
            FxHashMap::default();

        for &(lhs, rhs, pair) in pairs {
            let entry = worst_of.entry((lhs, rhs)).or_insert(pair);
            *entry = (*entry).max(pair);
        }

        let mut decided: Vec<Vec<FailedPair>> = vec![Vec::new(); unions.len()];
        let mut always = Comparison::Assign;
        let mut sorted: Vec<_> = worst_of.into_iter().collect();
        sorted.sort_unstable();

        for ((lhs, rhs), pair) in sorted {
            let last = [lhs, rhs]
                .into_iter()
                .flatten()
                .filter(|branch| range.contains(&branch.choice))
                .map(|branch| position[branch.choice - range.start])
                .max();

            match last {
                Some(last) => decided[last].push((lhs, rhs, pair)),
                None => always = always.max(pair),
            }
        }

        let mut search = Search {
            choices: self,
            range: &range,
            unions: &unions,
            decided: &decided,
            picked: vec![None; range.len()],
            best: None,
            nodes: 0,
            limit,
        };
        search.pick(0, always);

        if search.nodes > limit {
            return (Comparison::Invalid, search.nodes);
        }

        (search.best.unwrap_or(Comparison::Invalid), search.nodes)
    }

    /// Starts comparing a value with a union's members, under the given member of an enclosing
    /// union if there is one, and returns the index of the new choice.
    pub(crate) fn open(&mut self, parent: Option<BranchId>, members: usize) -> usize {
        self.0.push(Choice {
            parent,
            members: vec![None; members],
            fallback: None,
            bounded: vec![false; members + 1],
        });
        self.0.len() - 1
    }

    /// The union member that the choice is nested in, what each of its members and its fallback
    /// gave, and which collected bounds (see `ChoiceRecord` in `summary.rs`).
    #[allow(clippy::type_complexity)]
    pub(crate) fn record_of(
        &self,
        choice: usize,
    ) -> (
        Option<BranchId>,
        Vec<Option<Comparison>>,
        Option<Comparison>,
        Vec<bool>,
    ) {
        let choice = &self.0[choice];
        (
            choice.parent,
            choice.members.clone(),
            choice.fallback,
            choice.bounded.clone(),
        )
    }

    /// Opens a choice that's already been compared, with what each of its members and its fallback
    /// gave and which collected bounds, such as one that a summary recorded, and returns it.
    pub(crate) fn reopen(
        &mut self,
        parent: Option<BranchId>,
        members: &[Option<Comparison>],
        fallback: Option<Comparison>,
        bounded: &[bool],
    ) -> usize {
        self.0.push(Choice {
            parent,
            members: members.to_vec(),
            fallback,
            bounded: bounded.to_vec(),
        });
        self.0.len() - 1
    }

    /// Records what comparing the value with a union's member (or its fallback) gave.
    pub(crate) fn record(&mut self, branch: BranchId, result: Comparison) {
        let choice = &mut self.0[branch.choice];

        if branch.member == FALLBACK {
            choice.fallback = Some(result);
        } else {
            choice.members[branch.member] = Some(result);
        }
    }

    /// Records that the union member (or the fallback) collected bounds for a generic being
    /// instantiated.
    pub(crate) fn mark_bounded(&mut self, branch: BranchId) {
        let choice = &mut self.0[branch.choice];
        let index = branch.member.min(choice.members.len());
        choice.bounded[index] = true;
    }

    /// Whether the candidates collected under the union member hold, given `Choices::results`: the
    /// member gave the choice's result, the best that any member or the fallback gave, and the
    /// member the choice is nested in holds too. (The fallback is only compared when no member is
    /// `Assign`, so it only gives the best result when none is.) When nothing is better than
    /// `Invalid`, every member gave `Invalid`, so they all hold, and what a comparison that fails
    /// reports stays the same.
    pub(crate) fn matched_with(&self, results: &[Option<Comparison>], branch: BranchId) -> bool {
        let choice = &self.0[branch.choice];

        let result = if branch.member == FALLBACK {
            choice.fallback
        } else {
            choice.members[branch.member]
        };

        result.is_some()
            && result == results[branch.choice]
            && !self.is_held(branch, result.unwrap())
    }

    /// Whether the union member matched (see `Choices::matched_with`), and no other member of its
    /// union, or the fallback, gave the same result.
    pub(crate) fn only_match(&self, results: &[Option<Comparison>], branch: BranchId) -> bool {
        let choice = &self.0[branch.choice];
        let index = branch.member.min(choice.members.len());

        self.matched_with(results, branch)
            && choice
                .members
                .iter()
                .chain([&choice.fallback])
                .enumerate()
                .all(|(other, &result)| other == index || result != results[branch.choice])
    }

    /// Whether the union member collected bounds for a generic being instantiated while another
    /// member that gave the same, best, result didn't. That one holds the value whatever the
    /// generic is, so the bounds, and what checking them collects, aren't needed, such as `A ≥ nil`
    /// when comparing `nil` with `A | nil` (#360).
    fn is_held(&self, branch: BranchId, best: Comparison) -> bool {
        let choice = &self.0[branch.choice];
        let index = branch.member.min(choice.members.len());

        choice.bounded[index]
            && choice
                .members
                .iter()
                .chain([&choice.fallback])
                .zip(&choice.bounded)
                .any(|(&other, &bounded)| other == Some(best) && !bounded)
    }

    /// Whether what's collected under the union member holds so far, while comparisons may still be
    /// in progress: the member gave the best result of its choice's members compared so far, and
    /// the member its choice is nested in holds too. A member that's still being compared holds,
    /// since it hasn't given a result yet. This is for the bounds of a generic being instantiated,
    /// which are checked once the function comparison that bound it finishes, which can be in the
    /// middle of comparing an enclosing union's member. Since #362's step 13g, it only decides
    /// pairs with a bound collected under a union outside of that function comparison, and which
    /// candidates checking a pair collects: a failed pair under a union inside of it rules out
    /// picks of the members it was collected under instead (see `Choices::attributed`).
    pub(crate) fn matched_so_far(&self, branch: BranchId) -> bool {
        self.matched_since(branch, 0)
    }

    /// The union member that the member's union is nested in, if any.
    pub(crate) fn parent(&self, branch: BranchId) -> Option<BranchId> {
        self.0[branch.choice].parent
    }

    /// Whether the union member is the other one, or nested in it.
    pub(crate) fn is_under(&self, mut branch: BranchId, member: BranchId) -> bool {
        loop {
            if branch == member {
                return true;
            }

            match self.0[branch.choice].parent {
                Some(parent) => branch = parent,
                None => return false,
            }
        }
    }

    /// Whether the union members are alternatives: they're different members of one union, or
    /// nested in different members of one.
    pub(crate) fn exclusive(&self, lhs: BranchId, rhs: BranchId) -> bool {
        let mut ancestors = Vec::new();
        let mut next = Some(lhs);

        while let Some(branch) = next {
            ancestors.push(branch);
            next = self.0[branch.choice].parent;
        }

        let mut next = Some(rhs);

        while let Some(branch) = next {
            if let Some(other) = ancestors.iter().find(|other| other.choice == branch.choice) {
                return other.member != branch.member;
            }

            next = self.0[branch.choice].parent;
        }

        false
    }

    /// The result of each choice, the best that any member or the fallback gave, or `None` if the
    /// member it's nested in doesn't hold. A choice is opened after the one it's nested in, so they
    /// can be worked out in order.
    pub(crate) fn results(&self) -> Vec<Option<Comparison>> {
        let mut results: Vec<Option<Comparison>> = Vec::with_capacity(self.0.len());

        for choice in &self.0 {
            let parent_holds = choice
                .parent
                .is_none_or(|parent| self.matched_with(&results, parent));

            let best = best_of(choice);

            results.push(parent_holds.then_some(best));
        }

        results
    }
}

/// A search for the pick of a member of each union that gives the best result with the failed
/// pairs of bounds it picks both members of (see `Choices::attributed`).
struct Search<'a> {
    choices: &'a Choices,
    range: &'a std::ops::Range<usize>,
    /// The touched unions, each after the one it's nested in, and their compared members.
    unions: &'a [(usize, Vec<(usize, Comparison)>)],
    /// The failed pairs that are worked out once each union has been picked from, by its
    /// position in `unions`: those whose members are nested in it last.
    decided: &'a [Vec<FailedPair>],
    /// The member picked for each union in the range so far, if it's picked one.
    picked: Vec<Option<usize>>,
    /// The best result of a full pick so far, if one has been made.
    best: Option<Comparison>,
    /// How many picks were tried, which counts towards the comparison's steps.
    nodes: usize,
    /// How many picks it tries before it stops.
    limit: usize,
}

impl Search<'_> {
    /// Whether the member is picked, along with every member it's nested in (in the range).
    fn active(&self, mut branch: BranchId) -> bool {
        loop {
            if !self.range.contains(&branch.choice) {
                return true;
            }

            if self.picked[branch.choice - self.range.start] != Some(branch.member) {
                return false;
            }

            match self.choices.0[branch.choice].parent {
                Some(parent) => branch = parent,
                None => return true,
            }
        }
    }

    /// Whether the union is reached by the members picked so far: it's not nested in a member of
    /// a union in the range, or the one it's nested in is picked.
    fn reached(&self, union: usize) -> bool {
        self.choices.0[union]
            .parent
            .is_none_or(|parent| self.active(parent))
    }

    /// Picks a member of the union at the position and each after it, with `worst` the worst
    /// result of the members picked so far.
    fn pick(&mut self, position: usize, worst: Comparison) {
        self.nodes += 1;

        if self.nodes > self.limit {
            return;
        }

        // Nothing picked from here on can make it better than a pick already made
        if self.best.is_some_and(|best| worst >= best) {
            return;
        }

        let Some((union, members)) = self.unions.get(position) else {
            self.best = Some(self.best.map_or(worst, |best| best.min(worst)));
            return;
        };

        let union = *union;
        let slot = union - self.range.start;

        if !self.reached(union) || members.is_empty() {
            let worst = self.with_decided(position, worst);
            self.pick(position + 1, worst);
            return;
        }

        for &(member, result) in members {
            self.picked[slot] = Some(member);
            let worst = self.with_decided(position, worst.max(result));
            self.pick(position + 1, worst);
        }

        self.picked[slot] = None;
    }

    /// The worst of `worst` and the pairs worked out at the position whose members are both
    /// picked (see `Search::decided`).
    fn with_decided(&self, position: usize, worst: Comparison) -> Comparison {
        self.decided[position]
            .iter()
            .filter(|&&(lhs, rhs, _)| {
                [lhs, rhs]
                    .into_iter()
                    .flatten()
                    .all(|branch| self.active(branch))
            })
            .fold(worst, |worst, &(_, _, pair)| worst.max(pair))
    }
}

/// The best result that any member of the choice, or its fallback, gave so far.
fn best_of(choice: &Choice) -> Comparison {
    choice
        .members
        .iter()
        .chain([&choice.fallback])
        .flatten()
        .copied()
        .min()
        .unwrap_or(Comparison::Invalid)
}

/// Which list of a generic's `InferenceCandidates` a candidate is in.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum CandidateKind {
    Lower,
    Upper,
    Held,
}

impl CandidateKind {
    /// Every kind, in the order they're declared, which is the order they're visited in.
    pub(crate) const ALL: [Self; 3] = [Self::Lower, Self::Upper, Self::Held];
}

impl InferenceCandidates {
    pub(crate) fn list(&self, kind: CandidateKind) -> &Vec<TypeId> {
        match kind {
            CandidateKind::Lower => &self.lower,
            CandidateKind::Upper => &self.upper,
            CandidateKind::Held => &self.held,
        }
    }

    pub(crate) fn list_mut(&mut self, kind: CandidateKind) -> &mut Vec<TypeId> {
        match kind {
            CandidateKind::Lower => &mut self.lower,
            CandidateKind::Upper => &mut self.upper,
            CandidateKind::Held => &mut self.held,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn reopened(choices: &mut Choices, parent: Option<BranchId>, members: &[Comparison]) -> usize {
        let members: Vec<Option<Comparison>> = members.iter().copied().map(Some).collect();
        choices.reopen(parent, &members, None, &vec![true; members.len() + 1])
    }

    /// A failed pair across two unions rules out picking both of its members, so each union picks
    /// a member that holds with the other's pick.
    #[test]
    fn attributed_picks_members_across_unions() {
        use Comparison::{Assign, Cast, Invalid};

        let branch = |choice, member| BranchId { choice, member };
        let mut choices = Choices::default();
        let first = reopened(&mut choices, None, &[Assign, Cast]);
        let second = reopened(&mut choices, None, &[Assign, Assign]);

        // The first union's first member conflicts with both of the second's, so the first union
        // picks its second member, which gives `Cast`
        let pairs = [
            (Some(branch(first, 0)), Some(branch(second, 0)), Invalid),
            (Some(branch(first, 0)), Some(branch(second, 1)), Invalid),
        ];
        assert_eq!(choices.attributed(0..2, &pairs, usize::MAX).0, Cast);

        // With one conflict, the second union picks its other member
        assert_eq!(choices.attributed(0..2, &pairs[..1], usize::MAX).0, Assign);

        // A bound outside of any union charges the member in full
        let pairs = [
            (Some(branch(second, 0)), None, Invalid),
            (None, Some(branch(second, 1)), Invalid),
        ];
        assert_eq!(choices.attributed(0..2, &pairs, usize::MAX).0, Invalid);
    }

    /// A pair whose bounds are under a member and a union nested in it rules out the nested
    /// member, which the union it's in can avoid.
    #[test]
    fn attributed_charges_the_enclosed_member() {
        use Comparison::{Assign, Invalid};

        let branch = |choice, member| BranchId { choice, member };
        let mut choices = Choices::default();
        let outer = reopened(&mut choices, None, &[Assign, Invalid]);
        let inner = reopened(&mut choices, Some(branch(outer, 0)), &[Assign, Assign]);

        for pair in [
            (Some(branch(outer, 0)), Some(branch(inner, 0)), Invalid),
            (Some(branch(inner, 0)), Some(branch(outer, 0)), Invalid),
        ] {
            assert_eq!(choices.attributed(0..2, &[pair], usize::MAX).0, Assign);
        }
    }

    /// A union with a member that gave `Assign` without collecting bounds is held by it, so what's
    /// under its other members decides nothing, while one whose members all collected bounds isn't.
    #[test]
    fn needed_in_held_unions() {
        use Comparison::{Assign, Cast};

        let branch = |choice, member| BranchId { choice, member };
        let mut choices = Choices::default();
        let held = choices.reopen(
            None,
            &[Some(Assign), Some(Assign)],
            None,
            &[true, false, false],
        );
        let bounded = reopened(&mut choices, None, &[Assign, Assign]);
        let cast = choices.reopen(
            None,
            &[Some(Assign), Some(Cast)],
            None,
            &[true, false, false],
        );

        assert!(!choices.needed_in(branch(held, 0), &(0..3)));
        assert!(choices.needed_in(branch(bounded, 0), &(0..3)));
        // Held only by a member that gave `Assign`, so a result getting worse can't make it held
        assert!(choices.needed_in(branch(cast, 0), &(0..3)));
        // Outside of the range, it isn't the function comparison's to decide
        assert!(choices.needed_in(branch(held, 0), &(1..3)));
    }

    /// A union whose member without bounds gave `Cast`, though that's its best result, isn't
    /// held, so a result getting worse can't make it held.
    #[test]
    fn needed_in_not_held_by_a_best_cast() {
        use Comparison::Cast;

        let mut choices = Choices::default();
        let union = choices.reopen(None, &[Some(Cast), Some(Cast)], None, &[true, false, false]);
        assert!(choices.needed_in(
            BranchId {
                choice: union,
                member: 0
            },
            &(0..1)
        ));
    }

    /// Picking a member of each union is exponential in the number of unions, at worst, but a pick
    /// that a failed pair already rules out isn't tried further. Here, 20 unions of three members
    /// each have a failed pair for every two of them that picks the same member, like coloring a
    /// complete graph with three colors, which no pick does, and few picks are tried. With a limit
    /// on them, the search stops.
    #[test]
    fn attributed_picks_within_a_limit() {
        use Comparison::{Assign, Invalid};

        let branch = |choice, member| BranchId { choice, member };
        let mut choices = Choices::default();
        let unions: Vec<usize> = (0..20)
            .map(|_| reopened(&mut choices, None, &[Assign, Assign, Assign]))
            .collect();
        let mut pairs = Vec::new();

        for (position, &lhs) in unions.iter().enumerate() {
            for &rhs in &unions[position + 1..] {
                for member in 0..3 {
                    pairs.push((
                        Some(branch(lhs, member)),
                        Some(branch(rhs, member)),
                        Invalid,
                    ));
                }
            }
        }

        let (result, nodes) = choices.attributed(0..20, &pairs, usize::MAX);
        assert_eq!(result, Invalid);
        assert!(nodes < 1000, "tried {nodes} picks");

        // Past the limit, each union being picked from returns once for each of its members
        let (result, nodes) = choices.attributed(0..20, &pairs, 10);
        assert_eq!(result, Invalid);
        assert!(nodes <= 10 + 20 * 3, "tried {nodes} picks");
    }

    /// Past the limit, picking gives `Invalid`, whatever it found by then, and of two pairs with
    /// the same members, the worse one counts.
    #[test]
    fn attributed_stops_at_the_limit_and_keeps_the_worst_duplicate() {
        use Comparison::{Assign, Cast, Invalid};

        let branch = |choice, member| BranchId { choice, member };
        let mut choices = Choices::default();
        let first = reopened(&mut choices, None, &[Cast, Assign]);
        let second = reopened(&mut choices, None, &[Assign, Assign]);
        let pairs = [(Some(branch(first, 0)), Some(branch(second, 0)), Invalid)];
        assert_eq!(choices.attributed(0..2, &pairs, usize::MAX).0, Assign);

        // The first full pick gives `Cast`, then the limit is passed before `Assign` is found
        assert_eq!(choices.attributed(0..2, &pairs, 4), (Invalid, 5));

        let pairs = [
            (Some(branch(second, 0)), None, Invalid),
            (Some(branch(second, 0)), None, Cast),
            (Some(branch(second, 1)), None, Cast),
            (Some(branch(second, 1)), None, Invalid),
        ];
        assert_eq!(choices.attributed(1..2, &pairs, usize::MAX).0, Invalid);
    }

    /// A union nested in a member that isn't picked isn't picked from, so what it gives doesn't
    /// count.
    #[test]
    fn attributed_skips_unions_under_members_not_picked() {
        use Comparison::{Assign, Invalid};

        let branch = |choice, member| BranchId { choice, member };
        let mut choices = Choices::default();
        let outer = reopened(&mut choices, None, &[Assign, Assign]);
        let inner = reopened(&mut choices, Some(branch(outer, 0)), &[Invalid]);
        let pairs = [(Some(branch(inner, 0)), None, Invalid)];
        assert_eq!(choices.attributed(0..2, &pairs, usize::MAX).0, Assign);
    }

    #[test]
    fn exclusive_members() {
        let mut choices = Choices::default();
        let branch = |choice, member| BranchId { choice, member };

        // `first` and `second` are nested in different members of `outer`, `other` in neither
        let outer = choices.open(None, 2);
        let first = choices.open(Some(branch(outer, 0)), 2);
        let second = choices.open(Some(branch(outer, 1)), 2);
        let other = choices.open(None, 2);

        let exclusive = [
            (branch(outer, 0), branch(outer, 1)),
            (branch(first, 0), branch(second, 0)),
            (branch(second, 0), branch(first, 0)),
            (branch(outer, 1), branch(first, 0)),
            (branch(first, 0), branch(outer, 1)),
            (branch(first, 0), branch(first, 1)),
            (branch(outer, FALLBACK), branch(first, 0)),
        ];

        let not_exclusive = [
            (branch(outer, 0), branch(outer, 0)),
            (branch(outer, 0), branch(first, 1)),
            (branch(first, 1), branch(outer, 0)),
            (branch(first, 0), branch(other, 1)),
            (branch(outer, FALLBACK), branch(outer, FALLBACK)),
        ];

        for (lhs, rhs) in exclusive {
            assert!(choices.exclusive(lhs, rhs), "{lhs:?} and {rhs:?}");
        }

        for (lhs, rhs) in not_exclusive {
            assert!(!choices.exclusive(lhs, rhs), "{lhs:?} and {rhs:?}");
        }
    }
}
