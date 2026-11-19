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

use indexmap::IndexMap;

use crate::{Comparison, FxHashMap, TypeId};

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
    /// union member that didn't match the value (see `Inference::matched`). For example, passing a
    /// function to `T | (T, Int) | nil` matches the `T` member, so what comparing the function's
    /// runtime structure with `(T, Int)` collected for `T` is left out (#364).
    pub fn matched_candidates(&self, generic: TypeId) -> Option<InferenceCandidates> {
        let candidates = self.generics.get(&generic)?;
        let branches = &self.branches[&generic];
        let results = self.choices.results();

        let matched = |kind: CandidateKind, types: &[TypeId]| -> Vec<TypeId> {
            types
                .iter()
                .zip(branches.list(kind))
                .filter(|(_, branches)| {
                    branches
                        .iter()
                        .all(|&branch| self.choices.matched_with(&results, branch))
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

    /// The inference with each generic's candidates replaced by those that hold
    /// (`Inference::matched_candidates`), which is what the generics are resolved from. A generic
    /// whose candidates all come from union members that didn't match keeps every one of them, as
    /// in `pick(ident)` with `fn pick<U>(f: fn(x: Int) -> (Int, Int) | (U, Bytes))`, where `U`'s
    /// only candidate comes from the `(U, Bytes)` member.
    pub(crate) fn keep_matched(&self) -> Inference {
        let mut kept = self.clone();

        for (&generic, candidates) in &mut kept.generics {
            let matched = self.matched_candidates(generic).unwrap();

            if matched != InferenceCandidates::default() {
                *candidates = matched;
            }
        }

        kept
    }

    /// Keeps only the bounds that satisfy the predicate. Since the held candidates are only used
    /// if nothing else constrains the generic, they're dropped along with any bound.
    pub fn retain_bounds(&mut self, mut f: impl FnMut(TypeId) -> bool) {
        for (generic, candidates) in &mut self.generics {
            let branches = self.branches.get_mut(generic).unwrap();
            let len = candidates.lower.len() + candidates.upper.len();

            for kind in [CandidateKind::Lower, CandidateKind::Upper] {
                let types = candidates.list_mut(kind);
                let lists = branches.list_mut(kind);
                (*types, *lists) = mem::take(types)
                    .into_iter()
                    .zip(mem::take(lists))
                    .filter(|&(ty, _)| f(ty))
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
    /// middle of comparing an enclosing union's member.
    pub(crate) fn matched_so_far(&self, branch: BranchId) -> bool {
        self.matched_since(branch, 0)
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
