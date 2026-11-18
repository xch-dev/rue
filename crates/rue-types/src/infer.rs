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
#[derive(Debug, Default, Clone)]
pub(crate) struct Choices(Vec<Choice>);

/// A comparison of a value with the members of a union, any one of which can hold it.
#[derive(Debug, Clone)]
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
        }
    }

    /// Records that a generic being inferred was renamed from another generic. They're compared as
    /// the same type where neither is bound, and bounds aren't passed through it (see
    /// `Inference::passes_bounds`), since they were the same generic before. But a candidate that
    /// references this one is resolved through what it's inferred as, unlike one that references
    /// the other.
    pub fn rename(&mut self, generic: TypeId, from: TypeId) {
        self.renamed.insert(generic, from);
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

            for kind in [
                CandidateKind::Lower,
                CandidateKind::Upper,
                CandidateKind::Held,
            ] {
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

    /// Whether the candidates collected under the union member hold (see `Choices::matched_with`).
    #[cfg(test)]
    pub(crate) fn matched(&self, branch: BranchId) -> bool {
        self.choices.matched_with(&self.choices.results(), branch)
    }

    pub(crate) fn is_target(&self, generic: TypeId) -> bool {
        self.generics.contains_key(&generic)
    }

    /// Whether upper bounds are passed on to the generic, and lower bounds expanded through it (see
    /// `pass_upper_bounds`). A renamed generic is left out, as it was when it was the generic it
    /// was renamed from, so that a recursive call infers the same types as it did then.
    pub(crate) fn passes_bounds(&self, generic: TypeId) -> bool {
        self.is_target(generic) && !self.renamed.contains_key(&generic)
    }

    /// The generic's candidates. Any change must keep them in step with the union members they
    /// were collected under, so a candidate is added with `Inference::push`.
    pub(crate) fn candidates_mut(&mut self, generic: TypeId) -> &mut InferenceCandidates {
        self.generics.get_mut(&generic).unwrap()
    }
}

impl Choices {
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

        result.is_some() && result == results[branch.choice]
    }

    /// Whether what's collected under the union member holds so far, while comparisons may still be
    /// in progress: the member gave the best result of its choice's members compared so far, and
    /// the member its choice is nested in holds too. A member that's still being compared holds,
    /// since it hasn't given a result yet. This is for the bounds of a generic being instantiated,
    /// which are checked once the function comparison that bound it finishes, which can be in the
    /// middle of comparing an enclosing union's member.
    pub(crate) fn matched_so_far(&self, branch: BranchId) -> bool {
        let choice = &self.0[branch.choice];

        let result = if branch.member == FALLBACK {
            choice.fallback
        } else {
            choice.members[branch.member]
        };

        let Some(result) = result else {
            return true;
        };

        let best = choice
            .members
            .iter()
            .chain([&choice.fallback])
            .flatten()
            .copied()
            .min()
            .unwrap_or(Comparison::Invalid);

        // A member that gives the best result without bounding the generics being instantiated
        // holds the value whatever they are, so the bounds of another member are left out, such as
        // `A ≥ nil` when comparing `nil` with `A | nil`
        let index = branch.member.min(choice.members.len());
        let held = choice.bounded[index]
            && choice
                .members
                .iter()
                .chain([&choice.fallback])
                .zip(&choice.bounded)
                .any(|(&other, &bounded)| other == Some(best) && !bounded);

        result == best
            && !held
            && choice
                .parent
                .is_none_or(|parent| self.matched_so_far(parent))
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

            let best = choice
                .members
                .iter()
                .chain([&choice.fallback])
                .flatten()
                .copied()
                .min()
                .unwrap_or(Comparison::Invalid);

            results.push(parent_holds.then_some(best));
        }

        results
    }
}

/// Which list of a generic's `InferenceCandidates` a candidate is in.
#[derive(Debug, Clone, Copy)]
pub(crate) enum CandidateKind {
    Lower,
    Upper,
    Held,
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
