//! Generic inference: the candidates collected for the generics being inferred while comparing
//! (`compare_with_inference` and `infer_from_expected` in `compare.rs`), and how a type is picked for
//! each of them from those candidates.

mod bounds;
mod resolve;

pub use bounds::*;
pub use resolve::*;

use std::cmp::min;

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
    choices: Vec<Choice>,
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

/// A comparison of a value with the members of a union, any one of which can hold it.
#[derive(Debug, Clone)]
struct Choice {
    /// The member that the union itself was compared under, if any.
    parent: Option<BranchId>,
    /// What comparing the value with each member gave, once it has been compared.
    members: Vec<Option<Comparison>>,
    /// What the fallback gave, if it was compared.
    fallback: Option<Comparison>,
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
            choices: Vec::new(),
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

        let matched = |kind: CandidateKind, types: &[TypeId]| -> Vec<TypeId> {
            types
                .iter()
                .zip(branches.list(kind))
                .filter(|(_, branches)| branches.iter().all(|&branch| self.matched(branch)))
                .map(|(&ty, _)| ty)
                .collect()
        };

        Some(InferenceCandidates {
            lower: matched(CandidateKind::Lower, &candidates.lower),
            upper: matched(CandidateKind::Upper, &candidates.upper),
            held: matched(CandidateKind::Held, &candidates.held),
        })
    }

    /// Keeps only the bounds that satisfy the predicate. Since the held candidates are only used
    /// if nothing else constrains the generic, they're dropped along with any bound.
    pub fn retain_bounds(&mut self, mut f: impl FnMut(TypeId) -> bool) {
        for (generic, candidates) in &mut self.generics {
            let branches = self.branches.get_mut(generic).unwrap();
            let len = candidates.lower.len() + candidates.upper.len();

            for kind in [CandidateKind::Lower, CandidateKind::Upper] {
                let keep: Vec<bool> = candidates.list_mut(kind).iter().map(|&ty| f(ty)).collect();
                let mut keep_types = keep.iter();
                candidates
                    .list_mut(kind)
                    .retain(|_| *keep_types.next().unwrap());
                let mut keep_branches = keep.iter();
                branches
                    .list_mut(kind)
                    .retain(|_| *keep_branches.next().unwrap());
            }

            if candidates.lower.len() + candidates.upper.len() < len {
                candidates.held.clear();
                branches.held.clear();
            }
        }
    }

    /// Adds a candidate for a generic being inferred, collected under the given union members.
    pub(crate) fn push(
        &mut self,
        generic: TypeId,
        kind: CandidateKind,
        ty: TypeId,
        branches: Vec<BranchId>,
    ) {
        self.candidates_mut(generic).list_mut(kind).push(ty);
        self.branches
            .get_mut(&generic)
            .unwrap()
            .list_mut(kind)
            .push(branches);
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
        self.choices.push(Choice {
            parent,
            members: vec![None; members],
            fallback: None,
        });
        self.choices.len() - 1
    }

    /// Records what comparing the value with a union's member (or its fallback) gave.
    pub(crate) fn record(&mut self, branch: BranchId, result: Comparison) {
        let choice = &mut self.choices[branch.choice];

        if branch.member == FALLBACK {
            choice.fallback = Some(result);
        } else {
            choice.members[branch.member] = Some(result);
        }
    }

    /// Whether the candidates collected under the union member hold: the member gave the best
    /// result of any member (or the fallback, which only counts when no member is assignable),
    /// and the member it's nested in holds too. When nothing is better than `Invalid`, the
    /// comparison fails regardless, so every member holds, to keep what it reports the same.
    pub(crate) fn matched(&self, branch: BranchId) -> bool {
        let choice = &self.choices[branch.choice];

        if choice.parent.is_some_and(|parent| !self.matched(parent)) {
            return false;
        }

        let best = choice
            .members
            .iter()
            .flatten()
            .copied()
            .min()
            .unwrap_or(Comparison::Invalid);

        let result = if best == Comparison::Assign {
            best
        } else {
            min(best, choice.fallback.unwrap_or(Comparison::Invalid))
        };

        if result == Comparison::Invalid {
            return true;
        }

        let branch_result = if branch.member == FALLBACK {
            choice.fallback
        } else {
            choice.members[branch.member]
        };

        branch_result == Some(result)
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

    /// The generic's candidates, to replace one in place. A candidate is added with
    /// `Inference::push`, so that the union members it was collected under are recorded too.
    pub(crate) fn candidates_mut(&mut self, generic: TypeId) -> &mut InferenceCandidates {
        self.generics.get_mut(&generic).unwrap()
    }
}

/// Which list of a generic's `InferenceCandidates` a candidate is in.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CandidateKind {
    Lower,
    Upper,
    Held,
}

impl InferenceCandidates {
    pub(crate) fn list_mut(&mut self, kind: CandidateKind) -> &mut Vec<TypeId> {
        match kind {
            CandidateKind::Lower => &mut self.lower,
            CandidateKind::Upper => &mut self.upper,
            CandidateKind::Held => &mut self.held,
        }
    }
}
