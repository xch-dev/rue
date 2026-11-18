//! Generic inference: the candidates collected for the generics being inferred while comparing
//! (`compare_with_inference` and `infer_from_expected` in `compare.rs`), and how a type is picked for
//! each of them from those candidates.

mod bounds;
mod resolve;

pub use bounds::*;
pub use resolve::*;
pub(crate) use resolve::{pick_inferred_type, union_of};

use indexmap::IndexMap;

use crate::{FxHashMap, TypeId};

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
}

impl Inference {
    pub fn new(generics: impl IntoIterator<Item = TypeId>) -> Self {
        Self {
            generics: generics
                .into_iter()
                .map(|generic| (generic, InferenceCandidates::default()))
                .collect(),
            renamed: FxHashMap::default(),
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

    pub fn candidates(&self, generic: TypeId) -> Option<&InferenceCandidates> {
        self.generics.get(&generic)
    }

    /// Keeps only the bounds that satisfy the predicate. Since the held candidates are only used
    /// if nothing else constrains the generic, they're dropped along with any bound.
    pub fn retain_bounds(&mut self, mut f: impl FnMut(TypeId) -> bool) {
        for candidates in self.generics.values_mut() {
            let len = candidates.lower.len() + candidates.upper.len();

            candidates.lower.retain(|&ty| f(ty));
            candidates.upper.retain(|&ty| f(ty));

            if candidates.lower.len() + candidates.upper.len() < len {
                candidates.held.clear();
            }
        }
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

    pub(crate) fn candidates_mut(&mut self, generic: TypeId) -> &mut InferenceCandidates {
        self.generics.get_mut(&generic).unwrap()
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
    pub(crate) fn list_mut(&mut self, kind: CandidateKind) -> &mut Vec<TypeId> {
        match kind {
            CandidateKind::Lower => &mut self.lower,
            CandidateKind::Upper => &mut self.upper,
            CandidateKind::Held => &mut self.held,
        }
    }
}
