use id_arena::Arena;

use crate::{Type, TypeId, resolve_refs};

/// The values that are in every one of the types at once.
///
/// There's no syntax for writing one. They're only produced by narrowing a generic type, such as
/// `T & Atom` for a value of type `T` that has been checked to be an atom, since the narrowed
/// value must still be a `T` but can't be represented by any other type. `intersect` normalizes
/// them to some generics followed by at most one other type, which is the structure that the
/// value is known to have.
#[derive(Debug, Clone)]
pub struct Intersection {
    pub types: Vec<TypeId>,
}

impl Intersection {
    pub fn new(types: Vec<TypeId>) -> Self {
        Self { types }
    }

    /// The first member that isn't a generic, if there is one. Every value of the intersection
    /// is a value of it, so it can be used wherever the structure of the value is needed (such
    /// as to access its fields or to check it against another type at runtime).
    pub fn structure(&self, arena: &Arena<Type>) -> Option<TypeId> {
        self.types
            .iter()
            .copied()
            .find(|&id| !is_generic(arena, id))
    }

    /// The first member that is a generic, if there is one.
    pub(crate) fn generic(&self, arena: &Arena<Type>) -> Option<TypeId> {
        self.types.iter().copied().find(|&id| is_generic(arena, id))
    }
}

/// Whether the type is a generic, after following references.
pub(crate) fn is_generic(arena: &Arena<Type>, id: TypeId) -> bool {
    matches!(arena[resolve_refs(arena, id)], Type::Generic(_))
}

/// The members of an intersection, including those of nested intersections, or the type itself.
pub(crate) fn intersection_members(arena: &Arena<Type>, id: TypeId) -> Vec<TypeId> {
    match &arena[resolve_refs(arena, id)] {
        Type::Intersection(intersection) => intersection
            .types
            .iter()
            .flat_map(|&id| intersection_members(arena, id))
            .collect(),
        _ => vec![id],
    }
}
