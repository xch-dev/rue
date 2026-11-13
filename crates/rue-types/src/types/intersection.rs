use id_arena::Arena;

use crate::{Type, TypeId};

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
    pub fn generic(&self, arena: &Arena<Type>) -> Option<TypeId> {
        self.types.iter().copied().find(|&id| is_generic(arena, id))
    }
}

fn is_generic(arena: &Arena<Type>, mut id: TypeId) -> bool {
    while let Type::Ref(inner) = arena[id] {
        id = inner;
    }

    matches!(arena[id], Type::Generic(_))
}
