use id_arena::Arena;

use crate::{Atom, Type, TypeId, Union};

#[derive(Debug, Clone, Copy)]
pub struct BuiltinTypes {
    pub unresolved: TypeId,
    pub atom: TypeId,
    pub bytes: TypeId,
    pub bytes32: TypeId,
    pub public_key: TypeId,
    pub int: TypeId,
    pub bool: TypeId,
    pub bool_true: TypeId,
    pub bool_false: TypeId,
    pub nil: TypeId,
}

impl BuiltinTypes {
    pub fn new(arena: &mut Arena<Type>) -> Self {
        let bool_true = arena.alloc(Type::Atom(Atom::TRUE));
        let bool_false = arena.alloc(Type::Atom(Atom::FALSE));

        Self {
            unresolved: arena.alloc(Type::Unresolved),
            atom: arena.alloc(Type::Atom(Atom::ANY)),
            bytes: arena.alloc(Type::Atom(Atom::BYTES)),
            bytes32: arena.alloc(Type::Atom(Atom::BYTES_32)),
            public_key: arena.alloc(Type::Atom(Atom::PUBLIC_KEY)),
            int: arena.alloc(Type::Atom(Atom::INT)),
            bool: arena.alloc(Type::Union(Union::new(vec![bool_true, bool_false]))),
            bool_true,
            bool_false,
            nil: arena.alloc(Type::Atom(Atom::NIL)),
        }
    }
}
