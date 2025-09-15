use id_arena::Arena;

use crate::{Alias, Atom, Pair, Type, TypeId, Union};

#[derive(Debug, Clone, Copy)]
pub struct BuiltinTypes {
    pub unresolved: TypeId,
    pub atom: TypeId,
    pub bytes: TypeId,
    pub bytes32: TypeId,
    pub public_key: TypeId,
    pub int: TypeId,
    pub bool_true: TypeId,
    pub bool_false: TypeId,
    pub bool: TypeId,
    pub nil: TypeId,
    pub any: TypeId,
    pub any_pair: TypeId,
    pub list: TypeId,
    pub list_pair: TypeId,
    pub list_generic: TypeId,
}

impl BuiltinTypes {
    pub fn new(arena: &mut Arena<Type>) -> Self {
        let unresolved = arena.alloc(Type::Unresolved);
        let atom = arena.alloc(Type::Atom(Atom::ANY));
        let bytes = arena.alloc(Type::Atom(Atom::BYTES));
        let bytes32 = arena.alloc(Type::Atom(Atom::BYTES_32));
        let public_key = arena.alloc(Type::Atom(Atom::PUBLIC_KEY));
        let int = arena.alloc(Type::Atom(Atom::INT));
        let bool_true = arena.alloc(Type::Atom(Atom::TRUE));
        let bool_false = arena.alloc(Type::Atom(Atom::FALSE));
        let bool = arena.alloc(Type::Union(Union::new(vec![bool_true, bool_false])));
        let nil = arena.alloc(Type::Atom(Atom::NIL));

        let any = arena.alloc(Type::Unresolved);
        let any_pair = arena.alloc(Type::Pair(Pair::new(any, any)));
        *arena.get_mut(any).unwrap() = Type::Union(Union::new(vec![atom, any_pair]));

        let list = arena.alloc(Type::Unresolved);
        let list_generic = arena.alloc(Type::Generic);
        let list_pair = arena.alloc(Type::Pair(Pair::new(list_generic, list)));
        let list_union = arena.alloc(Type::Union(Union::new(vec![nil, list_pair])));
        *arena.get_mut(list).unwrap() = Type::Alias(Alias {
            name: None,
            inner: list_union,
            generics: vec![list_generic],
        });

        Self {
            unresolved,
            atom,
            bytes,
            bytes32,
            public_key,
            int,
            bool,
            bool_true,
            bool_false,
            nil,
            any,
            any_pair,
            list,
            list_pair,
            list_generic,
        }
    }
}
