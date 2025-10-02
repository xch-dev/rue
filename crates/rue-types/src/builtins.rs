use id_arena::Arena;

use crate::{Alias, Atom, Generic, Pair, Type, TypeId, Union};

#[derive(Debug, Clone, Copy)]
pub struct BuiltinTypes {
    pub unresolved: TypeId,
    pub atom: TypeId,
    pub bytes: TypeId,
    pub bytes32: TypeId,
    pub public_key: TypeId,
    pub signature: TypeId,
    pub k1_public_key: TypeId,
    pub k1_signature: TypeId,
    pub r1_public_key: TypeId,
    pub r1_signature: TypeId,
    pub int: TypeId,
    pub bool_true: TypeId,
    pub bool_false: TypeId,
    pub bool: TypeId,
    pub nil: TypeId,
    pub never: TypeId,
    pub recursive_any: TypeId,
    pub recursive_any_pair: TypeId,
    pub permissive_any: TypeId,
    pub list: TypeId,
    pub list_generic: TypeId,
    pub alternating_list: TypeId,
    pub alternating_list_generic_a: TypeId,
    pub alternating_list_generic_b: TypeId,
}

impl BuiltinTypes {
    pub fn new(arena: &mut Arena<Type>) -> Self {
        let unresolved = arena.alloc(Type::Unresolved);
        let atom = arena.alloc(Type::Atom(Atom::ANY));
        let bytes = arena.alloc(Type::Atom(Atom::BYTES));
        let bytes32 = arena.alloc(Type::Atom(Atom::BYTES_32));
        let public_key = arena.alloc(Type::Atom(Atom::PUBLIC_KEY));
        let signature = arena.alloc(Type::Atom(Atom::SIGNATURE));
        let k1_public_key = arena.alloc(Type::Atom(Atom::K1_PUBLIC_KEY));
        let k1_signature = arena.alloc(Type::Atom(Atom::K1_SIGNATURE));
        let r1_public_key = arena.alloc(Type::Atom(Atom::R1_PUBLIC_KEY));
        let r1_signature = arena.alloc(Type::Atom(Atom::R1_SIGNATURE));
        let int = arena.alloc(Type::Atom(Atom::INT));
        let bool_true = arena.alloc(Type::Atom(Atom::TRUE));
        let bool_false = arena.alloc(Type::Atom(Atom::FALSE));
        let bool = arena.alloc(Type::Union(Union::new(vec![bool_true, bool_false])));
        let never = arena.alloc(Type::Never);
        let nil = arena.alloc(Type::Atom(Atom::NIL));

        let recursive_any = arena.alloc(Type::Unresolved);
        let recursive_any_pair = arena.alloc(Type::Pair(Pair::new(recursive_any, recursive_any)));
        *arena.get_mut(recursive_any).unwrap() =
            Type::Union(Union::new(vec![atom, recursive_any_pair]));
        let permissive_any = arena.alloc(Type::Any);

        let list = arena.alloc(Type::Unresolved);
        let list_generic = arena.alloc(Type::Generic(Generic { name: None }));
        let list_pair = arena.alloc(Type::Pair(Pair::new(list_generic, list)));
        let list_union = arena.alloc(Type::Union(Union::new(vec![nil, list_pair])));
        *arena.get_mut(list).unwrap() = Type::Alias(Alias {
            name: None,
            inner: list_union,
            generics: vec![list_generic],
        });

        let alternating_list = arena.alloc(Type::Unresolved);
        let alternating_list_generic_a = arena.alloc(Type::Generic(Generic { name: None }));
        let alternating_list_generic_b = arena.alloc(Type::Generic(Generic { name: None }));
        let alternating_list_pair = arena.alloc(Type::Pair(Pair::new(
            alternating_list_generic_b,
            alternating_list,
        )));
        let alternating_list_pair = arena.alloc(Type::Pair(Pair::new(
            alternating_list_generic_a,
            alternating_list_pair,
        )));
        let alternating_list_union =
            arena.alloc(Type::Union(Union::new(vec![nil, alternating_list_pair])));
        *arena.get_mut(alternating_list).unwrap() = Type::Alias(Alias {
            name: None,
            inner: alternating_list_union,
            generics: vec![alternating_list_generic_a, alternating_list_generic_b],
        });

        Self {
            unresolved,
            atom,
            bytes,
            bytes32,
            public_key,
            signature,
            k1_public_key,
            k1_signature,
            r1_public_key,
            r1_signature,
            int,
            bool_true,
            bool_false,
            bool,
            nil,
            never,
            recursive_any,
            recursive_any_pair,
            permissive_any,
            list,
            list_generic,
            alternating_list,
            alternating_list_generic_a,
            alternating_list_generic_b,
        }
    }
}
