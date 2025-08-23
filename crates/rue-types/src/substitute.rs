use std::collections::HashMap;

use id_arena::Arena;

use crate::{Alias, Pair, Struct, Type, TypeId, Union};

pub fn substitute(arena: &mut Arena<Type>, id: TypeId, map: &HashMap<TypeId, TypeId>) -> TypeId {
    if let Some(id) = map.get(&id) {
        return *id;
    }

    match arena[id].clone() {
        Type::Unresolved | Type::Generic => id,
        Type::Apply(apply) => substitute(arena, apply.inner, &apply.generics),
        Type::Atom(_) => id,
        Type::Pair(pair) => {
            let first = substitute(arena, pair.first, map);
            let rest = substitute(arena, pair.rest, map);
            arena.alloc(Type::Pair(Pair::new(first, rest)))
        }
        Type::Struct(ty) => {
            let inner = substitute(arena, ty.inner, map);
            arena.alloc(Type::Struct(Struct { inner, ..ty }))
        }
        Type::Alias(alias) => {
            let inner = substitute(arena, alias.inner, map);
            arena.alloc(Type::Alias(Alias { inner }))
        }
        Type::Union(union) => {
            let types = union
                .types
                .iter()
                .map(|id| substitute(arena, *id, map))
                .collect();
            arena.alloc(Type::Union(Union::new(types)))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{Atom, Comparison, compare};

    use super::*;

    #[test]
    fn test_substitute_atom() {
        let mut arena = Arena::new();
        let int = arena.alloc(Type::Atom(Atom::INT));
        let result = substitute(&mut arena, int, &HashMap::new());
        assert_eq!(result, int);
    }

    #[test]
    fn test_substitute_generic() {
        let mut arena = Arena::new();
        let generic = arena.alloc(Type::Generic);
        let int = arena.alloc(Type::Atom(Atom::INT));
        assert_eq!(compare(&arena, generic, int), Comparison::Invalid);
        let result = substitute(&mut arena, generic, &HashMap::from_iter([(generic, int)]));
        assert_eq!(compare(&arena, result, int), Comparison::Assign);
    }

    #[test]
    fn test_substitute_pair() {
        let mut arena = Arena::new();
        let generic = arena.alloc(Type::Generic);
        let int = arena.alloc(Type::Atom(Atom::INT));
        let generic_pair = arena.alloc(Type::Pair(Pair::new(generic, int)));
        let int_pair = arena.alloc(Type::Pair(Pair::new(int, int)));
        assert_eq!(compare(&arena, generic_pair, int_pair), Comparison::Invalid);
        let result = substitute(
            &mut arena,
            generic_pair,
            &HashMap::from_iter([(generic, int)]),
        );
        assert_eq!(compare(&arena, result, int_pair), Comparison::Assign);
    }

    #[test]
    fn test_substitute_list() {
        let mut arena = Arena::new();

        let generic = arena.alloc(Type::Generic);
        let list = arena.alloc(Type::Unresolved);
        let nil = arena.alloc(Type::Atom(Atom::NIL));
        let pair = arena.alloc(Type::Pair(Pair::new(generic, list)));
        *arena.get_mut(list).unwrap() = Type::Union(Union::new(vec![nil, pair]));

        assert_eq!(compare(&arena, list, list), Comparison::Invalid);
    }
}
