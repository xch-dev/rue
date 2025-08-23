use std::collections::HashMap;

use id_arena::Arena;

use crate::{Alias, Pair, Struct, Type, TypeId, Union};

pub fn substitute(arena: &mut Arena<Type>, id: TypeId) -> TypeId {
    substitute_impl(arena, id, &HashMap::new(), &mut HashMap::new())
}

fn substitute_impl(
    arena: &mut Arena<Type>,
    old_id: TypeId,
    map: &HashMap<TypeId, TypeId>,
    visited: &mut HashMap<TypeId, TypeId>,
) -> TypeId {
    if let Some(id) = map.get(&old_id) {
        return *id;
    }

    if let Some(id) = visited.get(&old_id) {
        return *id;
    }

    let new_id = if !matches!(
        arena[old_id],
        Type::Unresolved | Type::Generic | Type::Atom(_)
    ) {
        let new_id = arena.alloc(Type::Unresolved);
        visited.insert(old_id, new_id);
        new_id
    } else {
        old_id
    };

    let result = match arena[old_id].clone() {
        Type::Unresolved | Type::Generic | Type::Atom(_) | Type::Function(_) => return old_id,
        Type::Ref(id) => substitute_impl(arena, id, map, visited),
        Type::Apply(apply) => substitute_impl(arena, apply.inner, &apply.generics, visited),
        Type::Pair(pair) => {
            let first = substitute_impl(arena, pair.first, map, visited);
            let rest = substitute_impl(arena, pair.rest, map, visited);
            arena.alloc(Type::Pair(Pair::new(first, rest)))
        }
        Type::Struct(ty) => {
            let inner = substitute_impl(arena, ty.inner, map, visited);
            arena.alloc(Type::Struct(Struct { inner, ..ty }))
        }
        Type::Alias(alias) => {
            let inner = substitute_impl(arena, alias.inner, map, visited);
            arena.alloc(Type::Alias(Alias { inner, ..alias }))
        }
        Type::Union(union) => {
            let types = union
                .types
                .iter()
                .map(|id| substitute_impl(arena, *id, map, visited))
                .collect();
            arena.alloc(Type::Union(Union::new(types)))
        }
    };

    *arena.get_mut(new_id).unwrap() = Type::Ref(result);

    new_id
}

#[cfg(test)]
mod tests {
    use crate::{Apply, Atom, Comparison, compare};

    use super::*;

    #[test]
    fn test_substitute_atom() {
        let mut arena = Arena::new();
        let int = arena.alloc(Type::Atom(Atom::INT));
        let result = substitute(&mut arena, int);
        assert_eq!(compare(&arena, result, int), Comparison::Assign);
    }

    #[test]
    fn test_substitute_generic() {
        let mut arena = Arena::new();
        let generic = arena.alloc(Type::Generic);
        let int = arena.alloc(Type::Atom(Atom::INT));
        assert_eq!(compare(&arena, generic, int), Comparison::Invalid);
        let apply = arena.alloc(Type::Apply(Apply::new(
            generic,
            HashMap::from_iter([(generic, int)]),
        )));
        let result = substitute(&mut arena, apply);
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
        let apply = arena.alloc(Type::Apply(Apply::new(
            generic_pair,
            HashMap::from_iter([(generic, int)]),
        )));
        let result = substitute(&mut arena, apply);
        assert_eq!(compare(&arena, result, int_pair), Comparison::Assign);
    }

    #[test]
    fn test_substitute_list() {
        let mut arena = Arena::new();

        let generic = arena.alloc(Type::Generic);
        let list = arena.alloc(Type::Unresolved);
        let nil = arena.alloc(Type::Atom(Atom::NIL));
        let inner_apply = arena.alloc(Type::Apply(Apply::new(
            list,
            HashMap::from_iter([(generic, generic)]),
        )));
        let pair = arena.alloc(Type::Pair(Pair::new(generic, inner_apply)));
        *arena.get_mut(list).unwrap() = Type::Union(Union::new(vec![nil, pair]));

        let int = arena.alloc(Type::Atom(Atom::INT));
        let bytes = arena.alloc(Type::Atom(Atom::BYTES));
        let nil = arena.alloc(Type::Atom(Atom::NIL));

        let int_list = arena.alloc(Type::Apply(Apply::new(
            list,
            HashMap::from_iter([(generic, int)]),
        )));
        let bytes_list = arena.alloc(Type::Apply(Apply::new(
            list,
            HashMap::from_iter([(generic, bytes)]),
        )));
        let nil_list = arena.alloc(Type::Apply(Apply::new(
            list,
            HashMap::from_iter([(generic, nil)]),
        )));

        let int_list = substitute(&mut arena, int_list);
        let bytes_list = substitute(&mut arena, bytes_list);
        let nil_list = substitute(&mut arena, nil_list);

        assert_eq!(compare(&arena, int_list, int_list), Comparison::Assign);
        assert_eq!(compare(&arena, int_list, bytes_list), Comparison::Cast);
        assert_eq!(compare(&arena, bytes_list, int_list), Comparison::Cast);
        assert_eq!(compare(&arena, nil_list, int_list), Comparison::Cast);
        assert_eq!(compare(&arena, int_list, nil_list), Comparison::Invalid);
    }
}
