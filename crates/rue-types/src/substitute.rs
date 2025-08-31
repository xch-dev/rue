use std::collections::{HashMap, HashSet};

use id_arena::Arena;

use crate::{Alias, FunctionType, Pair, Struct, Type, TypeId, Union};

pub fn substitute(arena: &mut Arena<Type>, id: TypeId) -> TypeId {
    substitute_impl(arena, id, &HashMap::new())
}

pub fn substitute_with_mappings(
    arena: &mut Arena<Type>,
    id: TypeId,
    mappings: &HashMap<TypeId, TypeId>,
) -> TypeId {
    substitute_impl(arena, id, mappings)
}

fn substitute_impl(
    arena: &mut Arena<Type>,
    mut old_id: TypeId,
    map: &HashMap<TypeId, TypeId>,
) -> TypeId {
    let mut seen_from_map = HashSet::new();

    while let Some(id) = map.get(&old_id) {
        if !seen_from_map.insert(*id) {
            return *id;
        }
        old_id = *id;
    }

    match arena[old_id].clone() {
        Type::Apply(apply) => substitute_impl(arena, apply.inner, &apply.generics),
        Type::Unresolved | Type::Generic | Type::Atom(_) | Type::Never | Type::Any => old_id,
        Type::List(id) => {
            let inner = substitute_impl(arena, id, map);
            arena.alloc(Type::List(inner))
        }
        Type::Pair(pair) => {
            let first = substitute_impl(arena, pair.first, map);
            let rest = substitute_impl(arena, pair.rest, map);
            arena.alloc(Type::Pair(Pair::new(first, rest)))
        }
        Type::Struct(ty) => {
            let inner = substitute_impl(arena, ty.inner, map);
            arena.alloc(Type::Struct(Struct { inner, ..ty }))
        }
        Type::Alias(alias) => {
            let inner = substitute_impl(arena, alias.inner, map);
            arena.alloc(Type::Alias(Alias { inner, ..alias }))
        }
        Type::Function(function) => {
            let params = function
                .params
                .iter()
                .map(|id| substitute_impl(arena, *id, map))
                .collect();
            let ret = substitute_impl(arena, function.ret, map);
            arena.alloc(Type::Function(FunctionType {
                params,
                ret,
                ..function
            }))
        }
        Type::Union(union) => {
            let types = union
                .types
                .iter()
                .map(|id| substitute_impl(arena, *id, map))
                .collect();
            arena.alloc(Type::Union(Union::new(types)))
        }
    }
}
