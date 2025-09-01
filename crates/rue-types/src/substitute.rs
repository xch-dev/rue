use std::collections::{HashMap, HashSet};

use id_arena::Arena;
use indexmap::IndexMap;
use log::debug;

use crate::{Alias, FunctionType, Pair, Struct, Type, TypeId, Union, stringify_impl};

pub fn substitute(arena: &mut Arena<Type>, id: TypeId) -> TypeId {
    substitute_with_mappings(arena, id, &HashMap::new())
}

pub fn substitute_with_mappings(
    arena: &mut Arena<Type>,
    id: TypeId,
    mappings: &HashMap<TypeId, TypeId>,
) -> TypeId {
    let name = stringify_impl(arena, id, &mut IndexMap::new());
    debug!("Substituting {name}");
    let result = substitute_impl(arena, id, mappings, &mut HashMap::new());
    let new_name = stringify_impl(arena, result, &mut IndexMap::new());
    debug!("Substitution from {name} yielded {new_name}");
    result
}

fn substitute_impl(
    arena: &mut Arena<Type>,
    mut old_id: TypeId,
    map: &HashMap<TypeId, TypeId>,
    cache: &mut HashMap<TypeId, TypeId>,
) -> TypeId {
    let mut seen_from_map = HashSet::new();

    while let Some(id) = map.get(&old_id) {
        if !seen_from_map.insert(*id) {
            return *id;
        }
        old_id = *id;
    }

    if let Some(id) = cache.get(&old_id) {
        return *id;
    }

    let new_id = arena.alloc(Type::Unresolved);
    cache.insert(old_id, new_id);

    let result = match arena[old_id].clone() {
        Type::Apply(apply) => {
            let mut new_map = apply.generics.clone();

            for arg in new_map.values_mut() {
                *arg = substitute_impl(arena, *arg, map, cache);
            }

            substitute_impl(arena, apply.inner, &new_map, cache)
        }
        Type::Unresolved | Type::Generic | Type::Atom(_) | Type::Never => old_id,
        Type::Ref(id) => substitute_impl(arena, id, map, cache),
        Type::Pair(pair) => {
            let first = substitute_impl(arena, pair.first, map, cache);
            let rest = substitute_impl(arena, pair.rest, map, cache);
            arena.alloc(Type::Pair(Pair::new(first, rest)))
        }
        Type::Struct(ty) => {
            let inner = substitute_impl(arena, ty.inner, map, cache);
            arena.alloc(Type::Struct(Struct { inner, ..ty }))
        }
        Type::Alias(alias) => {
            let inner = substitute_impl(arena, alias.inner, map, cache);
            arena.alloc(Type::Alias(Alias { inner, ..alias }))
        }
        Type::Function(function) => {
            let inner = substitute_impl(arena, function.inner, map, cache);
            let params = function
                .params
                .iter()
                .map(|id| substitute_impl(arena, *id, map, cache))
                .collect();
            let ret = substitute_impl(arena, function.ret, map, cache);
            arena.alloc(Type::Function(FunctionType {
                params,
                ret,
                inner,
                ..function
            }))
        }
        Type::Union(union) => {
            let types = union
                .types
                .iter()
                .map(|id| substitute_impl(arena, *id, map, cache))
                .collect();
            arena.alloc(Type::Union(Union::new(types)))
        }
    };

    *arena.get_mut(new_id).unwrap() = Type::Ref(result);

    result
}
