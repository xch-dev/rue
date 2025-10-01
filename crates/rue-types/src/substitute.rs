use std::collections::HashMap;

use id_arena::Arena;
use indexmap::IndexMap;
use log::trace;

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
    trace!("Substituting {name}");
    let result = substitute_impl(arena, id, mappings, &mut IndexMap::new());
    let new_name = stringify_impl(arena, result, &mut IndexMap::new());
    trace!("Substitution from {name} yielded {new_name}");
    result
}

fn substitute_impl(
    arena: &mut Arena<Type>,
    mut old_id: TypeId,
    map: &HashMap<TypeId, TypeId>,
    stack: &mut IndexMap<TypeId, TypeId>,
) -> TypeId {
    if let Some(id) = map.get(&old_id) {
        old_id = *id;
    }

    if let Some(id) = stack.get(&old_id) {
        return *id;
    }

    let new_id = arena.alloc(Type::Unresolved);
    stack.insert(old_id, new_id);

    let result = match arena[old_id].clone() {
        Type::Apply(apply) => {
            let mut new_map = apply.generics.clone();

            for arg in new_map.values_mut() {
                *arg = substitute_impl(arena, *arg, map, stack);
            }

            substitute_impl(arena, apply.inner, &new_map, stack)
        }
        Type::Unresolved | Type::Generic(_) | Type::Atom(_) | Type::Never | Type::Any => old_id,
        Type::Ref(id) => substitute_impl(arena, id, map, stack),
        Type::Pair(pair) => {
            let first = substitute_impl(arena, pair.first, map, stack);
            let rest = substitute_impl(arena, pair.rest, map, stack);
            arena.alloc(Type::Pair(Pair::new(first, rest)))
        }
        Type::Struct(ty) => {
            let inner = substitute_impl(arena, ty.inner, map, stack);
            arena.alloc(Type::Struct(Struct { inner, ..ty }))
        }
        Type::Alias(alias) => {
            let inner = substitute_impl(arena, alias.inner, map, stack);
            arena.alloc(Type::Alias(Alias { inner, ..alias }))
        }
        Type::Function(function) => {
            let params = function
                .params
                .iter()
                .map(|id| substitute_impl(arena, *id, map, stack))
                .collect();
            let ret = substitute_impl(arena, function.ret, map, stack);
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
                .map(|id| substitute_impl(arena, *id, map, stack))
                .collect();
            arena.alloc(Type::Union(Union::new(types)))
        }
    };

    stack.pop().unwrap();

    *arena.get_mut(new_id).unwrap() = Type::Ref(result);

    result
}
