use std::collections::HashSet;

use id_arena::Arena;

use crate::{Type, TypeId, substitute};

pub fn unwrap_semantic(arena: &mut Arena<Type>, id: TypeId, resolve_aliases: bool) -> TypeId {
    let id = substitute(arena, id);
    unwrap_semantic_impl(arena, id, resolve_aliases, &mut HashSet::new())
}

fn unwrap_semantic_impl(
    arena: &mut Arena<Type>,
    id: TypeId,
    resolve_aliases: bool,
    visited: &mut HashSet<TypeId>,
) -> TypeId {
    match arena[id].clone() {
        Type::Apply(_) => unreachable!(),
        Type::Ref(inner) => {
            if visited.insert(id) {
                unwrap_semantic_impl(arena, inner, resolve_aliases, visited)
            } else {
                id
            }
        }
        Type::Unresolved
        | Type::Generic(_)
        | Type::Never
        | Type::Atom(_)
        | Type::Pair(_)
        | Type::Function(_)
        | Type::Struct(_)
        | Type::Any => id,
        Type::Union(ty) => {
            if ty.types.len() == 1 {
                ty.types[0]
            } else {
                id
            }
        }
        Type::Alias(alias) => {
            // Stop at the alias if it's part of a non-contractive cycle
            if resolve_aliases && !visited.insert(id) {
                id
            } else if resolve_aliases {
                unwrap_semantic_impl(arena, alias.inner, resolve_aliases, visited)
            } else {
                id
            }
        }
    }
}
