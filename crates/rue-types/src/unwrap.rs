use std::collections::HashSet;

use id_arena::Arena;

use crate::{Type, TypeId, substitute};

pub fn unwrap_semantic(arena: &mut Arena<Type>, id: TypeId, resolve_aliases: bool) -> TypeId {
    let id = substitute(arena, id);
    unwrap_semantic_without_substitution(arena, id, resolve_aliases)
}

/// Like `unwrap_semantic`, but for a type that has already been substituted.
pub(crate) fn unwrap_semantic_without_substitution(
    arena: &Arena<Type>,
    id: TypeId,
    resolve_aliases: bool,
) -> TypeId {
    unwrap_semantic_impl(arena, id, resolve_aliases, &mut HashSet::new())
}

fn unwrap_semantic_impl(
    arena: &Arena<Type>,
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
        | Type::Intersection(_)
        | Type::Any => id,
        Type::Union(ty) => {
            // The only member may itself be an alias or a reference, which needs unwrapping too
            if ty.types.len() == 1 {
                unwrap_semantic_impl(arena, ty.types[0], resolve_aliases, visited)
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
