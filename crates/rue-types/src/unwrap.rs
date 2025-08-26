use id_arena::Arena;

use crate::{Type, TypeId, substitute};

pub fn unwrap_semantic(arena: &mut Arena<Type>, id: TypeId, resolve_aliases: bool) -> TypeId {
    let id = substitute(arena, id);
    unwrap_semantic_impl(arena, id, resolve_aliases)
}

fn unwrap_semantic_impl(arena: &mut Arena<Type>, id: TypeId, resolve_aliases: bool) -> TypeId {
    match arena[id].clone() {
        Type::Apply(_) => unreachable!(),
        Type::Unresolved
        | Type::Generic
        | Type::Never
        | Type::Any
        | Type::List(_)
        | Type::Atom(_)
        | Type::Pair(_)
        | Type::Function(_)
        | Type::Struct(_)
        | Type::Union(_) => id,
        Type::Alias(alias) => {
            if resolve_aliases {
                unwrap_semantic_impl(arena, alias.inner, resolve_aliases)
            } else {
                id
            }
        }
    }
}
