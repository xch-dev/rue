use id_arena::Arena;
use indexmap::IndexMap;

use crate::{Type, TypeId};

pub fn stringify(arena: &Arena<Type>, id: TypeId) -> String {
    stringify_impl(arena, id, &mut IndexMap::new())
}

fn stringify_impl(arena: &Arena<Type>, id: TypeId, stack: &mut IndexMap<TypeId, bool>) -> String {
    let len = stack.len();

    if stack.contains_key(&id) {
        stack.insert(id, true);
        return format!("<{}>", stack.get_index_of(&id).unwrap());
    } else {
        stack.insert(id, false);
    }

    let result = match arena[id].clone() {
        Type::Unresolved => "{unresolved}".to_string(),
        Type::Generic => "{generic}".to_string(),
        Type::Ref(id) => stringify_impl(arena, id, stack),
        Type::Atom(atom) => atom.to_string(),
        Type::Pair(pair) => {
            let first = stringify_impl(arena, pair.first, stack);
            let rest = stringify_impl(arena, pair.rest, stack);
            format!("({first}, {rest})")
        }
        Type::Struct(ty) => stringify_impl(arena, ty.inner, stack),
        Type::Alias(alias) => stringify_impl(arena, alias.inner, stack),
        Type::Apply(apply) => {
            let inner = stringify_impl(arena, apply.inner, stack);
            let generics = apply
                .generics
                .iter()
                .map(|(k, v)| {
                    format!(
                        "{}: {}",
                        stringify_impl(arena, *k, stack),
                        stringify_impl(arena, *v, stack)
                    )
                })
                .collect::<Vec<_>>()
                .join(", ");
            format!("{inner}<{generics}>")
        }
        Type::Union(union) => union
            .types
            .iter()
            .map(|id| stringify_impl(arena, *id, stack))
            .collect::<Vec<_>>()
            .join(" | "),
    };

    let recursed = stack.pop().unwrap().1;

    if recursed {
        if result.starts_with("(") {
            format!("{} @ {}", len, result)
        } else {
            format!("{} @ ({})", len, result)
        }
    } else {
        result
    }
}
