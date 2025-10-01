use id_arena::Arena;
use indexmap::IndexMap;

use crate::{Type, TypeId, substitute};

pub fn stringify(arena: &mut Arena<Type>, id: TypeId) -> String {
    let id = substitute(arena, id);
    stringify_impl(arena, id, &mut IndexMap::new())
}

pub(crate) fn stringify_impl(
    arena: &Arena<Type>,
    id: TypeId,
    stack: &mut IndexMap<TypeId, bool>,
) -> String {
    let len = stack.len();

    if stack.contains_key(&id) {
        stack.insert(id, true);
        return format!("<{}>", stack.get_index_of(&id).unwrap());
    }

    stack.insert(id, false);

    let result = match arena[id].clone() {
        Type::Ref(id) => stringify_impl(arena, id, stack),
        Type::Unresolved => "{unresolved}".to_string(),
        Type::Generic(generic) => {
            if let Some(name) = generic.name {
                name.text().to_string()
            } else {
                format!("{{generic {}}}", id.index())
            }
        }
        Type::Never => "Never".to_string(),
        Type::Any => "Any".to_string(),
        Type::Atom(atom) => atom.to_string(),
        Type::Pair(pair) => {
            let first = stringify_impl(arena, pair.first, stack);
            let rest = stringify_impl(arena, pair.rest, stack);
            format!("({first}, {rest})")
        }
        Type::Struct(ty) => {
            if let Some(name) = ty.name {
                name.text().to_string()
            } else {
                stringify_impl(arena, ty.inner, stack)
            }
        }
        Type::Alias(alias) => {
            if let Some(name) = alias.name {
                name.text().to_string()
            } else {
                stringify_impl(arena, alias.inner, stack)
            }
        }
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
        Type::Function(function) => {
            let params = function
                .params
                .iter()
                .map(|id| stringify_impl(arena, *id, stack))
                .collect::<Vec<_>>()
                .join(", ");
            let ret = stringify_impl(arena, function.ret, stack);
            format!("fn({params}) -> {ret}")
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
        if result.starts_with('(') {
            format!("{len} @ {result}")
        } else {
            format!("{len} @ ({result})")
        }
    } else {
        result
    }
}
