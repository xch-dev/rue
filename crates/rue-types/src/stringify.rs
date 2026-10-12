use id_arena::Arena;
use indexmap::IndexMap;

use crate::{Alias, Struct, Type, TypeId, substitute};

pub fn stringify(arena: &mut Arena<Type>, id: TypeId) -> String {
    let id = substitute(arena, id);
    stringify_impl(arena, id, &mut IndexMap::new())
}

pub fn stringify_without_substitution(arena: &Arena<Type>, id: TypeId) -> String {
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
                with_generics(arena, name.text(), &ty.generics, stack)
            } else {
                stringify_impl(arena, ty.inner, stack)
            }
        }
        Type::Alias(alias) => {
            if let Some(name) = alias.name {
                with_generics(arena, name.text(), &alias.generics, stack)
            } else {
                stringify_impl(arena, alias.inner, stack)
            }
        }
        Type::Apply(apply) => {
            // Order the arguments by the generic parameters they're applied to, if known.
            let (inner, parameters) = match &arena[apply.inner] {
                Type::Alias(Alias {
                    name: Some(name),
                    generics,
                    ..
                })
                | Type::Struct(Struct {
                    name: Some(name),
                    generics,
                    ..
                }) => (name.text().to_string(), generics.clone()),
                _ => (
                    stringify_impl(arena, apply.inner, stack),
                    apply.generics.keys().copied().collect(),
                ),
            };

            let arguments = parameters
                .iter()
                .map(|parameter| apply.generics.get(parameter).copied().unwrap_or(*parameter))
                .collect::<Vec<_>>();

            with_generics(arena, &inner, &arguments, stack)
        }
        Type::Function(function) => {
            let params = function
                .params
                .iter()
                .map(|(name, id)| format!("{}: {}", name, stringify_impl(arena, *id, stack)))
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

fn with_generics(
    arena: &Arena<Type>,
    name: &str,
    generics: &[TypeId],
    stack: &mut IndexMap<TypeId, bool>,
) -> String {
    if generics.is_empty() {
        return name.to_string();
    }

    let generics = generics
        .iter()
        .map(|&id| stringify_impl(arena, id, stack))
        .collect::<Vec<_>>()
        .join(", ");

    format!("{name}<{generics}>")
}
