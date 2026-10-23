use id_arena::Arena;
use indexmap::{IndexMap, IndexSet};

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
            if let Some(name) = &ty.name {
                if ty.narrowed {
                    narrowed_struct(arena, name.text(), &ty, stack)
                } else {
                    with_generics(arena, name.text(), &ty.generics, stack)
                }
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
            let len = function.params.len();
            let params = function
                .params
                .iter()
                .enumerate()
                .map(|(i, (name, id))| {
                    let spread = if !function.nil_terminated && i == len - 1 {
                        "..."
                    } else {
                        ""
                    };
                    format!("{spread}{name}: {}", stringify_impl(arena, *id, stack))
                })
                .collect::<Vec<_>>()
                .join(", ");
            let ret = stringify_impl(arena, function.ret, stack);
            format!(
                "{}({params}) -> {ret}",
                with_generics(arena, "fn", &function.generics, stack)
            )
        }
        // References to the same struct are substituted into distinct types, so variants which come
        // from the same declaration and look the same are only displayed once
        Type::Union(_) => union_variants(arena, id)
            .into_iter()
            .map(|id| (declaration(arena, id), stringify_impl(arena, id, stack)))
            .collect::<IndexSet<_>>()
            .into_iter()
            .map(|(_, name)| name)
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

/// Renders a struct whose fields have been narrowed, e.g. `Box { value: Int }`, since its generic
/// arguments no longer describe them.
fn narrowed_struct(
    arena: &Arena<Type>,
    name: &str,
    ty: &Struct,
    stack: &mut IndexMap<TypeId, bool>,
) -> String {
    let Some(variants) = narrowed_fields(arena, ty, 0, ty.inner, stack) else {
        return with_generics(arena, name, &ty.generics, stack);
    };

    variants
        .into_iter()
        .map(|fields| {
            if fields.is_empty() {
                name.to_string()
            } else {
                format!("{name} {{ {} }}", fields.join(", "))
            }
        })
        .collect::<IndexSet<_>>()
        .into_iter()
        .collect::<Vec<_>>()
        .join(" | ")
}

/// Renders the fields of a struct starting at `index`, where `current` is the type of the rest of
/// the struct. If the fields are correlated, each variant is rendered separately. Returns `None` if
/// the type doesn't have the structure of the fields.
fn narrowed_fields(
    arena: &Arena<Type>,
    ty: &Struct,
    index: usize,
    mut current: TypeId,
    stack: &mut IndexMap<TypeId, bool>,
) -> Option<Vec<Vec<String>>> {
    let Some(field) = ty.fields.get_index(index) else {
        return Some(vec![vec![]]);
    };

    if !ty.nil_terminated && index == ty.fields.len() - 1 {
        let value = stringify_impl(arena, current, stack);
        return Some(vec![vec![format!("...{field}: {value}")]]);
    }

    while let Type::Ref(id) = arena[current] {
        current = id;
    }

    match &arena[current] {
        Type::Pair(pair) => {
            let first = format!("{field}: {}", stringify_impl(arena, pair.first, stack));
            let mut variants = narrowed_fields(arena, ty, index + 1, pair.rest, stack)?;

            for fields in &mut variants {
                fields.insert(0, first.clone());
            }

            Some(variants)
        }
        Type::Union(union) => {
            let mut variants = Vec::new();

            for &id in &union.types {
                variants.extend(narrowed_fields(arena, ty, index, id, stack)?);
            }

            Some(variants)
        }
        _ => None,
    }
}

/// The variants of a union, including those of directly nested unions.
fn union_variants(arena: &Arena<Type>, id: TypeId) -> Vec<TypeId> {
    match &arena[id] {
        Type::Union(union) => union
            .types
            .iter()
            .flat_map(|&id| union_variants(arena, id))
            .collect(),
        _ => vec![id],
    }
}

/// The struct declaration that a type is an instance of, or the type itself otherwise.
fn declaration(arena: &Arena<Type>, mut id: TypeId) -> TypeId {
    loop {
        match &arena[id] {
            Type::Ref(inner) => id = *inner,
            Type::Apply(apply) => id = apply.inner,
            Type::Struct(ty) => return ty.semantic,
            _ => return id,
        }
    }
}
