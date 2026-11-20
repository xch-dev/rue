use std::cell::Cell;

use id_arena::Arena;
use indexmap::{IndexMap, IndexSet};

use crate::{Alias, DISCRIMINANT_FIELD, Struct, Type, TypeId, grow_stack, step, substitute};

pub fn stringify(arena: &mut Arena<Type>, id: TypeId) -> String {
    let id = substitute(arena, id);
    stringify_impl(arena, id, &mut IndexMap::new())
}

thread_local! {
    /// How many more parts of a type [`stringify_bounded`] writes before it leaves the rest out.
    static BUDGET: Cell<Option<usize>> = const { Cell::new(None) };
}

/// What's written in place of the parts of a type that [`stringify_bounded`] leaves out.
pub const ELIDED: &str = "...";

/// What's written for a type that couldn't be worked out.
const UNRESOLVED: &str = "{unresolved}";

/// Like [`stringify`], but only the first `parts` parts of the type are written (each type that it's
/// made of is one), and the rest are shown as `...`. A type can be far larger written out than it
/// is in memory, where its parts are shared, so this bounds the time and the length of the result
/// for types that are shown as they're inferred, such as by the language server.
pub fn stringify_bounded(arena: &mut Arena<Type>, id: TypeId, parts: usize) -> String {
    let id = substitute(arena, id);

    // The budget is cleared even if writing the type panics, so that it's only ever this type's
    let _unbounded = Unbounded;

    BUDGET.set(Some(parts));
    stringify_impl(arena, id, &mut IndexMap::new())
}

/// Clears the budget when it's dropped.
struct Unbounded;

impl Drop for Unbounded {
    fn drop(&mut self) {
        BUDGET.set(None);
    }
}

/// Counts a part of a type against the budget, and returns whether there was any of it left.
fn spend() -> bool {
    match BUDGET.get() {
        None => true,
        Some(0) => false,
        Some(parts) => {
            BUDGET.set(Some(parts - 1));
            true
        }
    }
}

pub fn stringify_without_substitution(arena: &Arena<Type>, id: TypeId) -> String {
    stringify_impl(arena, id, &mut IndexMap::new())
}

pub(crate) fn stringify_impl(
    arena: &Arena<Type>,
    id: TypeId,
    stack: &mut IndexMap<TypeId, bool>,
) -> String {
    grow_stack(|| stringify_impl_inner(arena, id, stack))
}

fn stringify_impl_inner(
    arena: &Arena<Type>,
    id: TypeId,
    stack: &mut IndexMap<TypeId, bool>,
) -> String {
    step();

    if !spend() {
        return ELIDED.to_string();
    }

    let len = stack.len();

    if stack.contains_key(&id) {
        stack.insert(id, true);
        return format!("<{}>", stack.get_index_of(&id).unwrap());
    }

    stack.insert(id, false);

    let result = match arena[id].clone() {
        Type::Ref(id) => stringify_impl(arena, id, stack),
        Type::Unresolved => UNRESOLVED.to_string(),
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
                .map(|(i, param)| {
                    let spread = if !function.nil_terminated && i == len - 1 {
                        "..."
                    } else {
                        ""
                    };
                    format!(
                        "{spread}{}: {}",
                        param.name,
                        stringify_impl(arena, param.ty, stack)
                    )
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
        // A member that's shown the same as an earlier one is left out if it's an atom, such as the
        // second `5` in a union that inference collected `5` into twice. Other types are kept apart
        // by their declaration, since two structs can have the same name
        Type::Union(_) => {
            // Each member takes at least one part to write, so the members that there's no
            // budget for aren't looked at, however many there are
            let limit = BUDGET.get().unwrap_or(usize::MAX);
            let mut members = IndexSet::new();
            let mut elided = !union_variants(arena, id, limit, &mut members);

            let mut variants = members
                .into_iter()
                .map(|id| {
                    let declaration =
                        (!matches!(arena[id], Type::Atom(_))).then(|| declaration(arena, id));
                    (declaration, stringify_impl(arena, id, stack))
                })
                .collect::<IndexSet<_>>()
                .into_iter()
                .map(|(_, name)| name)
                .collect::<Vec<_>>();

            // The members that were left out are shown once, after the ones that weren't
            let written = variants.len();
            variants.retain(|name| name != ELIDED);
            elided |= variants.len() < written;

            if elided {
                variants.push(ELIDED.to_string());
            }

            variants.join(" | ")
        }
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
    current: TypeId,
    stack: &mut IndexMap<TypeId, bool>,
) -> Option<Vec<Vec<String>>> {
    grow_stack(|| narrowed_fields_inner(arena, ty, index, current, stack))
}

fn narrowed_fields_inner(
    arena: &Arena<Type>,
    ty: &Struct,
    index: usize,
    mut current: TypeId,
    stack: &mut IndexMap<TypeId, bool>,
) -> Option<Vec<Vec<String>>> {
    let Some(field) = ty.fields.get_index(index) else {
        return Some(vec![vec![]]);
    };

    // The discriminant of an enum variant is part of the variant's name, so it isn't shown
    let hidden = field == DISCRIMINANT_FIELD;

    if !ty.nil_terminated && index == ty.fields.len() - 1 {
        if hidden {
            return Some(vec![vec![]]);
        }

        let value = stringify_impl(arena, current, stack);
        return Some(vec![vec![format!("...{field}: {value}")]]);
    }

    while let Type::Ref(id) = arena[current] {
        current = id;
    }

    match &arena[current] {
        Type::Pair(pair) => {
            let mut variants = narrowed_fields(arena, ty, index + 1, pair.rest, stack)?;

            if !hidden {
                let first = format!("{field}: {}", stringify_impl(arena, pair.first, stack));

                for fields in &mut variants {
                    fields.insert(0, first.clone());
                }
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

/// Adds the variants of a union, including those of directly nested unions, each of them once,
/// until there are as many as the limit. Returns whether that was all of them.
fn union_variants(
    arena: &Arena<Type>,
    id: TypeId,
    limit: usize,
    variants: &mut IndexSet<TypeId>,
) -> bool {
    grow_stack(|| union_variants_inner(arena, id, limit, variants))
}

fn union_variants_inner(
    arena: &Arena<Type>,
    id: TypeId,
    limit: usize,
    variants: &mut IndexSet<TypeId>,
) -> bool {
    match &arena[id] {
        Type::Union(union) => union
            .types
            .iter()
            .all(|&id| union_variants(arena, id, limit, variants)),
        // A member that's there more than once is only written once
        _ if variants.contains(&id) => true,
        _ if variants.len() < limit => {
            variants.insert(id);
            true
        }
        _ => false,
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

#[cfg(test)]
mod tests {
    use crate::{Atom, Pair, Union};

    use super::*;

    #[test]
    fn test_stringify_bounded() {
        let mut arena = Arena::new();

        let int = arena.alloc(Type::Atom(Atom::INT));
        let bytes = arena.alloc(Type::Atom(Atom::BYTES));

        // A type that's small enough is written in full
        let pair = arena.alloc(Type::Pair(Pair::new(int, bytes)));
        assert_eq!(stringify_bounded(&mut arena, pair, 3), "(Int, Bytes)");
        assert_eq!(stringify_bounded(&mut arena, pair, 2), "(Int, ...)");
        assert_eq!(stringify_bounded(&mut arena, pair, 0), "...");

        // A union says that members were left out only if they were
        let union = arena.alloc(Type::Union(Union::new(vec![int, bytes, pair])));
        let full = "Int | Bytes | (Int, Bytes)";
        assert_eq!(stringify_bounded(&mut arena, union, 6), full);
        assert_eq!(
            stringify_bounded(&mut arena, union, 5),
            "Int | Bytes | (Int, ...)"
        );
        assert_eq!(stringify_bounded(&mut arena, union, 3), "Int | Bytes | ...");
        assert_eq!(stringify_bounded(&mut arena, union, 2), "Int | ...");
        assert_eq!(stringify_bounded(&mut arena, union, 1), "...");

        // And so does a union inside of another type, which the union's members are part of
        let nested = arena.alloc(Type::Union(Union::new(vec![union, int])));
        let list = arena.alloc(Type::Pair(Pair::new(nested, bytes)));
        assert_eq!(
            stringify_bounded(&mut arena, list, 9),
            "(Int | Bytes | (Int, Bytes), Bytes)"
        );
        assert_eq!(
            stringify_bounded(&mut arena, list, 4),
            "(Int | Bytes | ..., ...)"
        );

        // A member that's in a union more than once doesn't count as left out
        let repeated = arena.alloc(Type::Union(Union::new(vec![union, union, int])));
        assert_eq!(stringify_bounded(&mut arena, repeated, 7), full);

        let same = arena.alloc(Type::Union(Union::new(vec![int; 100])));
        assert_eq!(stringify_bounded(&mut arena, same, 3), "Int");

        // A union with far more members than there's budget for isn't walked to the end
        let many = (0..100_000)
            .map(|_| arena.alloc(Type::Atom(Atom::INT)))
            .collect::<Vec<_>>();
        let wide = arena.alloc(Type::Union(Union::new(many)));
        assert_eq!(stringify_bounded(&mut arena, wide, 3), "Int | ...");

        // Each pair is made of the one before it twice, so the type is over a million times as
        // long written out as it is in memory
        let mut nested = int;

        for _ in 0..20 {
            nested = arena.alloc(Type::Pair(Pair::new(nested, nested)));
        }

        let short = stringify_bounded(&mut arena, nested, 8);
        assert!(short.len() < 200, "{short}");
        assert!(short.contains(ELIDED), "{short}");

        // The budget is only for the type it was given for
        assert_eq!(stringify(&mut arena, pair), "(Int, Bytes)");
    }
}
