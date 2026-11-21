use std::cell::Cell;
use std::collections::HashSet;

use id_arena::Arena;
use indexmap::{IndexMap, IndexSet};

use rue_diagnostic::Name;

use crate::{
    Alias, Atom, AtomRestriction, AtomSemantic, DISCRIMINANT_FIELD, Struct, Type, TypeId,
    grow_stack, hash::FxIndexSet, step, substitute,
};

pub fn stringify(arena: &mut Arena<Type>, id: TypeId) -> String {
    let id = substitute(arena, id);
    stringify_impl(arena, id, &mut IndexMap::new())
}

thread_local! {
    /// How many more parts of a type [`stringify_bounded`] writes before it leaves the rest out.
    static BUDGET: Cell<Option<usize>> = const { Cell::new(None) };
}

thread_local! {
    /// Whether [`stringify_bounded`] has left any part of the type out.
    static CUT: Cell<bool> = const { Cell::new(false) };
}

/// What's written in place of the parts of a type that [`stringify_bounded`] leaves out.
pub const ELIDED: &str = "...";

/// Like [`stringify`], but only the first `parts` parts of the type are written (each type that it's
/// made of is one), and the rest are shown as `...`. A type can be far larger written out than it
/// is in memory, where its parts are shared, so this bounds the time and the length of the result
/// for types that are shown as they're inferred, such as by the language server.
///
/// Returns what was written, and whether anything was left out of it.
pub fn stringify_bounded(arena: &mut Arena<Type>, id: TypeId, parts: usize) -> (String, bool) {
    let id = substitute(arena, id);

    // The budget is cleared even if writing the type panics, so that it's only ever this type's
    let _unbounded = Unbounded;

    BUDGET.set(Some(parts));
    CUT.set(false);

    let result = stringify_impl(arena, id, &mut IndexMap::new());

    (result, CUT.get())
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
        Some(0) => {
            CUT.set(true);
            false
        }
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
        // A member is shown once however many times it's in the union. References to the same
        // struct are substituted into distinct types, and inference collects the same type into
        // a union once for each argument that it came from, so members that are the same type
        // written out are only displayed once. So is a member that another one already
        // includes, such as the `0` in `Int | 0`, and `true | false` is displayed as `Bool`.
        // Members that are only shown the same are kept apart, since two structs can have the
        // same name
        Type::Union(_) => {
            // Each member takes at least one part to write, so the members that there's no
            // budget for aren't looked at, however many there are
            let limit = BUDGET.get().unwrap_or(usize::MAX);
            let mut members = IndexSet::new();
            let mut elided = !union_variants(arena, id, limit, &mut members);

            let members = members
                .into_iter()
                .map(|id| (id, stringify_impl(arena, id, stack)))
                .collect::<Vec<_>>();

            let mut variants = distinct_members(arena, members);

            // The members that were left out are shown once, after the ones that weren't
            let written = variants.len();
            variants.retain(|name| name != ELIDED);
            elided |= variants.len() < written;

            if elided {
                CUT.set(true);
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
/// until there are as many as the limit. Returns whether that was all of them, which it can say
/// it wasn't when the only ones left are written the same as ones that were added.
fn union_variants(
    arena: &Arena<Type>,
    id: TypeId,
    limit: usize,
    variants: &mut IndexSet<TypeId>,
) -> bool {
    union_variants_impl(arena, id, limit, variants, &mut HashSet::new())
}

fn union_variants_impl(
    arena: &Arena<Type>,
    id: TypeId,
    limit: usize,
    variants: &mut IndexSet<TypeId>,
    unions: &mut HashSet<TypeId>,
) -> bool {
    grow_stack(|| union_variants_inner(arena, id, limit, variants, unions))
}

fn union_variants_inner(
    arena: &Arena<Type>,
    id: TypeId,
    limit: usize,
    variants: &mut IndexSet<TypeId>,
    unions: &mut HashSet<TypeId>,
) -> bool {
    match &arena[id] {
        // A union that's nested in more than one place only adds its members once, and types
        // that share their parts can have far more places than parts
        Type::Union(_) if !unions.insert(id) => true,
        Type::Union(union) => union
            .types
            .iter()
            .all(|&id| union_variants_impl(arena, id, limit, variants, unions)),
        // A member that's there more than once is only written once
        _ if variants.contains(&id) => true,
        _ if variants.len() < limit => {
            variants.insert(id);
            true
        }
        _ => false,
    }
}

/// The names of a union's members as they're displayed, given each member with its name: without
/// the members that are the same type as an earlier one or that another member includes, and with
/// `true` and `false` shown together as `Bool`.
fn distinct_members(arena: &Arena<Type>, members: Vec<(TypeId, String)>) -> Vec<String> {
    let mut members = members
        .into_iter()
        .map(|(id, name)| {
            // A member that wasn't written in full is only known by what was written of it
            let shown = if name.contains(ELIDED) {
                Shown::Name
            } else if let Some(value) = bool_value(arena, id) {
                Shown::Bool(Some(value))
            } else {
                Shown::Type(id)
            };
            (shown, name)
        })
        .collect::<Vec<_>>();

    // `true | false` is every `Bool`, which is shown where the first of them was
    let has = |value| {
        members
            .iter()
            .any(|(shown, _)| *shown == Shown::Bool(Some(value)))
    };

    if has(true) && has(false) {
        let mut merged = false;

        members.retain_mut(|(shown, name)| {
            if !matches!(shown, Shown::Bool(Some(_))) {
                return true;
            }

            if merged {
                return false;
            }

            merged = true;
            *shown = Shown::Bool(None);
            *name = AtomSemantic::Bool.to_string();

            true
        });
    }

    let mut kept: Vec<(Shown, String)> = Vec::new();

    for (shown, name) in members {
        // An atom is the same as an earlier one that's shown the same, such as the second `5`
        // in a union that inference collected `5` into twice
        let repeated = kept
            .iter()
            .any(|(other, other_name)| *other_name == name && shown.same_as(arena, other));

        if !repeated {
            kept.push((shown, name));
        }
    }

    // Of two members that include each other, the first is kept
    let included = (0..kept.len())
        .map(|i| {
            (0..kept.len()).any(|j| {
                i != j
                    && kept[j].0.includes(arena, &kept[i].0)
                    && !(i < j && kept[i].0.includes(arena, &kept[j].0))
            })
        })
        .collect::<Vec<_>>();

    kept.into_iter()
        .zip(included)
        .filter(|(_, included)| !included)
        .map(|((_, name), _)| name)
        .collect()
}

/// What's known about a member of a union for telling whether it's displayed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Shown {
    /// Only its name, since it wasn't written in full.
    Name,
    /// `true` or `false`, or `Bool` if the union has both.
    Bool(Option<bool>),
    Type(TypeId),
}

impl Shown {
    /// Whether two members that are shown the same are the same type.
    fn same_as(&self, arena: &Arena<Type>, other: &Self) -> bool {
        match (self, other) {
            (Self::Name, _) | (_, Self::Name) => false,
            (Self::Type(lhs), Self::Type(rhs)) => {
                Matcher::default().matches(arena, *lhs, *rhs, false)
            }
            _ => self == other,
        }
    }

    /// Whether every value of the other member is a value of this one, as far as that can be told
    /// from how the types are written.
    fn includes(&self, arena: &Arena<Type>, other: &Self) -> bool {
        match (self, other) {
            (Self::Type(lhs), Self::Type(rhs)) => {
                Matcher::default().matches(arena, *lhs, *rhs, true)
            }
            (Self::Type(lhs), Self::Bool(_)) => {
                matches!(unwrap_refs(arena, *lhs), Type::Atom(atom) if atom.semantic == AtomSemantic::Bool && atom.restriction.is_none())
            }
            _ => false,
        }
    }
}

/// Whether a type is the atom `true` or the atom `false`, and which.
fn bool_value(arena: &Arena<Type>, id: TypeId) -> Option<bool> {
    let Type::Atom(Atom {
        semantic: AtomSemantic::Bool,
        restriction: Some(AtomRestriction::Value(value)),
    }) = unwrap_refs(arena, id)
    else {
        return None;
    };

    match value.as_ref() {
        [] => Some(false),
        [1] => Some(true),
        _ => None,
    }
}

fn unwrap_refs(arena: &Arena<Type>, mut id: TypeId) -> &Type {
    while let Type::Ref(inner) = arena[id] {
        id = inner;
    }

    &arena[id]
}

/// How many pairs of types [`Matcher`] looks at before it says that two types aren't the same,
/// which only leaves a member of a union displayed that didn't need to be.
const MATCH_LIMIT: usize = 4096;

/// Tells whether two types are the same type written out, or whether one includes the other.
#[derive(Default)]
struct Matcher {
    /// The pairs that are being matched, which match if nothing else in them says otherwise.
    stack: FxIndexSet<(TypeId, TypeId, bool)>,
    work: usize,
}

impl Matcher {
    /// Whether the types are the same: made of the same parts in the same places, where a struct
    /// is the same as another instance of its declaration with the same generic arguments.
    ///
    /// With `includes`, whether the first type includes the second instead: an atom includes the
    /// values and lengths of it, and a pair includes a pair of what its parts include. This is
    /// only what can be told without comparing the types, so that it's the same whichever type is
    /// being displayed.
    fn matches(&mut self, arena: &Arena<Type>, lhs: TypeId, rhs: TypeId, includes: bool) -> bool {
        grow_stack(|| self.matches_inner(arena, lhs, rhs, includes))
    }

    fn matches_inner(
        &mut self,
        arena: &Arena<Type>,
        mut lhs: TypeId,
        mut rhs: TypeId,
        includes: bool,
    ) -> bool {
        while let Type::Ref(inner) = arena[lhs] {
            lhs = inner;
        }

        while let Type::Ref(inner) = arena[rhs] {
            rhs = inner;
        }

        if lhs == rhs {
            return true;
        }

        self.work += 1;

        if self.work > MATCH_LIMIT {
            return false;
        }

        if !self.stack.insert((lhs, rhs, includes)) {
            return true;
        }

        let result = match (&arena[lhs], &arena[rhs]) {
            // A union is included if each of its members is, and includes what a member does
            (_, Type::Union(_)) if includes => {
                let mut members = IndexSet::new();

                union_variants(arena, rhs, MATCH_LIMIT, &mut members)
                    && members
                        .iter()
                        .all(|&rhs| self.matches(arena, lhs, rhs, true))
            }
            (Type::Union(_), _) if includes => {
                let mut members = IndexSet::new();
                union_variants(arena, lhs, MATCH_LIMIT, &mut members);

                members
                    .iter()
                    .any(|&lhs| self.matches(arena, lhs, rhs, true))
            }
            (Type::Unresolved, Type::Unresolved)
            | (Type::Never, Type::Never)
            | (Type::Any, Type::Any) => true,
            (Type::Atom(lhs), Type::Atom(rhs)) => {
                lhs.to_string() == rhs.to_string() || (includes && atom_includes(lhs, rhs))
            }
            (Type::Pair(lhs), Type::Pair(rhs)) => {
                self.matches(arena, lhs.first, rhs.first, includes)
                    && self.matches(arena, lhs.rest, rhs.rest, includes)
            }
            (Type::Struct(lhs), Type::Struct(rhs)) => {
                lhs.semantic == rhs.semantic
                    && lhs.narrowed == rhs.narrowed
                    && self.all_match(arena, &lhs.generics, &rhs.generics)
                    && (!lhs.narrowed || self.matches(arena, lhs.inner, rhs.inner, false))
            }
            (Type::Alias(lhs), Type::Alias(rhs)) => {
                lhs.name.as_ref().map(Name::text) == rhs.name.as_ref().map(Name::text)
                    && self.all_match(arena, &lhs.generics, &rhs.generics)
                    && self.matches(arena, lhs.inner, rhs.inner, false)
            }
            (Type::Apply(lhs), Type::Apply(rhs)) => {
                lhs.generics.len() == rhs.generics.len()
                    && self.matches(arena, lhs.inner, rhs.inner, false)
                    && lhs.generics.iter().all(|(generic, &lhs)| {
                        rhs.generics
                            .get(generic)
                            .is_some_and(|&rhs| self.matches(arena, lhs, rhs, false))
                    })
            }
            (Type::Function(lhs), Type::Function(rhs)) => {
                lhs.generics == rhs.generics
                    && lhs.nil_terminated == rhs.nil_terminated
                    && lhs.params.len() == rhs.params.len()
                    && lhs.params.iter().zip(&rhs.params).all(|(lhs, rhs)| {
                        lhs.name == rhs.name && self.matches(arena, lhs.ty, rhs.ty, false)
                    })
                    && self.matches(arena, lhs.ret, rhs.ret, false)
            }
            (Type::Union(_), Type::Union(_)) => {
                let mut lhs_members = IndexSet::new();
                let mut rhs_members = IndexSet::new();

                union_variants(arena, lhs, MATCH_LIMIT, &mut lhs_members)
                    && union_variants(arena, rhs, MATCH_LIMIT, &mut rhs_members)
                    && lhs_members.iter().all(|&lhs| {
                        rhs_members
                            .iter()
                            .any(|&rhs| self.matches(arena, lhs, rhs, false))
                    })
                    && rhs_members.iter().all(|&rhs| {
                        lhs_members
                            .iter()
                            .any(|&lhs| self.matches(arena, lhs, rhs, false))
                    })
            }
            _ => false,
        };

        self.stack.pop();

        result
    }

    fn all_match(&mut self, arena: &Arena<Type>, lhs: &[TypeId], rhs: &[TypeId]) -> bool {
        lhs.len() == rhs.len()
            && lhs
                .iter()
                .zip(rhs)
                .all(|(&lhs, &rhs)| self.matches(arena, lhs, rhs, false))
    }
}

/// The value of an atom that's a value of `Bytes`.
fn bytes_value(atom: &Atom) -> Option<&[u8]> {
    match atom {
        Atom {
            semantic: AtomSemantic::Bytes,
            restriction: Some(AtomRestriction::Value(value)),
        } => Some(value),
        _ => None,
    }
}

/// Whether every value of the second atom is a value of the first, and of the same kind.
fn atom_includes(lhs: &Atom, rhs: &Atom) -> bool {
    // `nil` is kept beside the `Bytes` that it's a value of, since it's written there to say
    // that there might be no value, as in a `T | nil` whose `T` is `Bytes`
    if lhs.semantic != rhs.semantic || bytes_value(rhs).is_some_and(<[u8]>::is_empty) {
        return false;
    }

    match (&lhs.restriction, &rhs.restriction) {
        (None, _) => true,
        (Some(_), None) => false,
        (Some(AtomRestriction::Length(length)), Some(AtomRestriction::Value(value))) => {
            value.len() == *length
        }
        (Some(lhs), Some(rhs)) => lhs == rhs,
    }
}

#[cfg(test)]
mod tests {
    use crate::{FunctionParameter, FunctionType, Pair, Union};

    use super::*;

    fn bounded(arena: &mut Arena<Type>, id: TypeId, parts: usize) -> String {
        let (name, cut) = stringify_bounded(arena, id, parts);

        // Whether anything was left out is what the marks for it say
        assert_eq!(cut, name.contains(ELIDED), "{name}");

        name
    }

    #[test]
    fn test_stringify_bounded() {
        let mut arena = Arena::new();

        let int = arena.alloc(Type::Atom(Atom::INT));
        let bytes = arena.alloc(Type::Atom(Atom::BYTES));

        // A type that's small enough is written in full
        let pair = arena.alloc(Type::Pair(Pair::new(int, bytes)));
        assert_eq!(bounded(&mut arena, pair, 3), "(Int, Bytes)");
        assert_eq!(bounded(&mut arena, pair, 2), "(Int, ...)");
        assert_eq!(bounded(&mut arena, pair, 0), "...");

        // A union says that members were left out only if they were
        let union = arena.alloc(Type::Union(Union::new(vec![int, bytes, pair])));
        let full = "Int | Bytes | (Int, Bytes)";
        assert_eq!(bounded(&mut arena, union, 6), full);
        assert_eq!(bounded(&mut arena, union, 5), "Int | Bytes | (Int, ...)");
        assert_eq!(bounded(&mut arena, union, 3), "Int | Bytes | ...");
        assert_eq!(bounded(&mut arena, union, 2), "Int | ...");
        assert_eq!(bounded(&mut arena, union, 1), "...");

        // And so does a union inside of another type, which the union's members are part of
        let nested = arena.alloc(Type::Union(Union::new(vec![union, int])));
        let list = arena.alloc(Type::Pair(Pair::new(nested, bytes)));
        assert_eq!(
            bounded(&mut arena, list, 9),
            "(Int | Bytes | (Int, Bytes), Bytes)"
        );
        assert_eq!(bounded(&mut arena, list, 4), "(Int | Bytes | ..., ...)");

        // A member that there's no budget left for by the time it's written is left out like
        // the ones that weren't looked at, and they're all shown once
        let int_bytes = arena.alloc(Type::Pair(Pair::new(int, bytes)));
        let bytes_int = arena.alloc(Type::Pair(Pair::new(bytes, int)));
        let bytes_bytes = arena.alloc(Type::Pair(Pair::new(bytes, bytes)));
        let pairs = arena.alloc(Type::Union(Union::new(vec![
            int_bytes,
            bytes_int,
            bytes_bytes,
        ])));
        assert_eq!(bounded(&mut arena, pairs, 4), "(Int, Bytes) | ...");
        assert_eq!(
            bounded(&mut arena, pairs, 5),
            "(Int, Bytes) | (..., ...) | ..."
        );
        assert_eq!(
            bounded(&mut arena, pairs, 7),
            "(Int, Bytes) | (Bytes, Int) | ..."
        );
        assert_eq!(
            bounded(&mut arena, pairs, 10),
            "(Int, Bytes) | (Bytes, Int) | (Bytes, Bytes)"
        );

        let inner = arena.alloc(Type::Union(Union::new(vec![bytes, int_bytes])));
        let alias = arena.alloc(Type::Alias(Alias {
            name: None,
            inner,
            generics: Vec::new(),
        }));
        let mixed = arena.alloc(Type::Union(Union::new(vec![int, alias, bytes_int])));
        assert_eq!(bounded(&mut arena, mixed, 3), "Int | ...");

        // A member that's in a union more than once doesn't count as left out
        let repeated = arena.alloc(Type::Union(Union::new(vec![union, union, int])));
        assert_eq!(bounded(&mut arena, repeated, 7), full);

        let same = arena.alloc(Type::Union(Union::new(vec![int; 100])));
        assert_eq!(bounded(&mut arena, same, 3), "Int");
        assert_eq!(bounded(&mut arena, same, 2), "Int");

        // A union with far more members than there's budget for isn't walked to the end
        let mut doubled = union;

        for _ in 0..40 {
            doubled = arena.alloc(Type::Union(Union::new(vec![doubled, doubled])));
        }

        let wide = arena.alloc(Type::Union(Union::new(vec![doubled, int_bytes, bytes_int])));
        assert_eq!(
            bounded(&mut arena, wide, 4),
            "Int | Bytes | (..., ...) | ..."
        );

        // Each pair is made of the one before it twice, so the type is over a million times as
        // long written out as it is in memory
        let mut nested = int;

        for _ in 0..20 {
            nested = arena.alloc(Type::Pair(Pair::new(nested, nested)));
        }

        let short = bounded(&mut arena, nested, 8);
        assert!(short.len() < 200, "{short}");
        assert!(short.contains(ELIDED), "{short}");

        // The budget is only for the type it was given for
        assert_eq!(stringify(&mut arena, pair), "(Int, Bytes)");
    }

    fn union(arena: &mut Arena<Type>, types: &[TypeId]) -> String {
        let union = arena.alloc(Type::Union(Union::new(types.to_vec())));
        stringify(arena, union)
    }

    fn pair(arena: &mut Arena<Type>, first: TypeId, rest: TypeId) -> TypeId {
        arena.alloc(Type::Pair(Pair::new(first, rest)))
    }

    fn value(arena: &mut Arena<Type>, semantic: AtomSemantic, value: &[u8]) -> TypeId {
        arena.alloc(Type::Atom(Atom::new(
            semantic,
            Some(AtomRestriction::Value(value.to_vec().into())),
        )))
    }

    #[test]
    fn test_union_members_shown_once() {
        let mut arena = Arena::new();
        let arena = &mut arena;

        let int = arena.alloc(Type::Atom(Atom::INT));
        let bytes = arena.alloc(Type::Atom(Atom::BYTES));

        // Members that are the same type written out are shown once, even if they were
        // allocated separately
        let first = pair(arena, int, int);
        let second = pair(arena, int, int);
        let other = pair(arena, int, bytes);
        assert_eq!(
            union(arena, &[first, second, other, first]),
            "(Int, Int) | (Int, Bytes)"
        );

        // Whatever the order of the members of a union inside of them
        let one = value(arena, AtomSemantic::Int, &[1]);
        let two = value(arena, AtomSemantic::Int, &[2]);
        let one_two = arena.alloc(Type::Union(Union::new(vec![one, two])));
        let two_one = arena.alloc(Type::Union(Union::new(vec![two, one])));
        let first = pair(arena, int, one_two);
        let second = pair(arena, int, two_one);
        assert_eq!(union(arena, &[first, second]), "(Int, 1 | 2)");

        // Aliases with the same name and arguments are the same if what they're aliases of is
        let alias = |arena: &mut Arena<Type>, inner, generics: Vec<TypeId>| {
            arena.alloc(Type::Alias(Alias {
                name: Some(Name::new("Item", None)),
                inner,
                generics,
            }))
        };
        let first = alias(arena, int, vec![int]);
        let second = alias(arena, int, vec![int]);
        let different = alias(arena, bytes, vec![int]);
        assert_eq!(union(arena, &[first, second]), "Item<Int>");
        assert_eq!(union(arena, &[first, different]), "Item<Int> | Item<Int>");

        // A recursive alias is the same as another copy of it
        let list = |arena: &mut Arena<Type>| {
            let nil = arena.alloc(Type::Atom(Atom::NIL));
            let list = arena.alloc(Type::Unresolved);
            let item = arena.alloc(Type::Pair(Pair::new(int, list)));
            let inner = arena.alloc(Type::Union(Union::new(vec![nil, item])));
            arena[list] = Type::Alias(Alias {
                name: Some(Name::new("List", None)),
                inner,
                generics: vec![int],
            });
            list
        };
        let first = list(arena);
        let second = list(arena);
        assert_eq!(union(arena, &[first, second]), "List<Int>");

        // Structs are the same if they're instances of the same declaration, not if they only
        // have the same name
        let structure = |arena: &mut Arena<Type>, semantic| {
            arena.alloc(Type::Struct(Struct {
                name: Some(Name::new("Point", None)),
                semantic,
                inner: int,
                generics: Vec::new(),
                fields: IndexSet::new(),
                nil_terminated: true,
                narrowed: false,
            }))
        };
        let first = structure(arena, int);
        let second = structure(arena, int);
        let different = structure(arena, bytes);
        assert_eq!(union(arena, &[first, second]), "Point");
        assert_eq!(union(arena, &[first, different]), "Point | Point");

        let first = pair(arena, first, int);
        let different = pair(arena, different, int);
        assert_eq!(
            union(arena, &[first, different]),
            "(Point, Int) | (Point, Int)"
        );

        // Functions are the same if their parameters and return types are
        let function = |arena: &mut Arena<Type>, param, ret| {
            arena.alloc(Type::Function(FunctionType {
                generics: Vec::new(),
                params: vec![FunctionParameter::new("x", param)],
                nil_terminated: true,
                ret,
            }))
        };
        let first = function(arena, int, one_two);
        let second = function(arena, int, two_one);
        assert_eq!(union(arena, &[first, second]), "fn(x: Int) -> 1 | 2");
    }

    #[test]
    fn test_union_members_included_in_others() {
        let mut arena = Arena::new();
        let arena = &mut arena;

        let int = arena.alloc(Type::Atom(Atom::INT));
        let bytes = arena.alloc(Type::Atom(Atom::BYTES));
        let bytes32 = arena.alloc(Type::Atom(Atom::BYTES_32));
        let nil = arena.alloc(Type::Atom(Atom::NIL));
        let zero = value(arena, AtomSemantic::Int, &[]);
        let one = value(arena, AtomSemantic::Int, &[1]);
        let text = value(arena, AtomSemantic::String, b"a");
        let hash = value(arena, AtomSemantic::Bytes, &[7; 32]);

        // A value or a length of an atom is included in the atom, whichever comes first
        assert_eq!(union(arena, &[int, zero]), "Int");
        assert_eq!(union(arena, &[zero, int, one]), "Int");
        assert_eq!(union(arena, &[bytes32, bytes, hash]), "Bytes");
        assert_eq!(union(arena, &[bytes32, hash]), "Bytes32");

        // Other than `nil`, which says that there might be no value
        assert_eq!(union(arena, &[bytes, nil]), "Bytes | nil");
        assert_eq!(union(arena, &[bytes32, nil, nil]), "Bytes32 | nil");
        let first = pair(arena, bytes, nil);
        let second = pair(arena, nil, nil);
        assert_eq!(union(arena, &[first, second]), "(Bytes, nil) | (nil, nil)");

        // But not in an atom of another kind, which it can't be assigned to without a cast
        assert_eq!(union(arena, &[bytes, text, zero]), "Bytes | \"a\" | 0");
        assert_eq!(union(arena, &[zero, one]), "0 | 1");

        // A pair is included in a pair of what includes its parts
        let wide = pair(arena, bytes32, int);
        let narrow = pair(arena, bytes32, zero);
        let mixed = pair(arena, hash, one);
        assert_eq!(union(arena, &[wide, narrow, mixed]), "(Bytes32, Int)");
        assert_eq!(union(arena, &[narrow, mixed, wide]), "(Bytes32, Int)");

        let left = pair(arena, int, one);
        let right = pair(arena, zero, int);
        assert_eq!(union(arena, &[left, right]), "(Int, 1) | (0, Int)");

        // A union is included if all of its members are, and includes what one of them does
        let zero_one = arena.alloc(Type::Union(Union::new(vec![zero, one])));
        let int_nil = arena.alloc(Type::Union(Union::new(vec![int, nil])));
        let first = pair(arena, zero_one, bytes);
        let second = pair(arena, int_nil, bytes);
        let third = pair(arena, zero, bytes);
        assert_eq!(union(arena, &[first, second]), "(Int | nil, Bytes)");
        assert_eq!(union(arena, &[third, first]), "(0 | 1, Bytes)");

        // Of two members that include each other, the first is shown
        let int_zero = arena.alloc(Type::Union(Union::new(vec![int, zero])));
        let first = pair(arena, int_zero, bytes);
        let second = pair(arena, int, bytes);
        assert_eq!(union(arena, &[first, second]), "(Int, Bytes)");
        assert_eq!(union(arena, &[second, first]), "(Int, Bytes)");

        // The parameters of a function aren't included in wider ones
        let function = |arena: &mut Arena<Type>, param, ret| {
            arena.alloc(Type::Function(FunctionType {
                generics: Vec::new(),
                params: vec![FunctionParameter::new("x", param)],
                nil_terminated: true,
                ret,
            }))
        };
        let first = function(arena, int, int);
        let second = function(arena, zero, int);
        assert_eq!(
            union(arena, &[first, second]),
            "fn(x: Int) -> Int | fn(x: 0) -> Int"
        );
    }

    #[test]
    fn test_union_bool() {
        let mut arena = Arena::new();
        let arena = &mut arena;

        let int = arena.alloc(Type::Atom(Atom::INT));
        let bool = arena.alloc(Type::Atom(Atom::new(AtomSemantic::Bool, None)));
        let yes = arena.alloc(Type::Atom(Atom::TRUE));
        let no = arena.alloc(Type::Atom(Atom::FALSE));
        let other_yes = arena.alloc(Type::Atom(Atom::TRUE));

        // Both values of `Bool` are shown as `Bool`, where the first of them was
        assert_eq!(union(arena, &[yes, no]), "Bool");
        assert_eq!(union(arena, &[no, other_yes, yes]), "Bool");
        assert_eq!(union(arena, &[int, no, yes]), "Int | Bool");
        assert_eq!(union(arena, &[yes, int, no]), "Bool | Int");

        // One of them is shown as it is
        assert_eq!(union(arena, &[yes, int, other_yes]), "true | Int");

        // And `Bool` itself includes them
        assert_eq!(union(arena, &[yes, bool]), "Bool");
        assert_eq!(union(arena, &[yes, no, bool]), "Bool");
        assert_eq!(union(arena, &[bool, no, int, yes]), "Bool | Int");

        let list = arena.alloc(Type::Union(Union::new(vec![yes, no])));
        let item = pair(arena, list, int);
        assert_eq!(stringify(arena, item), "(Bool, Int)");
    }

    #[test]
    fn test_union_members_left_out() {
        let mut arena = Arena::new();
        let arena = &mut arena;

        let int = arena.alloc(Type::Atom(Atom::INT));
        let bytes = arena.alloc(Type::Atom(Atom::BYTES));

        // Members that weren't written in full aren't known to be the same
        let first = pair(arena, int, bytes);
        let second = pair(arena, int, bytes);
        let types = arena.alloc(Type::Union(Union::new(vec![first, second])));
        assert_eq!(bounded(arena, types, 7), "(Int, Bytes)");
        assert_eq!(bounded(arena, types, 6), "(Int, Bytes) | (Int, ...)");
    }
}
