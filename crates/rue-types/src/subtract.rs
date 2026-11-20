use std::collections::HashSet;

use id_arena::Arena;
use indexmap::IndexMap;
use log::{Level, log_enabled, trace};

use crate::{
    Alias, BuiltinTypes, Comparison, Pair, Type, TypeId, Union, compare, grow_stack, pairs_of,
    reaches_non_contractive_cycle, step, stringify_impl, substitute,
};

pub fn subtract(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    lhs_id: TypeId,
    rhs_id: TypeId,
) -> TypeId {
    let lhs_id = substitute(arena, lhs_id);
    let rhs_id = substitute(arena, rhs_id);

    // These should be rejected before reaching this point, but they would recurse forever
    if reaches_non_contractive_cycle(arena, lhs_id) {
        return lhs_id;
    }

    let names = log_enabled!(Level::Trace).then(|| {
        (
            stringify_impl(arena, lhs_id, &mut IndexMap::new()),
            stringify_impl(arena, rhs_id, &mut IndexMap::new()),
        )
    });

    if let Some((lhs_name, rhs_name)) = &names {
        trace!("Subtracting {lhs_name} from {rhs_name}");
    }

    let result = subtract_impl(arena, builtins, lhs_id, rhs_id);

    if let Some((lhs_name, rhs_name)) = &names {
        let new_name = stringify_impl(arena, result, &mut IndexMap::new());
        trace!("Subtraction from {lhs_name} to {rhs_name} yielded {new_name}");
    }

    result
}

fn subtract_impl(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    lhs_id: TypeId,
    rhs_id: TypeId,
) -> TypeId {
    let variants = remaining_variants(arena, builtins, lhs_id, rhs_id, &mut HashSet::new())
        .unwrap_or_else(|| variants_of(arena, builtins, lhs_id));

    repackage(arena, builtins, variants)
}

/// The variants of `lhs_id` that aren't covered by `rhs_id`, or `None` if none of them changed.
///
/// A pair variant whose parts aren't all covered is split by the one part that differs, if the
/// other is covered: `(X, Y) - (P, Y) = (X - P, Y)`. The values that fail the check against
/// `(P, Y)` are exactly the ones whose first value isn't a `P`. When both parts differ, the
/// remainder isn't a single pair, so the variant is kept whole.
///
/// The parts of a recursive type can lead back to a subtraction that's already in progress, such
/// as `L - R` with `type L = Bytes | (Int, L)` and `type R = Int | (Int, R)`. That subtraction
/// keeps its part whole, so the result is only wider than it could be.
fn remaining_variants(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    lhs_id: TypeId,
    rhs_id: TypeId,
    stack: &mut HashSet<(TypeId, TypeId)>,
) -> Option<Vec<Variant>> {
    grow_stack(|| remaining_variants_inner(arena, builtins, lhs_id, rhs_id, stack))
}

fn remaining_variants_inner(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    lhs_id: TypeId,
    rhs_id: TypeId,
    stack: &mut HashSet<(TypeId, TypeId)>,
) -> Option<Vec<Variant>> {
    step();

    if !stack.insert((lhs_id, rhs_id)) {
        return None;
    }

    let variants = variants_of(arena, builtins, lhs_id);
    let mut rhs_pairs = None;
    let mut changed = false;
    let mut remaining = Vec::with_capacity(variants.len());

    for mut variant in variants {
        if compare(arena, builtins, variant.type_id, rhs_id) <= Comparison::Cast {
            changed = true;
            continue;
        }

        let Type::Pair(pair) = arena[variant.type_id] else {
            remaining.push(variant);
            continue;
        };

        let rhs_pairs = rhs_pairs.get_or_insert_with(|| pairs_of(arena, rhs_id));

        match split_pair(arena, builtins, pair, rhs_pairs, stack) {
            Split::Unchanged => {}
            Split::Removed => {
                changed = true;
                continue;
            }
            Split::Narrowed(type_id) => {
                changed = true;
                variant.type_id = type_id;
            }
        }

        remaining.push(variant);
    }

    stack.remove(&(lhs_id, rhs_id));

    changed.then_some(remaining)
}

enum Split {
    Unchanged,
    Removed,
    Narrowed(TypeId),
}

/// Subtracts each of the pairs in the target from the pair, one at a time, where only one of the
/// parts differs.
fn split_pair(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    pair: Pair,
    rhs_pairs: &[Pair],
    stack: &mut HashSet<(TypeId, TypeId)>,
) -> Split {
    let mut first = pair.first;
    let mut rest = pair.rest;
    let mut changed = false;

    for rhs in rhs_pairs {
        let first_covered = compare(arena, builtins, first, rhs.first) <= Comparison::Cast;
        let rest_covered = compare(arena, builtins, rest, rhs.rest) <= Comparison::Cast;

        let (part, rhs_part) = match (first_covered, rest_covered) {
            (true, true) => return Split::Removed,
            (false, true) => (&mut first, rhs.first),
            (true, false) => (&mut rest, rhs.rest),
            (false, false) => continue,
        };

        let Some(variants) = remaining_variants(arena, builtins, *part, rhs_part, stack) else {
            continue;
        };

        if variants.is_empty() {
            return Split::Removed;
        }

        *part = repackage(arena, builtins, variants);
        changed = true;
    }

    if changed {
        Split::Narrowed(arena.alloc(Type::Pair(Pair::new(first, rest))))
    } else {
        Split::Unchanged
    }
}

fn repackage(arena: &mut Arena<Type>, builtins: &BuiltinTypes, variants: Vec<Variant>) -> TypeId {
    grow_stack(|| repackage_inner(arena, builtins, variants))
}

fn repackage_inner(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    variants: Vec<Variant>,
) -> TypeId {
    let mut collected = vec![];
    let mut semantic_type_id = None;
    let mut leftover = vec![];

    for mut variant in variants {
        if let Some(id) = variant.semantic_type_ids.last().copied() {
            if let Some(semantic_type_id) = semantic_type_id {
                if id == semantic_type_id {
                    variant.semantic_type_ids.pop().unwrap();
                    collected.push(variant);
                } else {
                    leftover.push(variant);
                }
            } else {
                semantic_type_id = Some(id);
                variant.semantic_type_ids.pop().unwrap();
                collected.push(variant);
            }
        } else {
            leftover.push(variant);
        }
    }

    if collected.is_empty() {
        let leftover: Vec<TypeId> = leftover
            .into_iter()
            .map(|variant| variant.type_id)
            .collect();

        if leftover.is_empty() {
            return arena.alloc(Type::Never);
        } else if leftover.len() == 1 {
            return leftover[0];
        }

        return arena.alloc(Type::Union(Union::new(leftover)));
    }

    let semantic_type_id = semantic_type_id.unwrap();

    let inner = repackage(arena, builtins, collected);

    let result = match arena[semantic_type_id].clone() {
        Type::Alias(alias) => arena.alloc(Type::Alias(Alias { inner, ..alias })),
        Type::Struct(ty) => {
            let ty = ty.narrow(arena, builtins, inner);
            arena.alloc(Type::Struct(ty))
        }
        _ => unreachable!(),
    };

    if leftover.is_empty() {
        return result;
    }

    let leftover = repackage(arena, builtins, leftover);

    arena.alloc(Type::Union(Union::new(vec![result, leftover])))
}

struct Variant {
    semantic_type_ids: Vec<TypeId>,
    type_id: TypeId,
}

fn variants_of(arena: &Arena<Type>, builtins: &BuiltinTypes, id: TypeId) -> Vec<Variant> {
    grow_stack(|| variants_of_inner(arena, builtins, id))
}

fn variants_of_inner(arena: &Arena<Type>, builtins: &BuiltinTypes, id: TypeId) -> Vec<Variant> {
    match arena[id].clone() {
        Type::Apply(_) => unreachable!(),
        Type::Ref(id) => variants_of(arena, builtins, id),
        // Nothing is known about the values of a generic type, so it can't be split into narrower
        // variants. It's kept as is, and only removed if the target covers every possible value.
        Type::Unresolved | Type::Atom(_) | Type::Pair(_) | Type::Generic(_) => vec![Variant {
            semantic_type_ids: vec![],
            type_id: id,
        }],
        Type::Never => vec![],
        Type::Alias(alias) => {
            let mut variants = variants_of(arena, builtins, alias.inner);

            for variant in &mut variants {
                variant.semantic_type_ids.push(id);
            }

            variants
        }
        Type::Struct(ty) => {
            let mut variants = variants_of(arena, builtins, ty.inner);

            for variant in &mut variants {
                variant.semantic_type_ids.push(id);
            }

            variants
        }
        Type::Function(_) | Type::Any => vec![
            Variant {
                semantic_type_ids: vec![],
                type_id: builtins.atom,
            },
            Variant {
                semantic_type_ids: vec![],
                type_id: builtins.recursive_any_pair,
            },
        ],
        Type::Union(ty) => {
            let mut variants = Vec::new();

            for variant in ty.types {
                variants.extend(variants_of(arena, builtins, variant));
            }

            variants
        }
    }
}
