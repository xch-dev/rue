use std::collections::{HashMap, HashSet};

use id_arena::Arena;
use indexmap::IndexMap;
use log::{Level, log_enabled, trace};

use crate::{
    Alias, BuiltinTypes, Comparison, Pair, Type, TypeId, Union, compare, extract_pairs_substituted,
    grow_stack, reaches_non_contractive_cycle, step, stringify_impl, substitute,
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
    let mut cx = Subtraction::default();

    match remaining_variants(arena, builtins, &mut cx, lhs_id, rhs_id) {
        Some(variants) => repackage(arena, builtins, variants),
        None => lhs_id,
    }
}

/// The state of one subtraction, which is shared by the subtractions of the parts of its pairs.
#[derive(Default)]
struct Subtraction {
    /// The subtractions in progress, to stop at a cycle through the parts of recursive types.
    stack: HashSet<(TypeId, TypeId)>,
    /// What's left of each variant once a target is subtracted from it. The same variant often
    /// appears many times, such as in a union that repeats a type at each level (#496), and each
    /// copy should be narrowed to the same type.
    variants: HashMap<(TypeId, TypeId), Outcome>,
    /// What's left of each part of a pair once a part of a target pair is subtracted from it. The
    /// same subtraction can be reached through either part of the pairs in a target.
    parts: HashMap<(TypeId, TypeId), Outcome>,
}

#[derive(Debug, Clone, Copy)]
enum Outcome {
    Unchanged,
    Removed,
    Narrowed(TypeId),
}

/// The variants of `lhs_id` that aren't covered by `rhs_id`, or `None` if none of them changed.
///
/// A pair variant whose parts aren't all covered is split by the one part that differs, if the
/// other is covered: `(X, Y) - (P, Y) = (X - P, Y)`. The values that fail the check against
/// `(P, Y)` are exactly the ones whose first value isn't a `P`. When both parts differ, the
/// remainder isn't a single pair, so the variant is kept whole.
fn remaining_variants(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    cx: &mut Subtraction,
    lhs_id: TypeId,
    rhs_id: TypeId,
) -> Option<Vec<Variant>> {
    grow_stack(|| remaining_variants_inner(arena, builtins, cx, lhs_id, rhs_id))
}

fn remaining_variants_inner(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    cx: &mut Subtraction,
    lhs_id: TypeId,
    rhs_id: TypeId,
) -> Option<Vec<Variant>> {
    step();

    cx.stack.insert((lhs_id, rhs_id));

    let variants = variants_of(arena, builtins, lhs_id);
    let mut rhs_pairs = None;
    let mut changed = false;
    let mut remaining = Vec::with_capacity(variants.len());

    for mut variant in variants {
        let key = (variant.type_id, rhs_id);

        let outcome = if let Some(outcome) = cx.variants.get(&key) {
            *outcome
        } else {
            let outcome = if compare(arena, builtins, variant.type_id, rhs_id) <= Comparison::Cast {
                Outcome::Removed
            } else if let Type::Pair(pair) = arena[variant.type_id] {
                let rhs_pairs =
                    rhs_pairs.get_or_insert_with(|| extract_pairs_substituted(arena, rhs_id));
                split_pair(arena, builtins, cx, pair, rhs_pairs)
            } else {
                Outcome::Unchanged
            };

            cx.variants.insert(key, outcome);
            outcome
        };

        match outcome {
            Outcome::Unchanged => {}
            Outcome::Removed => {
                changed = true;
                continue;
            }
            Outcome::Narrowed(type_id) => {
                changed = true;
                variant.type_id = type_id;
            }
        }

        remaining.push(variant);
    }

    cx.stack.remove(&(lhs_id, rhs_id));

    changed.then_some(remaining)
}

/// Subtracts each of the pairs in the target from the pair, one at a time, where only one of the
/// parts differs.
fn split_pair(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    cx: &mut Subtraction,
    pair: Pair,
    rhs_pairs: &[Pair],
) -> Outcome {
    let mut first = pair.first;
    let mut rest = pair.rest;
    let mut changed = false;

    for rhs in rhs_pairs {
        let first_covered = compare(arena, builtins, first, rhs.first) <= Comparison::Cast;
        let rest_covered = compare(arena, builtins, rest, rhs.rest) <= Comparison::Cast;

        let (part, rhs_part) = match (first_covered, rest_covered) {
            (true, true) => return Outcome::Removed,
            (false, true) => (&mut first, rhs.first),
            (true, false) => (&mut rest, rhs.rest),
            (false, false) => continue,
        };

        match subtract_part(arena, builtins, cx, *part, rhs_part) {
            Outcome::Unchanged => {}
            Outcome::Removed => return Outcome::Removed,
            Outcome::Narrowed(type_id) => {
                *part = type_id;
                changed = true;
            }
        }
    }

    if changed {
        Outcome::Narrowed(arena.alloc(Type::Pair(Pair::new(first, rest))))
    } else {
        Outcome::Unchanged
    }
}

/// Subtracts a part of a target pair from a part of a pair.
///
/// The parts of a recursive type can lead back to a subtraction that's already in progress, such
/// as `L - R` with `type L = 2 | (1, L)` and `type R = 1 | (1, R)`. That part is kept whole, so the
/// result is only wider than it could be.
fn subtract_part(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    cx: &mut Subtraction,
    lhs_id: TypeId,
    rhs_id: TypeId,
) -> Outcome {
    let key = (lhs_id, rhs_id);

    if let Some(outcome) = cx.parts.get(&key) {
        return *outcome;
    }

    if cx.stack.contains(&key) {
        return Outcome::Unchanged;
    }

    let outcome = match remaining_variants(arena, builtins, cx, lhs_id, rhs_id) {
        None => Outcome::Unchanged,
        Some(variants) if variants.is_empty() => Outcome::Removed,
        Some(variants) => Outcome::Narrowed(repackage(arena, builtins, variants)),
    };

    cx.parts.insert(key, outcome);
    outcome
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
