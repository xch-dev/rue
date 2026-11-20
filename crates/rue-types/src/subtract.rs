use id_arena::Arena;
use indexmap::IndexMap;
use log::{Level, log_enabled, trace};

use crate::{
    Alias, BuiltinTypes, Comparison, ComparisonContext, FxHashMap, FxHashSet, IntersectContext,
    Pair, Type, TypeId, Union, compare_impl, extract_pairs_substituted, grow_stack,
    intersect_substituted, reaches_non_contractive_cycle, step, stringify_impl, substitute,
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
    if reaches_non_contractive_cycle(arena, lhs_id) || reaches_non_contractive_cycle(arena, rhs_id)
    {
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
    stack: FxHashSet<(TypeId, TypeId)>,
    /// What's left of each variant once a target is subtracted from it. The same variant often
    /// appears many times, such as in a union that repeats a type at each level (#496), and each
    /// copy should be narrowed to the same type. The same subtraction can also be reached through
    /// either part of the pairs in a target, so this keeps nested splits from being exponential.
    variants: FxHashMap<(TypeId, TypeId), Outcome>,
    /// Shared by every comparison, since they all start from an empty stack. Otherwise each part
    /// of a nested split would compare the whole of the types it's given again.
    comparison: ComparisonContext<'static>,
    /// Shared by every intersection, for the same reason.
    intersection: IntersectContext,
}

/// Whether every value of `lhs_id` passes a check against `rhs_id`. The types have already been
/// substituted, so they can be compared directly.
fn covered(
    arena: &Arena<Type>,
    builtins: &BuiltinTypes,
    cx: &mut Subtraction,
    lhs_id: TypeId,
    rhs_id: TypeId,
) -> bool {
    compare_impl(
        arena,
        builtins,
        &mut cx.comparison,
        lhs_id,
        rhs_id,
        None,
        None,
    ) <= Comparison::Cast
}

/// What's left of a variant once a target is subtracted from it.
#[derive(Debug, Clone)]
enum Outcome {
    Unchanged,
    Removed,
    /// The variant is replaced by these types, each with the same semantic types as the variant.
    Narrowed(Vec<TypeId>),
}

/// What's left of a part of a pair once a part of a target pair is subtracted from it.
#[derive(Debug, Clone, Copy)]
enum Part {
    Unchanged,
    Removed,
    Narrowed(TypeId),
}

/// The most pairs that a pair variant can be split into. Splitting by a target pair whose parts
/// both differ adds a pair, so a target with many such pairs would otherwise multiply the variants
/// of the type. Past this, the remaining pairs are kept whole, which is only wider than it could be.
const MAX_SPLIT_PAIRS: usize = 16;

/// The variants of `lhs_id` that aren't covered by `rhs_id`, or `None` if none of them changed.
///
/// A pair variant that isn't covered as a whole is split by the pairs in the target (see
/// `split_pair`).
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

    for variant in variants {
        let key = (variant.type_id, rhs_id);

        let outcome = if let Some(outcome) = cx.variants.get(&key) {
            outcome.clone()
        } else {
            let outcome = if covered(arena, builtins, cx, variant.type_id, rhs_id) {
                Outcome::Removed
            } else if let Type::Pair(pair) = arena[variant.type_id] {
                let rhs_pairs =
                    rhs_pairs.get_or_insert_with(|| extract_pairs_substituted(arena, rhs_id));
                split_pair(arena, builtins, cx, pair, rhs_pairs)
            } else {
                Outcome::Unchanged
            };

            cx.variants.insert(key, outcome.clone());
            outcome
        };

        match outcome {
            Outcome::Unchanged => remaining.push(variant),
            Outcome::Removed => changed = true,
            Outcome::Narrowed(type_ids) => {
                changed = true;

                for type_id in type_ids {
                    remaining.push(Variant {
                        semantic_type_ids: variant.semantic_type_ids.clone(),
                        type_id,
                    });
                }
            }
        }
    }

    cx.stack.remove(&(lhs_id, rhs_id));

    changed.then_some(remaining)
}

/// Subtracts each of the pairs in the target from the pair, one at a time.
///
/// If one part of a target pair covers that part of the pair, only the other part is split:
/// `(X, Y) - (P, Y) = (X - P, Y)`, since the values that fail the check against `(P, Y)` are
/// exactly the ones whose first value isn't a `P`. If neither part is covered, the pair is split in
/// two: `(X, Y) - (P, Q) = (X - P, Y) | (X & P, Y - Q)`, where `X & P` is the intersection. A value
/// fails the check if its first value isn't a `P`, or if it is but its rest isn't a `Q`.
fn split_pair(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    cx: &mut Subtraction,
    pair: Pair,
    rhs_pairs: &[Pair],
) -> Outcome {
    let mut pairs = vec![(pair.first, pair.rest)];
    let mut changed = false;

    for rhs in rhs_pairs {
        let mut next = Vec::with_capacity(pairs.len());

        for (index, &(first, rest)) in pairs.iter().enumerate() {
            let first_covered = covered(arena, builtins, cx, first, rhs.first);
            let rest_covered = covered(arena, builtins, cx, rest, rhs.rest);

            match (first_covered, rest_covered) {
                (true, true) => changed = true,
                (false, true) => match subtract_part(arena, builtins, cx, first, rhs.first) {
                    Part::Unchanged => next.push((first, rest)),
                    Part::Removed => changed = true,
                    Part::Narrowed(first) => {
                        changed = true;
                        next.push((first, rest));
                    }
                },
                (true, false) => match subtract_part(arena, builtins, cx, rest, rhs.rest) {
                    Part::Unchanged => next.push((first, rest)),
                    Part::Removed => changed = true,
                    Part::Narrowed(rest) => {
                        changed = true;
                        next.push((first, rest));
                    }
                },
                (false, false) => {
                    // Each pair that's left to split may still be split in two
                    if next.len() + (pairs.len() - index) >= MAX_SPLIT_PAIRS {
                        next.push((first, rest));
                        continue;
                    }

                    let first_left = subtract_part(arena, builtins, cx, first, rhs.first);
                    let rest_left = subtract_part(arena, builtins, cx, rest, rhs.rest);

                    // If either part is unchanged by its subtraction, the two pairs would cover
                    // the whole pair again
                    if matches!(first_left, Part::Unchanged) || matches!(rest_left, Part::Unchanged)
                    {
                        next.push((first, rest));
                        continue;
                    }

                    changed = true;

                    if let Part::Narrowed(first_left) = first_left {
                        next.push((first_left, rest));
                    }

                    if let Part::Narrowed(rest_left) = rest_left {
                        // If every first value is a `P`, the intersection is the first part itself
                        let both = if matches!(first_left, Part::Removed) {
                            first
                        } else {
                            intersect_substituted(
                                arena,
                                builtins,
                                &mut cx.intersection,
                                first,
                                rhs.first,
                            )
                        };

                        next.push((both, rest_left));
                    }
                }
            }
        }

        pairs = next;
    }

    if pairs.is_empty() {
        Outcome::Removed
    } else if changed {
        Outcome::Narrowed(
            pairs
                .into_iter()
                .map(|(first, rest)| arena.alloc(Type::Pair(Pair::new(first, rest))))
                .collect(),
        )
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
) -> Part {
    if cx.stack.contains(&(lhs_id, rhs_id)) {
        return Part::Unchanged;
    }

    match remaining_variants(arena, builtins, cx, lhs_id, rhs_id) {
        None => Part::Unchanged,
        Some(variants) if variants.is_empty() => Part::Removed,
        Some(variants) => Part::Narrowed(repackage(arena, builtins, variants)),
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
