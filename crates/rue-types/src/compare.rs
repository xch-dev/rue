use std::{
    cmp::{max, min},
    collections::{HashMap, HashSet},
};

use id_arena::Arena;
use indexmap::{IndexMap, IndexSet};
use log::{Level, debug, log_enabled, trace};

use crate::{
    AtomRestriction, AtomSemantic, BuiltinTypes, Memo, Type, TypeId, Union, stringify_impl,
    substitute,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Comparison {
    Assign,
    Cast,
    Invalid,
}

/// The constraints collected for a generic type parameter during inference.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct InferenceCandidates {
    /// Types that flowed into the generic from a covariant position, so the generic must be able
    /// to hold all of them. For example, passing an `Int` to a parameter of type `T`.
    pub lower: Vec<TypeId>,

    /// Types that the generic flows into from a contravariant position (a function parameter),
    /// so the generic must be assignable to all of them. For example, passing a
    /// `fn(x: Bytes32) -> Bytes32` to a parameter of type `fn(x: T) -> Bytes32`.
    pub upper: Vec<TypeId>,
}

/// Tracks the candidates for the generic type parameters being inferred. Only the generics that
/// the inference was created with are inferred, so generics that are bound elsewhere (such as by
/// a generic function type in a parameter, or by an enclosing function) are left rigid.
#[derive(Debug, Default, Clone)]
pub struct Inference {
    generics: IndexMap<TypeId, InferenceCandidates>,
}

impl Inference {
    pub fn new(generics: impl IntoIterator<Item = TypeId>) -> Self {
        Self {
            generics: generics
                .into_iter()
                .map(|generic| (generic, InferenceCandidates::default()))
                .collect(),
        }
    }

    pub fn candidates(&self, generic: TypeId) -> Option<&InferenceCandidates> {
        self.generics.get(&generic)
    }

    fn is_target(&self, generic: TypeId) -> bool {
        self.generics.contains_key(&generic)
    }

    fn candidates_mut(&mut self, generic: TypeId) -> &mut InferenceCandidates {
        self.generics.get_mut(&generic).unwrap()
    }
}

type ComparisonKey = (TypeId, TypeId, bool);

#[derive(Debug, Default)]
pub(crate) struct ComparisonContext<'a> {
    infer: Option<&'a mut Inference>,
    stack: IndexSet<ComparisonKey>,
    /// The indices in `stack` of comparisons between pairs or functions. A cycle is only
    /// coinductively assignable if it passes through one of these, since otherwise the
    /// recursion isn't bounded by the structure of the value.
    guards: Vec<usize>,
    /// Whether the comparison has been flipped an odd number of times by function parameters.
    /// When this is set, the generics being inferred are on the left hand side rather than the
    /// right hand side.
    flipped: bool,
    /// The generics of left hand side functions that are being instantiated, since a generic
    /// function is assignable to a function type if it can be instantiated to it. Each generic
    /// maps to the value of `flipped` in which it's on the right hand side, and the bounds that
    /// have been collected for it. The bounds are checked against each other afterward.
    instances: IndexMap<TypeId, (bool, InferenceCandidates)>,
    /// The number of inference candidates that have been collected.
    candidates: usize,
    /// Completed comparisons, keyed by the comparison and the semantics passed down to it.
    memo: Memo<ComparisonKey, (ComparisonKey, Option<TypeId>, Option<TypeId>), Comparison>,
}

impl ComparisonContext<'_> {
    fn is_target(&self, generic: TypeId) -> bool {
        self.infer
            .as_ref()
            .is_some_and(|infer| infer.is_target(generic))
    }
}

pub fn compare_with_inference(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    lhs: TypeId,
    rhs: TypeId,
    infer: Option<&mut Inference>,
) -> Comparison {
    let lhs = substitute(arena, lhs);
    let rhs = substitute(arena, rhs);
    let names = log_enabled!(Level::Trace).then(|| {
        (
            stringify_impl(arena, lhs, &mut IndexMap::new()),
            stringify_impl(arena, rhs, &mut IndexMap::new()),
        )
    });

    if let Some((lhs_name, rhs_name)) = &names {
        trace!("Comparing {lhs_name} to {rhs_name}");
    }

    let result = compare_impl(
        arena,
        builtins,
        &mut ComparisonContext {
            infer,
            ..Default::default()
        },
        lhs,
        rhs,
        None,
        None,
    );

    if let Some((lhs_name, rhs_name)) = &names {
        trace!("Comparison from {lhs_name} to {rhs_name} yielded {result:?}");
    }

    result
}

pub fn compare(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    lhs: TypeId,
    rhs: TypeId,
) -> Comparison {
    compare_with_inference(arena, builtins, lhs, rhs, None)
}

/// Picks a type for each inferred generic, given the candidates collected while comparing.
///
/// Inference only proposes types, it doesn't guarantee that they satisfy every constraint. The
/// caller must check the values against the types substituted with these mappings afterward.
pub fn resolve_inference(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    infer: &Inference,
) -> HashMap<TypeId, TypeId> {
    let mut mappings = HashMap::new();

    for (&generic, candidates) in &infer.generics {
        let ty = if !candidates.lower.is_empty() {
            // Literals are widened to their base type, so that `pick(1, 2)` infers `Int` and a
            // lambda whose parameter type comes from `T` can accept more than the one literal.
            // This is only done if the widened type still satisfies every upper bound, since
            // otherwise something like `fn(x: 5) -> Int` couldn't be passed to `fn(x: T) -> Int`.
            let widened: Vec<TypeId> = candidates
                .lower
                .iter()
                .map(|&ty| widen_literal(arena, builtins, ty))
                .collect();
            let widened = union_of(arena, widened);

            if candidates
                .upper
                .iter()
                .all(|&upper| compare(arena, builtins, widened, upper) == Comparison::Assign)
            {
                widened
            } else {
                union_of(arena, candidates.lower.clone())
            }
        } else if let Some(&first) = candidates.upper.first() {
            // Pick the most specific upper bound, if there is one
            candidates
                .upper
                .iter()
                .copied()
                .find(|&ty| {
                    candidates
                        .upper
                        .iter()
                        .all(|&other| compare(arena, builtins, ty, other) == Comparison::Assign)
                })
                .unwrap_or(first)
        } else {
            // Nothing constrains the generic, so leave it as is
            continue;
        };

        mappings.insert(generic, ty);
    }

    mappings
}

fn union_of(arena: &mut Arena<Type>, types: impl IntoIterator<Item = TypeId>) -> TypeId {
    let types: IndexSet<TypeId> = types.into_iter().collect();

    if types.len() == 1 {
        types[0]
    } else {
        arena.alloc(Type::Union(Union::new(types.into_iter().collect())))
    }
}

/// Replaces a literal atom type with the type it's a value of, such as `1` with `Int`.
fn widen_literal(arena: &mut Arena<Type>, builtins: &BuiltinTypes, ty: TypeId) -> TypeId {
    let Type::Atom(atom) = &arena[ty] else {
        return ty;
    };

    let Some(AtomRestriction::Value(value)) = &atom.restriction else {
        return ty;
    };

    let widened = match (atom.semantic, value.len()) {
        // `nil` is kept as is, since it's commonly used to represent the absence of a value
        // (for example, `T | nil`), and widening it to `Bytes` would lose that
        (AtomSemantic::Bytes, 0) => return ty,
        (AtomSemantic::Bytes, 32) => builtins.bytes32,
        (AtomSemantic::Bytes, _) => builtins.bytes,
        (AtomSemantic::String, _) => builtins.string,
        (AtomSemantic::Int, _) => builtins.int,
        (AtomSemantic::Bool, _) => builtins.bool,
        (AtomSemantic::PublicKey, 48) => builtins.public_key,
        (AtomSemantic::Signature, 96) => builtins.signature,
        _ => return ty,
    };

    if compare(arena, builtins, ty, widened) == Comparison::Assign {
        widened
    } else {
        ty
    }
}

pub(crate) fn compare_impl(
    arena: &Arena<Type>,
    builtins: &BuiltinTypes,
    ctx: &mut ComparisonContext<'_>,
    lhs: TypeId,
    rhs: TypeId,
    lhs_semantic: Option<TypeId>,
    rhs_semantic: Option<TypeId>,
) -> Comparison {
    let key = (lhs, rhs, ctx.flipped);

    if let Some(index) = ctx.stack.get_index_of(&key) {
        ctx.memo.reached(index);

        // Revisiting a comparison is only sound to treat as assignable if we've passed through
        // a pair or function since, otherwise a non-contractive type like `type A = B; type B = A;`
        // would be assignable to anything. Those should be rejected before reaching this point.
        return if ctx.guards.last().is_some_and(|&guard| guard >= index) {
            Comparison::Assign
        } else {
            Comparison::Invalid
        };
    }

    // Generics that are being instantiated are compared differently and collect bounds as a side
    // effect, so results are only memoized while there aren't any
    let memo_key = ctx
        .instances
        .is_empty()
        .then_some((key, lhs_semantic, rhs_semantic));

    if let Some(memo_key) = &memo_key
        && let Some(result) = ctx.memo.get(memo_key, ctx.stack.iter().copied())
    {
        return result;
    }

    let frame = ctx.memo.enter(ctx.stack.len());
    let candidates = ctx.candidates;

    ctx.stack.insert(key);

    let result = match (arena[lhs].clone(), arena[rhs].clone()) {
        (Type::Apply(_), _) | (_, Type::Apply(_)) => unreachable!(),
        (Type::Ref(lhs), _) => {
            compare_impl(arena, builtins, ctx, lhs, rhs, lhs_semantic, rhs_semantic)
        }
        (_, Type::Ref(rhs)) => {
            compare_impl(arena, builtins, ctx, lhs, rhs, lhs_semantic, rhs_semantic)
        }
        (Type::Unresolved, _) | (_, Type::Unresolved) => Comparison::Assign,
        (Type::Never, _) => Comparison::Assign,
        (_, Type::Any) => Comparison::Assign,
        (Type::Generic(_), _)
            if ctx
                .instances
                .get(&lhs)
                .is_some_and(|&(flipped, _)| flipped != ctx.flipped) =>
        {
            ctx.instances[&lhs].1.upper.push(rhs);
            Comparison::Assign
        }
        (_, Type::Generic(_))
            if ctx
                .instances
                .get(&rhs)
                .is_some_and(|&(flipped, _)| flipped == ctx.flipped) =>
        {
            ctx.instances[&rhs].1.lower.push(lhs);
            Comparison::Assign
        }
        (Type::Atom(lhs), Type::Atom(rhs)) => {
            let semantic = if lhs.semantic == rhs.semantic
                || rhs.semantic == AtomSemantic::Any
                || (lhs.semantic == AtomSemantic::String && rhs.semantic == AtomSemantic::Bytes)
                || (lhs.semantic == AtomSemantic::Bytes && rhs.semantic == AtomSemantic::String)
            {
                Comparison::Assign
            } else {
                Comparison::Cast
            };

            let restriction = match (lhs.restriction, rhs.restriction) {
                (_, None) => Comparison::Assign,
                (None, _) => Comparison::Invalid,
                (Some(AtomRestriction::Length(lhs)), Some(AtomRestriction::Length(rhs))) => {
                    if lhs == rhs {
                        Comparison::Assign
                    } else {
                        Comparison::Invalid
                    }
                }
                (Some(AtomRestriction::Value(lhs)), Some(AtomRestriction::Value(rhs))) => {
                    if lhs == rhs {
                        Comparison::Assign
                    } else {
                        Comparison::Invalid
                    }
                }
                (Some(AtomRestriction::Length(_)), Some(AtomRestriction::Value(_))) => {
                    Comparison::Invalid
                }
                (Some(AtomRestriction::Value(lhs)), Some(AtomRestriction::Length(rhs))) => {
                    if lhs.len() == rhs {
                        Comparison::Assign
                    } else {
                        Comparison::Invalid
                    }
                }
            };

            max(semantic, restriction)
        }
        (Type::Pair(lhs), Type::Pair(rhs)) => {
            ctx.guards.push(ctx.stack.len() - 1);
            let first = compare_impl(arena, builtins, ctx, lhs.first, rhs.first, None, None);
            let rest = compare_impl(arena, builtins, ctx, lhs.rest, rhs.rest, None, None);
            ctx.guards.pop().unwrap();
            max(first, rest)
        }
        (Type::Atom(_), Type::Pair(_)) => Comparison::Invalid,
        (Type::Pair(_), Type::Atom(_)) => Comparison::Invalid,
        (Type::Function(lhs), Type::Function(rhs)) => {
            // TODO: We could relax the identical parameter requirement

            if lhs.nil_terminated != rhs.nil_terminated || lhs.params.len() != rhs.params.len() {
                Comparison::Invalid
            } else {
                ctx.guards.push(ctx.stack.len() - 1);

                // The left hand side's generics are instantiated with whatever the right hand
                // side needs, whereas the right hand side's generics stay rigid, since the
                // function must work for any of them. Comparing two generic functions this way
                // makes them equivalent up to renaming of their generics.
                let previous: Vec<_> = lhs
                    .generics
                    .iter()
                    .map(|&generic| {
                        let instance = (!ctx.flipped, InferenceCandidates::default());
                        (generic, ctx.instances.insert(generic, instance))
                    })
                    .collect();

                // The return type is covariant, since the value returned by the function flows
                // out to whoever expects the right hand side's return type.
                let mut result = compare_impl(arena, builtins, ctx, lhs.ret, rhs.ret, None, None);

                // The parameters are contravariant, since the arguments passed according to the
                // right hand side's parameter types flow into the left hand side's function.
                // A `Cast` is still fine here, since casting a function's type just reinterprets
                // its arguments (which requires the same representation) at the call site.
                ctx.flipped = !ctx.flipped;

                for (i, param) in lhs.params.values().enumerate() {
                    result = max(
                        result,
                        compare_impl(arena, builtins, ctx, rhs.params[i], *param, None, None),
                    );
                }

                ctx.flipped = !ctx.flipped;

                let mut bounds = Vec::new();

                for (generic, previous) in previous.into_iter().rev() {
                    let instance = if let Some(previous) = previous {
                        ctx.instances.insert(generic, previous)
                    } else {
                        ctx.instances.shift_remove(&generic)
                    };
                    bounds.push(instance.unwrap().1);
                }

                // There is an instantiation of each generic if every type that flows into it is
                // assignable to every type it flows into (such as their union, or `Never`).
                // The bounds come from the other side, where the generics of any enclosing
                // functions being instantiated are rigid, so they're hidden while checking.
                let instances = std::mem::take(&mut ctx.instances);

                for candidates in bounds {
                    for &lower in &candidates.lower {
                        for &upper in &candidates.upper {
                            result = max(
                                result,
                                compare_impl(arena, builtins, ctx, lower, upper, None, None),
                            );
                        }
                    }
                }

                ctx.instances = instances;
                ctx.guards.pop().unwrap();

                result
            }
        }
        (Type::Generic(_), Type::Generic(_)) if lhs == rhs => Comparison::Assign,
        (Type::Generic(_), _) if ctx.flipped && ctx.is_target(lhs) => {
            debug!(
                "Inferring {} must be assignable to {}",
                stringify_impl(arena, lhs, &mut IndexMap::new()),
                stringify_impl(arena, rhs, &mut IndexMap::new())
            );
            let infer = ctx.infer.as_mut().unwrap();
            infer.candidates_mut(lhs).upper.push(rhs);
            ctx.candidates += 1;
            Comparison::Assign
        }
        (_, Type::Generic(_)) if !ctx.flipped && ctx.is_target(rhs) => {
            debug!(
                "Inferring {} could include {}",
                stringify_impl(arena, rhs, &mut IndexMap::new()),
                stringify_impl(arena, lhs, &mut IndexMap::new())
            );
            let infer = ctx.infer.as_mut().unwrap();
            infer.candidates_mut(rhs).lower.push(lhs);
            ctx.candidates += 1;
            Comparison::Assign
        }
        (_, Type::Generic(_)) => match arena[lhs].clone() {
            Type::Union(lhs) => {
                let mut result = Comparison::Assign;

                for &id in &lhs.types {
                    result = max(
                        result,
                        compare_impl(arena, builtins, ctx, id, rhs, lhs_semantic, rhs_semantic),
                    );
                }

                result
            }
            Type::Alias(lhs) => compare_impl(
                arena,
                builtins,
                ctx,
                lhs.inner,
                rhs,
                lhs_semantic,
                rhs_semantic,
            ),
            _ => Comparison::Invalid,
        },
        (Type::Struct(lhs), Type::Struct(rhs)) => max(
            compare_impl(arena, builtins, ctx, lhs.inner, rhs.inner, None, None),
            if lhs.semantic == rhs.semantic {
                Comparison::Assign
            } else {
                Comparison::Cast
            },
        ),
        (Type::Struct(lhs), _) => {
            let inner = compare_impl(
                arena,
                builtins,
                ctx,
                lhs.inner,
                rhs,
                Some(lhs.semantic),
                rhs_semantic,
            );

            let rhs_semantics = semantics_of(arena, rhs);

            if rhs_semantic == Some(lhs.semantic)
                || rhs_semantics.contains(&Some(Semantic::Id(lhs.semantic)))
                || rhs_semantics.contains(&Some(Semantic::All))
            {
                inner
            } else {
                max(inner, Comparison::Cast)
            }
        }
        (_, Type::Struct(rhs)) => {
            let inner = compare_impl(
                arena,
                builtins,
                ctx,
                lhs,
                rhs.inner,
                lhs_semantic,
                Some(rhs.semantic),
            );

            let semantics = semantics_of(arena, lhs);

            if (semantics.len() != 1
                || (!semantics.contains(&Some(Semantic::Id(rhs.semantic)))
                    && !semantics.contains(&Some(Semantic::All))))
                && lhs_semantic != Some(rhs.semantic)
            {
                max(inner, Comparison::Cast)
            } else {
                inner
            }
        }
        (Type::Alias(lhs), _) => compare_impl(
            arena,
            builtins,
            ctx,
            lhs.inner,
            rhs,
            lhs_semantic,
            rhs_semantic,
        ),
        (_, Type::Alias(rhs)) => compare_impl(
            arena,
            builtins,
            ctx,
            lhs,
            rhs.inner,
            lhs_semantic,
            rhs_semantic,
        ),
        (Type::Generic(_), Type::Union(rhs_union)) => {
            // A generic is opaque, so it can only be assigned to a union if it's assignable to
            // one of the members (such as `T` to `T | nil`). Otherwise, fall back to whatever
            // the generic could be at runtime, as with any other type.
            let mut result = Comparison::Invalid;

            for &id in &rhs_union.types {
                result = min(
                    result,
                    compare_impl(arena, builtins, ctx, lhs, id, lhs_semantic, rhs_semantic),
                );
            }

            if result == Comparison::Assign {
                result
            } else {
                min(
                    result,
                    compare_impl(
                        arena,
                        builtins,
                        ctx,
                        builtins.recursive_any,
                        rhs,
                        lhs_semantic,
                        rhs_semantic,
                    ),
                )
            }
        }
        (Type::Generic(_) | Type::Any, _) => compare_impl(
            arena,
            builtins,
            ctx,
            builtins.recursive_any,
            rhs,
            lhs_semantic,
            rhs_semantic,
        ),
        (Type::Function(_), _) => {
            let result = if let Type::Union(rhs) = arena[rhs].clone() {
                let mut result = Comparison::Invalid;

                for &id in &rhs.types {
                    result = min(
                        result,
                        compare_impl(arena, builtins, ctx, lhs, id, lhs_semantic, rhs_semantic),
                    );
                }

                result
            } else {
                Comparison::Invalid
            };

            min(
                result,
                compare_impl(
                    arena,
                    builtins,
                    ctx,
                    builtins.recursive_any,
                    rhs,
                    lhs_semantic,
                    rhs_semantic,
                ),
            )
        }
        (Type::Union(lhs), _) => {
            let mut result = Comparison::Assign;

            for &id in &lhs.types {
                result = max(
                    result,
                    compare_impl(arena, builtins, ctx, id, rhs, lhs_semantic, rhs_semantic),
                );
            }

            result
        }
        (_, Type::Union(rhs)) => {
            let mut result = Comparison::Invalid;

            for &id in &rhs.types {
                result = min(
                    result,
                    compare_impl(arena, builtins, ctx, lhs, id, lhs_semantic, rhs_semantic),
                );
            }

            result
        }
        (_, Type::Never) => Comparison::Invalid,
        (_, Type::Function(_)) => Comparison::Invalid,
    };

    ctx.stack.pop().unwrap();

    // Inference candidates are collected as a side effect of comparing, so a comparison that
    // collected any can't be skipped
    let memo_key = memo_key.filter(|_| ctx.candidates == candidates);
    ctx.memo.exit(frame, key, memo_key, result);

    result
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Semantic {
    All,
    Id(TypeId),
}

fn semantics_of(arena: &Arena<Type>, id: TypeId) -> HashSet<Option<Semantic>> {
    semantics_of_impl(arena, id, &mut HashSet::new())
}

fn semantics_of_impl(
    arena: &Arena<Type>,
    id: TypeId,
    visited: &mut HashSet<TypeId>,
) -> HashSet<Option<Semantic>> {
    // Guard against non-contractive cycles of aliases and unions
    if !visited.insert(id) {
        return HashSet::new();
    }

    match arena[id].clone() {
        Type::Apply(_) => unreachable!(),
        Type::Ref(id) => semantics_of_impl(arena, id, visited),
        Type::Alias(alias) => semantics_of_impl(arena, alias.inner, visited),
        Type::Never => HashSet::new(),
        Type::Any => HashSet::from_iter([Some(Semantic::All)]),
        Type::Unresolved | Type::Generic(_) | Type::Atom(_) | Type::Pair(_) | Type::Function(_) => {
            HashSet::from_iter([None])
        }
        Type::Struct(ty) => HashSet::from_iter([Some(Semantic::Id(ty.semantic))]),
        Type::Union(ty) => {
            let mut semantics = HashSet::new();

            for &id in &ty.types {
                semantics.extend(semantics_of_impl(arena, id, visited));
            }

            semantics
        }
    }
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use id_arena::Arena;
    use rstest::rstest;

    use crate::{Atom, Type, compare};

    use super::*;

    #[rstest]
    #[case(Atom::NIL, Atom::NIL, Comparison::Assign)]
    #[case(Atom::NIL, Atom::FALSE, Comparison::Cast)]
    #[case(Atom::NIL, Atom::TRUE, Comparison::Invalid)]
    #[case(Atom::NIL, Atom::BYTES, Comparison::Assign)]
    #[case(Atom::NIL, Atom::BYTES_32, Comparison::Invalid)]
    #[case(Atom::NIL, Atom::PUBLIC_KEY, Comparison::Invalid)]
    #[case(Atom::NIL, Atom::SIGNATURE, Comparison::Invalid)]
    #[case(Atom::NIL, Atom::INT, Comparison::Cast)]
    #[case(Atom::FALSE, Atom::NIL, Comparison::Cast)]
    #[case(Atom::FALSE, Atom::FALSE, Comparison::Assign)]
    #[case(Atom::FALSE, Atom::TRUE, Comparison::Invalid)]
    #[case(Atom::FALSE, Atom::BYTES, Comparison::Cast)]
    #[case(Atom::FALSE, Atom::BYTES_32, Comparison::Invalid)]
    #[case(Atom::FALSE, Atom::PUBLIC_KEY, Comparison::Invalid)]
    #[case(Atom::FALSE, Atom::SIGNATURE, Comparison::Invalid)]
    #[case(Atom::FALSE, Atom::INT, Comparison::Cast)]
    #[case(Atom::TRUE, Atom::NIL, Comparison::Invalid)]
    #[case(Atom::TRUE, Atom::FALSE, Comparison::Invalid)]
    #[case(Atom::TRUE, Atom::TRUE, Comparison::Assign)]
    #[case(Atom::TRUE, Atom::BYTES, Comparison::Cast)]
    #[case(Atom::TRUE, Atom::BYTES_32, Comparison::Invalid)]
    #[case(Atom::TRUE, Atom::PUBLIC_KEY, Comparison::Invalid)]
    #[case(Atom::TRUE, Atom::SIGNATURE, Comparison::Invalid)]
    #[case(Atom::TRUE, Atom::INT, Comparison::Cast)]
    #[case(Atom::BYTES, Atom::NIL, Comparison::Invalid)]
    #[case(Atom::BYTES, Atom::FALSE, Comparison::Invalid)]
    #[case(Atom::BYTES, Atom::TRUE, Comparison::Invalid)]
    #[case(Atom::BYTES, Atom::BYTES, Comparison::Assign)]
    #[case(Atom::BYTES, Atom::BYTES_32, Comparison::Invalid)]
    #[case(Atom::BYTES, Atom::PUBLIC_KEY, Comparison::Invalid)]
    #[case(Atom::BYTES, Atom::SIGNATURE, Comparison::Invalid)]
    #[case(Atom::BYTES, Atom::INT, Comparison::Cast)]
    #[case(Atom::BYTES_32, Atom::NIL, Comparison::Invalid)]
    #[case(Atom::BYTES_32, Atom::FALSE, Comparison::Invalid)]
    #[case(Atom::BYTES_32, Atom::TRUE, Comparison::Invalid)]
    #[case(Atom::BYTES_32, Atom::BYTES, Comparison::Assign)]
    #[case(Atom::BYTES_32, Atom::BYTES_32, Comparison::Assign)]
    #[case(Atom::BYTES_32, Atom::PUBLIC_KEY, Comparison::Invalid)]
    #[case(Atom::BYTES_32, Atom::SIGNATURE, Comparison::Invalid)]
    #[case(Atom::BYTES_32, Atom::INT, Comparison::Cast)]
    #[case(Atom::PUBLIC_KEY, Atom::NIL, Comparison::Invalid)]
    #[case(Atom::PUBLIC_KEY, Atom::FALSE, Comparison::Invalid)]
    #[case(Atom::PUBLIC_KEY, Atom::TRUE, Comparison::Invalid)]
    #[case(Atom::PUBLIC_KEY, Atom::BYTES, Comparison::Cast)]
    #[case(Atom::PUBLIC_KEY, Atom::BYTES_32, Comparison::Invalid)]
    #[case(Atom::PUBLIC_KEY, Atom::PUBLIC_KEY, Comparison::Assign)]
    #[case(Atom::PUBLIC_KEY, Atom::SIGNATURE, Comparison::Invalid)]
    #[case(Atom::PUBLIC_KEY, Atom::INT, Comparison::Cast)]
    #[case(Atom::SIGNATURE, Atom::NIL, Comparison::Invalid)]
    #[case(Atom::SIGNATURE, Atom::FALSE, Comparison::Invalid)]
    #[case(Atom::SIGNATURE, Atom::TRUE, Comparison::Invalid)]
    #[case(Atom::SIGNATURE, Atom::BYTES, Comparison::Cast)]
    #[case(Atom::SIGNATURE, Atom::BYTES_32, Comparison::Invalid)]
    #[case(Atom::SIGNATURE, Atom::PUBLIC_KEY, Comparison::Invalid)]
    #[case(Atom::SIGNATURE, Atom::SIGNATURE, Comparison::Assign)]
    #[case(Atom::SIGNATURE, Atom::INT, Comparison::Cast)]
    #[case(Atom::INT, Atom::NIL, Comparison::Invalid)]
    #[case(Atom::INT, Atom::FALSE, Comparison::Invalid)]
    #[case(Atom::INT, Atom::TRUE, Comparison::Invalid)]
    #[case(Atom::INT, Atom::BYTES, Comparison::Cast)]
    #[case(Atom::INT, Atom::BYTES_32, Comparison::Invalid)]
    #[case(Atom::INT, Atom::PUBLIC_KEY, Comparison::Invalid)]
    #[case(Atom::INT, Atom::SIGNATURE, Comparison::Invalid)]
    #[case(Atom::INT, Atom::INT, Comparison::Assign)]
    #[case(Atom::new(AtomSemantic::Int, Some(AtomRestriction::Value(Cow::Borrowed(&[1])))), Atom::INT, Comparison::Assign)]
    #[case(Atom::new(AtomSemantic::Int, Some(AtomRestriction::Value(Cow::Borrowed(&[1])))), Atom::BYTES, Comparison::Cast)]
    fn test_atoms(#[case] lhs: Atom, #[case] rhs: Atom, #[case] expected: Comparison) {
        let mut arena = Arena::new();
        let builtins = BuiltinTypes::new(&mut arena);
        let lhs_id = arena.alloc(Type::Atom(lhs.clone()));
        let rhs_id = arena.alloc(Type::Atom(rhs.clone()));
        assert_eq!(
            compare(&mut arena, &builtins, lhs_id, rhs_id),
            expected,
            "{lhs} -> {rhs}"
        );
    }
}
