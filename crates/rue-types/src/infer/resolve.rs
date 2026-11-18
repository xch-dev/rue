use std::collections::{HashMap, HashSet};

use id_arena::Arena;
use indexmap::IndexSet;

use crate::{
    AtomRestriction, AtomSemantic, BuiltinTypes, Comparison, FxHashMap, Inference,
    InferenceCandidates, Type, TypeId, Union, compare, compare_renamed, free_generics,
    substitute_with_mappings, unwrap_semantic_without_substitution,
};

/// Picks a type for each inferred generic, given the candidates collected while comparing.
///
/// A candidate can reference another generic being inferred, such as `A` becoming a candidate for
/// `B` when `fn<U>(x: U) -> U` is passed to `fn(a: A) -> B`. It's resolved through what that
/// generic is inferred as, so the generics it depends on are resolved first. Generics that depend
/// on each other are resolved together, with each one taking the candidates of the ones it's
/// directly bounded by, since a cycle like `A: B, B: A` only says that they're the same type.
///
/// Inference only proposes types, it doesn't guarantee that they satisfy every constraint. The
/// caller must check the values against the types substituted with these mappings afterward.
pub fn resolve_inference(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    infer: &Inference,
) -> HashMap<TypeId, TypeId> {
    resolve_inference_with_fallback(arena, builtins, infer, &HashMap::new())
}

/// Like `resolve_inference`, but a generic that nothing constrains is mapped to its type in
/// `fallback` (such as one inferred from the type expected of a call), if it has one, and the
/// generics that depend on it are resolved through that.
pub fn resolve_inference_with_fallback(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    infer: &Inference,
    fallback: &HashMap<TypeId, TypeId>,
) -> HashMap<TypeId, TypeId> {
    let references: HashMap<TypeId, HashSet<TypeId>> = infer
        .generics
        .iter()
        .map(|(&generic, candidates)| {
            let references = candidates
                .lower
                .iter()
                .chain(&candidates.upper)
                .chain(&candidates.held)
                .flat_map(|&ty| free_generics(arena, ty))
                .filter(|&other| infer.is_target(other))
                .collect();
            (generic, references)
        })
        .collect();

    let reachable = infer
        .generics
        .keys()
        .map(|&generic| {
            let mut reachable = HashSet::new();
            let mut stack = vec![generic];

            while let Some(generic) = stack.pop() {
                for &other in &references[&generic] {
                    if reachable.insert(other) {
                        stack.push(other);
                    }
                }
            }

            (generic, reachable)
        })
        .collect();

    let mut resolver = InferenceResolver {
        infer,
        fallback,
        reachable,
        resolved: HashSet::new(),
        mappings: HashMap::new(),
    };

    for &generic in infer.generics.keys() {
        resolver.resolve(arena, builtins, generic);
    }

    resolver.mappings
}

struct InferenceResolver<'a> {
    infer: &'a Inference,
    fallback: &'a HashMap<TypeId, TypeId>,
    /// The generics being inferred that each generic's candidates reference, directly or through
    /// other generics.
    reachable: HashMap<TypeId, HashSet<TypeId>>,
    resolved: HashSet<TypeId>,
    mappings: HashMap<TypeId, TypeId>,
}

impl InferenceResolver<'_> {
    /// Resolves the generic along with the others that it depends on, and that depend on it.
    fn resolve(&mut self, arena: &mut Arena<Type>, builtins: &BuiltinTypes, generic: TypeId) {
        if self.resolved.contains(&generic) {
            return;
        }

        let reachable = self.reachable[&generic].clone();
        let component: IndexSet<TypeId> = self
            .infer
            .generics
            .keys()
            .copied()
            .filter(|&other| {
                other == generic
                    || (reachable.contains(&other) && self.reachable[&other].contains(&generic))
            })
            .collect();

        self.resolved.extend(component.iter().copied());

        for other in reachable {
            if !component.contains(&other) {
                self.resolve(arena, builtins, other);
            }
        }

        // The generics that are only bounded by the others in the component are resolved after them
        let mut bounded_by_component = Vec::new();

        for &generic in &component {
            let candidates = InferenceCandidates {
                lower: self.expand(arena, &component, generic, |c| &c.lower),
                upper: self.expand(arena, &component, generic, |c| &c.upper),
                held: self.expand(arena, &component, generic, |c| &c.held),
            };

            if candidates == InferenceCandidates::default()
                && self.infer.generics[&generic] != InferenceCandidates::default()
            {
                bounded_by_component.push(generic);
                continue;
            }

            let original = self.infer.renamed.get(&generic).copied();
            let ty =
                pick_inferred_type(arena, builtins, &self.infer.renamed, original, &candidates)
                    .or_else(|| self.fallback.get(&generic).copied());

            if let Some(ty) = ty {
                self.mappings.insert(generic, ty);
            }
        }

        // Such a generic is resolved through the ones it's bounded by that were resolved, which can
        // be others like it, so this is repeated until nothing more resolves. Then the remaining
        // ones fall back like a generic that nothing constrains, and the ones they bound are
        // resolved through them in turn. If neither is possible, there's nothing to resolve them
        // to, so their candidates are used as they are.
        let mut pending = bounded_by_component;

        while !pending.is_empty() {
            let resolved = self.resolve_through_component(arena, builtins, &pending);

            if !resolved.is_empty() {
                pending.retain(|generic| !resolved.contains_key(generic));
                self.mappings.extend(resolved);
                continue;
            }

            let fallbacks: HashMap<TypeId, TypeId> = pending
                .iter()
                .filter_map(|generic| Some((*generic, *self.fallback.get(generic)?)))
                .collect();

            if !fallbacks.is_empty() {
                pending.retain(|generic| !fallbacks.contains_key(generic));
                self.mappings.extend(fallbacks);
                continue;
            }

            for generic in pending.drain(..) {
                if let Some(ty) = pick_inferred_type(
                    arena,
                    builtins,
                    &self.infer.renamed,
                    self.infer.renamed.get(&generic).copied(),
                    &self.infer.generics[&generic],
                ) {
                    self.mappings.insert(generic, ty);
                }
            }
        }
    }

    /// Picks a type for each of the generics, which are only bounded by others in their component,
    /// from the types that those were resolved to. They all read the same mappings, so the order
    /// of the generics doesn't matter. The ones not bounded by any that were resolved are left out.
    fn resolve_through_component(
        &self,
        arena: &mut Arena<Type>,
        builtins: &BuiltinTypes,
        pending: &[TypeId],
    ) -> HashMap<TypeId, TypeId> {
        // Their candidates are all generics in the component, since they're only bounded by it
        let resolved = |types: &Vec<TypeId>| -> Vec<TypeId> {
            types
                .iter()
                .filter_map(|ty| self.mappings.get(ty).copied())
                .collect()
        };

        pending
            .iter()
            .filter_map(|&generic| {
                let raw = &self.infer.generics[&generic];
                let candidates = InferenceCandidates {
                    lower: resolved(&raw.lower),
                    upper: resolved(&raw.upper),
                    held: resolved(&raw.held),
                };
                let original = self.infer.renamed.get(&generic).copied();
                let ty = pick_inferred_type(
                    arena,
                    builtins,
                    &self.infer.renamed,
                    original,
                    &candidates,
                )?;
                Some((generic, ty))
            })
            .collect()
    }

    /// Collects the generic's candidates of one kind, with those of the generics in its component
    /// that it's directly bounded by in place of them, and the generics that were already resolved
    /// substituted. A candidate that references a generic in the component in any other way would
    /// be an infinite type, so it's left as is.
    fn expand(
        &self,
        arena: &mut Arena<Type>,
        component: &IndexSet<TypeId>,
        generic: TypeId,
        kind: impl Fn(&InferenceCandidates) -> &Vec<TypeId>,
    ) -> Vec<TypeId> {
        let mut visited = HashSet::from([generic]);
        let mut stack = vec![generic];
        let mut result = IndexSet::new();

        while let Some(generic) = stack.pop() {
            for &ty in kind(&self.infer.generics[&generic]) {
                if component.contains(&ty) {
                    if visited.insert(ty) {
                        stack.push(ty);
                    }
                    continue;
                }

                let free = free_generics(arena, ty);

                if free.iter().any(|other| component.contains(other)) {
                    result.insert(ty);
                    continue;
                }

                let mappings: HashMap<TypeId, TypeId> = free
                    .into_iter()
                    .filter(|&other| self.infer.is_target(other))
                    .filter_map(|other| Some((other, *self.mappings.get(&other)?)))
                    .collect();

                if mappings.is_empty() {
                    result.insert(ty);
                } else {
                    result.insert(substitute_with_mappings(arena, ty, &mappings));
                }
            }
        }

        result.into_iter().collect()
    }
}

/// Picks a type for a generic from its candidates, or `None` if nothing constrains it. The
/// `original` is the generic it was renamed from, if it was (see `Inference::rename`).
pub(crate) fn pick_inferred_type(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    renamed: &FxHashMap<TypeId, TypeId>,
    original: Option<TypeId>,
    candidates: &InferenceCandidates,
) -> Option<TypeId> {
    let own_lower = without_subsumed_original(arena, builtins, renamed, original, candidates);

    // The values that are already held by a union's other members are only used if nothing
    // else constrains the generic
    let lower = if own_lower.is_empty() && candidates.upper.is_empty() {
        &candidates.held
    } else {
        &own_lower
    };
    let lower = &without_subsumed_functions(arena, builtins, renamed, lower);

    if !lower.is_empty() {
        // Literals are widened to their base type, so that `pick(1, 2)` infers `Int` and a
        // lambda whose parameter type comes from `T` can accept more than the one literal.
        // This is only done if the widened type still satisfies every upper bound, since
        // otherwise something like `fn(x: 5) -> Int` couldn't be passed to `fn(x: T) -> Int`.
        let widened: Vec<TypeId> = lower
            .iter()
            .map(|&ty| widen_literal(arena, builtins, ty))
            .collect();
        let widened = union_of(arena, widened);

        if candidates.upper.iter().all(|&upper| {
            compare_renamed(arena, builtins, renamed, widened, upper) == Comparison::Assign
        }) {
            Some(widened)
        } else {
            Some(union_of(arena, lower.clone()))
        }
    } else if let Some(&first) = candidates.upper.first() {
        // Pick the most specific upper bound, if there is one
        Some(
            candidates
                .upper
                .iter()
                .copied()
                .find(|&ty| {
                    candidates.upper.iter().all(|&other| {
                        compare_renamed(arena, builtins, renamed, ty, other) == Comparison::Assign
                    })
                })
                .unwrap_or(first),
        )
    } else {
        // Nothing constrains the generic, so leave it as is
        None
    }
}

/// Returns the lower candidates without the generic that the one being inferred was renamed from
/// (see `Inference::rename`) where the other lower and held candidates already hold it, such as
/// `A` next to `Any`. A renamed generic takes the one it was renamed from as a candidate (#370), which
/// adds nothing then, so its type is what it was before.
fn without_subsumed_original(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    renamed: &FxHashMap<TypeId, TypeId>,
    original: Option<TypeId>,
    candidates: &InferenceCandidates,
) -> Vec<TypeId> {
    let Some(original) = original else {
        return candidates.lower.clone();
    };

    let others: Vec<TypeId> = candidates
        .lower
        .iter()
        .chain(&candidates.held)
        .copied()
        .filter(|&other| other != original)
        .collect();

    // The others together can hold it, such as the members of `Any` in a union's other members
    if !others.is_empty() && {
        let others = union_of(arena, others);
        compare_renamed(arena, builtins, renamed, original, others) == Comparison::Assign
    } {
        candidates
            .lower
            .iter()
            .copied()
            .filter(|&ty| ty != original)
            .collect()
    } else {
        candidates.lower.clone()
    }
}

/// Leaves out the function types that are assignable to another of the types, such as
/// `fn<V>(value: V) -> V` next to `fn(x: Int) -> Int`, since that one already holds them, so their
/// union is just the other (#355). Of function types that are assignable to each other, the first
/// is kept. Otherwise a generic function's own generics would be in the parameter types of the
/// union, where they aren't bound, and a union of function types can't be called.
fn without_subsumed_functions(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    renamed: &FxHashMap<TypeId, TypeId>,
    types: &[TypeId],
) -> Vec<TypeId> {
    let functions: Vec<bool> = types
        .iter()
        .map(|&ty| {
            let ty = unwrap_semantic_without_substitution(arena, ty, true);
            matches!(arena[ty], Type::Function(_))
        })
        .collect();

    if functions.iter().filter(|&&function| function).count() < 2 {
        return types.to_vec();
    }

    let mut result = Vec::new();

    for (i, &ty) in types.iter().enumerate() {
        let subsumed = functions[i]
            && types.iter().enumerate().any(|(j, &other)| {
                j != i
                    && functions[j]
                    && compare_renamed(arena, builtins, renamed, ty, other) == Comparison::Assign
                    && (j < i
                        || compare_renamed(arena, builtins, renamed, other, ty)
                            != Comparison::Assign)
            });

        if !subsumed {
            result.push(ty);
        }
    }

    result
}

pub(crate) fn union_of(arena: &mut Arena<Type>, types: impl IntoIterator<Item = TypeId>) -> TypeId {
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
