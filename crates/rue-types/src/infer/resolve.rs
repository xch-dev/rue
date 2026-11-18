use std::{
    collections::{HashMap, HashSet},
    mem,
};

use id_arena::Arena;
use indexmap::IndexSet;

use crate::{
    AtomRestriction, AtomSemantic, BuiltinTypes, CandidateKind, Comparison, FxHashMap, Inference,
    InferenceCandidates, Type, TypeId, Union, compare, compare_renamed, free_generics,
    references_generic, substitute_with_mappings, unwrap_semantic_without_substitution,
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
    // Only the candidates collected under union members that matched are resolved from (#364)
    let infer = &infer.keep_matched();

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

    let upper = own_upper(arena, builtins, renamed, original, &candidates.upper);

    // The values that are already held by a union's other members are only used if nothing
    // else constrains the generic
    let lower = if own_lower.is_empty() && upper.is_empty() {
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

        // The generic it was renamed from doesn't keep it from being widened, as it didn't before
        // it was a candidate (#378)
        if upper.iter().all(|&upper| {
            Some(upper) == original
                || compare_renamed(arena, builtins, renamed, widened, upper) == Comparison::Assign
        }) {
            Some(widened)
        } else {
            Some(union_of(arena, lower.clone()))
        }
    } else if let Some(&first) = upper.first() {
        // Pick the most specific upper bound, if there is one
        Some(
            upper
                .iter()
                .copied()
                .find(|&ty| {
                    upper.iter().all(|&other| {
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

/// Returns the upper candidates without the generic that the one being inferred was renamed from
/// (see `Inference::rename`), unless it's the most specific of them, so that it only decides the
/// type where it's what the generic is, such as `A` next to `A | nil` (#378). Next to an upper
/// bound it isn't assignable to, such as `Int`, it's left out, and the type is what it was before
/// it was a candidate.
fn own_upper(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    renamed: &FxHashMap<TypeId, TypeId>,
    original: Option<TypeId>,
    upper: &[TypeId],
) -> Vec<TypeId> {
    let Some(original) = original.filter(|original| upper.contains(original)) else {
        return upper.to_vec();
    };

    if upper.iter().all(|&other| {
        other == original
            || compare_renamed(arena, builtins, renamed, original, other) == Comparison::Assign
    }) {
        upper.to_vec()
    } else {
        upper.iter().copied().filter(|&ty| ty != original).collect()
    }
}

impl Inference {
    /// Resolves the vars for passed generic functions' instances (see `Inference::add_instance`)
    /// first, each on its own from its bounds, replaces them in the candidates that reference them,
    /// and removes them, so only the generics of the function being called are left.
    ///
    /// A lower or held candidate takes the least instantiation, from the instance's lower bounds,
    /// and an upper candidate the greatest, from its upper bounds. An instance without bounds of
    /// that kind is instantiated as `Never` or `Any` respectively, such as `(Never, Never)` for `B`
    /// when `fn<T>() -> (T, T)` is compared to `fn() -> B` (#339). That's the least or greatest
    /// type for the generic itself, regardless of where the candidate references it, so a generic
    /// that's only a parameter type in the candidate gives it the greatest function type instead.
    pub(crate) fn resolve_instances(&mut self, arena: &mut Arena<Type>, builtins: &BuiltinTypes) {
        if self.instances.is_empty() {
            return;
        }

        let instances: Vec<(TypeId, InferenceCandidates)> = mem::take(&mut self.instances)
            .into_iter()
            .map(|var| {
                self.branches.shift_remove(&var);
                (var, self.generics.shift_remove(&var).unwrap())
            })
            .collect();

        // What each instance is instantiated as, for lower (and held) and upper candidates
        let mut picked: FxHashMap<(TypeId, bool), TypeId> = FxHashMap::default();

        for candidates in self.generics.values_mut() {
            for kind in [
                CandidateKind::Lower,
                CandidateKind::Upper,
                CandidateKind::Held,
            ] {
                let upper = matches!(kind, CandidateKind::Upper);

                for ty in candidates.list_mut(kind) {
                    // Most candidates reference none of the vars, which this finds in one pass
                    let free = free_generics(arena, *ty);
                    let mut mappings = HashMap::new();

                    for (var, bounds) in &instances {
                        if !free.contains(var)
                            || !references_generic(arena, *ty, *var, |_| Vec::new())
                        {
                            continue;
                        }

                        let instantiated = *picked.entry((*var, upper)).or_insert_with(|| {
                            // Without bounds of the candidate's kind, the least instantiation is
                            // `Never` and the greatest is `Any`
                            if upper {
                                let upper = InferenceCandidates {
                                    upper: bounds.upper.clone(),
                                    ..Default::default()
                                };
                                pick_inferred_type(arena, builtins, &self.renamed, None, &upper)
                                    .unwrap_or(builtins.permissive_any)
                            } else if bounds.lower.is_empty() {
                                builtins.never
                            } else {
                                pick_inferred_type(arena, builtins, &self.renamed, None, bounds)
                                    .unwrap()
                            }
                        });

                        mappings.insert(*var, instantiated);
                    }

                    if !mappings.is_empty() {
                        *ty = substitute_with_mappings(arena, *ty, &mappings);
                    }
                }
            }
        }
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

/// The union of the types. A member that's a union of atoms is replaced by its members, and an
/// atom that's the same as an earlier one is left out, so that unions built from each other, such
/// as those of generics inferred through a chain of others, don't double in size each time (#337).
/// Unions with other members are kept whole, since one can be recursive, such as the runtime
/// structure of a function.
///
/// The result is only a single type if it was given one. Otherwise it stays a union, even of one
/// member, since a union isn't widened as a literal is (see `widen_literal`), which inference has
/// always relied on for `7 | 7`.
pub(crate) fn union_of(arena: &mut Arena<Type>, types: impl IntoIterator<Item = TypeId>) -> TypeId {
    let types: IndexSet<TypeId> = types.into_iter().collect();

    if types.len() == 1 {
        return types[0];
    }

    let mut members: IndexSet<TypeId> = IndexSet::new();
    let mut atoms: Vec<(AtomSemantic, Option<AtomRestriction>)> = Vec::new();
    let mut stack: Vec<TypeId> = types.into_iter().rev().collect();

    while let Some(ty) = stack.pop() {
        match &arena[ty] {
            Type::Union(union)
                if union
                    .types
                    .iter()
                    .all(|&member| matches!(arena[member], Type::Atom(_))) =>
            {
                stack.extend(union.types.iter().rev().copied());
            }
            Type::Atom(atom) => {
                let key = (atom.semantic, atom.restriction.clone());

                if !atoms.contains(&key) {
                    atoms.push(key);
                    members.insert(ty);
                }
            }
            _ => {
                members.insert(ty);
            }
        }
    }

    arena.alloc(Type::Union(Union::new(members.into_iter().collect())))
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
