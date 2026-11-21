use std::{
    collections::{BTreeSet, HashMap, HashSet},
    mem,
};

use id_arena::Arena;
use indexmap::{IndexMap, IndexSet};

use crate::{
    AtomRestriction, AtomSemantic, BuiltinTypes, CandidateKind, Comparison, FxHashMap, FxHashSet,
    Inference, InferenceCandidates, InstancePolicy, Type, TypeId, Union, compare, compare_renamed,
    free_generics, references_generic, substitute_with_mappings, unwrap_semantic,
    unwrap_semantic_without_substitution,
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
    resolve_inference_around(arena, builtins, infer, fallback, &HashMap::new())
}

/// Like `resolve_inference_with_fallback`, but the generics in `fixed` are already resolved, to
/// the types they map to, so the others are resolved through those (see
/// `resolve_inference_with_bounds`).
pub(crate) fn resolve_inference_around(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    infer: &Inference,
    fallback: &HashMap<TypeId, TypeId>,
    fixed: &HashMap<TypeId, TypeId>,
) -> HashMap<TypeId, TypeId> {
    // Only the candidates collected under union members that matched, and that no other
    // member makes unneeded, are resolved from (#364, `Inference::keep_matched`)
    let infer = &infer.keep_matched(arena, builtins);

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
        resolved: fixed.keys().copied().collect(),
        mappings: fixed.clone(),
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
            // A generic that's already resolved isn't resolved with it again. Without fixed ones
            // (see `resolve_inference_around`) that can't happen, since it would have been resolved
            // in the same component. A fixed generic can still reach the generic through a
            // candidate its mapping doesn't use, such as an upper candidate next to lower ones, and
            // it keeps its mapping then too. No test or fuzzer program has been found to reach it
            .filter(|&other| {
                other == generic
                    || (reachable.contains(&other)
                        && self.reachable[&other].contains(&generic)
                        && !self.resolved.contains(&other))
            })
            .collect();

        self.resolved.extend(component.iter().copied());

        for other in reachable {
            if !component.contains(&other) {
                self.resolve(arena, builtins, other);
            }
        }

        // The generics whose candidates all come from the others in the component, or say nothing,
        // are resolved after them
        let mut bounded_by_component = Vec::new();

        for &generic in &component {
            let lower = self.expand(arena, &component, generic, CandidateKind::Lower, false);
            let candidates = InferenceCandidates {
                upper: self.expand(
                    arena,
                    &component,
                    generic,
                    CandidateKind::Upper,
                    lower.is_empty(),
                ),
                held: self.expand(arena, &component, generic, CandidateKind::Held, false),
                lower,
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
        // to, so their candidates are used as they are, unless that names the generic itself.
        let mut pending = bounded_by_component;

        while !pending.is_empty() {
            let resolved = self.resolve_through_component(arena, builtins, &component, &pending);

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

            // These are the raw candidates, which still have what `expand` leaves out, such as
            // `A | nil` for `A ≤ A | nil`, so the generic is left uninferred if what they give isn't
            // a solution for it (see `is_solution`)
            for generic in pending.drain(..) {
                if let Some(ty) = pick_inferred_type(
                    arena,
                    builtins,
                    &self.infer.renamed,
                    self.infer.renamed.get(&generic).copied(),
                    &self.infer.generics[&generic],
                ) && is_solution(arena, &component, generic, ty)
                {
                    self.mappings.insert(generic, ty);
                }
            }
        }
    }

    /// Picks a type for each of the generics, whose candidates all come from others in their component,
    /// from the types that those were resolved to. They all read the same mappings, so the order
    /// of the generics doesn't matter. The ones not bounded by any that were resolved are left out.
    fn resolve_through_component(
        &self,
        arena: &mut Arena<Type>,
        builtins: &BuiltinTypes,
        component: &IndexSet<TypeId>,
        pending: &[TypeId],
    ) -> HashMap<TypeId, TypeId> {
        // Their candidates are generics in the component, since they're only bounded by it, or what
        // `expand` leaves out or splits: a lower union with a generic of the component as a
        // member, whose members are taken instead, and an upper union with the generic itself as
        // one, which says nothing
        let resolved = |arena: &mut Arena<Type>, generic: TypeId, kind: CandidateKind| {
            let mut types: Vec<TypeId> = self.infer.generics[&generic]
                .list(kind)
                .iter()
                .rev()
                .copied()
                .collect();
            let mut result = Vec::new();

            while let Some(ty) = types.pop() {
                if kind == CandidateKind::Lower
                    && let Some(members) = union_members(arena, ty)
                {
                    types.extend(members.into_iter().rev());
                } else if let Some(&mapped) = self.mappings.get(&ty)
                    && is_solution(arena, component, generic, mapped)
                {
                    result.push(mapped);
                }
            }

            result
        };

        pending
            .iter()
            .filter_map(|&generic| {
                let candidates = InferenceCandidates {
                    lower: resolved(arena, generic, CandidateKind::Lower),
                    upper: resolved(arena, generic, CandidateKind::Upper),
                    held: resolved(arena, generic, CandidateKind::Held),
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
    ///
    /// A lower candidate that's a union is a lower candidate of each of its members (`T ≥ X | Y` is
    /// `T ≥ X` and `T ≥ Y`), so one with a generic of the component as a member is split into them,
    /// and the generic is followed as a candidate of its own. That's what passing a generic function
    /// for one of the callee's function types gives, as `T ≥ Int | T` from
    /// `fn<V>(x: V) -> Int | V` for `fn(a: T) -> T`, which only says `T ≥ Int`. A union behind an
    /// alias counts too, as `Maybe<T>` from `fn<V>(x: V) -> Maybe<V>`, and so does one nested in a
    /// member.
    ///
    /// An upper candidate that's a union with the generic as a member says nothing, since
    /// `T ≤ X | T` always holds, so it's left out. That's what an untyped lambda parameter compiled
    /// as `Maybe<A>` gives against `fn(a: Maybe<A>) -> Int`, as `A ≤ A | nil`, which `A` was then
    /// inferred as, though nothing gives it a type (infer-fuzz generator 3, seed 6717). The same
    /// goes for one with another generic of the component as a member, which the cycle lets be the
    /// generic: `T ≤ V` with `V ≤ T | nil` says nothing about either.
    ///
    /// The upper ones are only left out (`drop_upper_unions`) where the generic has no lower
    /// candidates, since that's where they'd be what it's inferred as. Where it has some, it's
    /// inferred from them, and the upper ones are kept as they are, where they keep a literal from
    /// being widened, as `6` in `g5(fn(x) => 6)` with `fn g5<A>(f: fn(a: Maybe<A>) -> A) -> A`, which
    /// `Int` can't be shown to satisfy `A ≤ A | nil` for, as before #362's 13k5.
    fn expand(
        &self,
        arena: &mut Arena<Type>,
        component: &IndexSet<TypeId>,
        generic: TypeId,
        kind: CandidateKind,
        drop_upper_unions: bool,
    ) -> Vec<TypeId> {
        let mut visited = HashSet::from([generic]);
        let mut stack = vec![generic];
        let mut result = IndexSet::new();

        while let Some(bounded) = stack.pop() {
            // A stack, so that they're taken in the order they were found, with a split union's
            // members in its place
            let mut candidates: Vec<TypeId> = self.infer.generics[&bounded]
                .list(kind)
                .iter()
                .rev()
                .copied()
                .collect();

            while let Some(ty) = candidates.pop() {
                // A union with a generic of the component as a member is split if it's a lower
                // candidate, and says nothing if it's an upper one
                if (kind == CandidateKind::Lower || drop_upper_unions)
                    && has_union_member(arena, ty, |member| component.contains(&member))
                {
                    if kind == CandidateKind::Lower {
                        let members = union_members(arena, ty).expect("it has union members");
                        candidates.extend(members.into_iter().rev());
                    }
                    continue;
                }

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
    /// when `fn<T>() -> (T, T)` is compared to `fn() -> B` (#339).
    ///
    /// When they're solved jointly (`InstancePolicy::Joint`), a var that's only contravariant in a
    /// lower or held candidate, such as `T` in `fn(x: T) -> nil`, takes the greatest one, which is
    /// what gives the candidate its least type (#358), as far as `Inference::restore_unheld` keeps
    /// it.
    pub(crate) fn resolve_instances(&mut self, arena: &mut Arena<Type>, builtins: &BuiltinTypes) {
        let joint = self.policy == InstancePolicy::Joint;
        let instances = mem::take(&mut self.instances);

        // The candidates with a var instantiated the other way, and what they'd be with each var
        // instantiated for their kind instead (see `Inference::restore_unheld`)
        let mut flipped: IndexMap<(TypeId, CandidateKind, usize), TypeId> = IndexMap::new();

        // Each is replaced in the candidates that are left, including the bounds of the vars after
        // it, so a var that a bound passed on to another var references is replaced in turn. Only
        // the candidates that can reference it are visited (`mentions`), in the order of the
        // generics, their kinds and their indices, so the candidates are replaced in the same order
        // as if each one were checked
        let order: Vec<TypeId> = self.generics.keys().copied().collect();
        let pending: HashSet<TypeId> = instances.iter().copied().collect();
        let mut mentions: HashMap<TypeId, BTreeSet<CandidateKey>> = HashMap::new();

        for (position, candidates) in self.generics.values().enumerate() {
            for kind in CandidateKind::ALL {
                for (index, &ty) in candidates.list(kind).iter().enumerate() {
                    note_mentions(arena, &mut mentions, &pending, ty, (position, kind, index));
                }
            }
        }

        for &var in &instances {
            self.branches.shift_remove(&var);
            let mut bounds = self.generics.shift_remove(&var).unwrap();

            // A bound passed on to it can reference it, through another var that was replaced with
            // it, which says nothing about what it is
            for kind in [CandidateKind::Lower, CandidateKind::Upper] {
                bounds
                    .list_mut(kind)
                    .retain(|&ty| !references_generic(arena, ty, var, |_| Vec::new()));
            }

            // What it's instantiated as where the least and greatest instantiation is needed
            let mut picked: [Option<TypeId>; 2] = [None, None];
            let mut instantiate = |arena: &mut Arena<Type>, ty: TypeId, greatest: bool| {
                let instantiated = *picked[usize::from(greatest)].get_or_insert_with(|| {
                    // Without bounds of the kind it needs, the least instantiation is `Never` and
                    // the greatest is `Any`
                    if greatest {
                        let upper = InferenceCandidates {
                            upper: bounds.upper.clone(),
                            ..Default::default()
                        };
                        pick_inferred_type(arena, builtins, &self.renamed, None, &upper)
                            .unwrap_or(builtins.permissive_any)
                    } else if bounds.lower.is_empty() {
                        builtins.never
                    } else {
                        pick_inferred_type(arena, builtins, &self.renamed, None, &bounds).unwrap()
                    }
                });

                substitute_with_mappings(arena, ty, &HashMap::from([(var, instantiated)]))
            };

            for (position, kind, index) in mentions.remove(&var).unwrap_or_default() {
                let generic = order[position];

                // The vars before this one have been removed, and their bounds with them
                let Some(candidates) = self.generics.get_mut(&generic) else {
                    continue;
                };

                // The bounds of the vars that are left are only used to pick what they are
                let flips = joint && !pending.contains(&generic);

                let upper = kind == CandidateKind::Upper;
                let key = (generic, kind, index);
                let ty = &mut candidates.list_mut(kind)[index];

                // What it would be with each var instantiated for its kind can reference vars that
                // it doesn't, through the other instantiation's bounds
                if let Some(by_kind) = flipped.get_mut(&key)
                    && references_generic(arena, *by_kind, var, |_| Vec::new())
                {
                    *by_kind = instantiate(arena, *by_kind, upper);

                    note_mentions(
                        arena,
                        &mut mentions,
                        &pending,
                        *by_kind,
                        (position, kind, index),
                    );
                }

                if !references_generic(arena, *ty, var, |_| Vec::new()) {
                    continue;
                }

                // Where it's only contravariant in a lower or held candidate, the greatest
                // instantiation gives the candidate its least type, as a parameter type in
                // `fn(x: T) -> nil` does (#358). An upper candidate keeps the greatest
                // instantiation wherever it is, since a generic with only upper candidates is
                // inferred as them, and a weaker bound would be a wider type
                let flip = flips && !upper && {
                    let (covariant, contravariant) = polarity(arena, *ty, var);
                    contravariant && !covariant
                };

                if flip && !flipped.contains_key(&key) {
                    let by_kind = instantiate(arena, *ty, upper);
                    flipped.insert(key, by_kind);

                    note_mentions(
                        arena,
                        &mut mentions,
                        &pending,
                        by_kind,
                        (position, kind, index),
                    );
                }

                *ty = instantiate(arena, *ty, upper || flip);

                note_mentions(arena, &mut mentions, &pending, *ty, (position, kind, index));
            }
        }

        self.restore_unheld(arena, builtins, &flipped);
    }

    /// Puts back each candidate with a var instantiated the other way (see
    /// `Inference::resolve_instances`) with what it is with the var instantiated for its kind,
    /// unless one of the function types among its generic's other candidates of the same kind holds
    /// it and all the others, or there are none. Otherwise the function types wouldn't collapse into
    /// one as they do with the other instantiation: `fn(a: Never) -> Bytes` holds
    /// `fn(a: Bytes) -> "a"`, but `fn(a: Any) -> Bytes` would be a member of a union with it. The
    /// other candidates instantiated the other way don't count, since they would hold each other
    /// when the same function is passed twice.
    fn restore_unheld(
        &mut self,
        arena: &mut Arena<Type>,
        builtins: &BuiltinTypes,
        flipped: &IndexMap<(TypeId, CandidateKind, usize), TypeId>,
    ) {
        let mut others: IndexMap<(TypeId, CandidateKind), Vec<TypeId>> = IndexMap::new();

        for (&(generic, kind, index), &by_kind) in flipped {
            let others = others.entry((generic, kind)).or_insert_with(|| {
                self.generics[&generic]
                    .list(kind)
                    .iter()
                    .enumerate()
                    .filter(|&(other, &ty)| {
                        !flipped.contains_key(&(generic, kind, other))
                            // As `without_subsumed_functions` sees them, through aliases
                            && matches!(
                                arena[unwrap_semantic_without_substitution(arena, ty, true)],
                                Type::Function(_)
                            )
                    })
                    .map(|(_, &ty)| ty)
                    .collect()
            });

            let candidate = self.generics[&generic].list(kind)[index];
            let mut assigns = |from: TypeId, to: TypeId| {
                compare_renamed(arena, builtins, &self.renamed, from, to) == Comparison::Assign
            };

            let held = others.is_empty()
                || others.iter().any(|&holder| {
                    assigns(candidate, holder)
                        && others
                            .iter()
                            .all(|&other| other == holder || assigns(other, holder))
                });

            if !held {
                self.generics[&generic].list_mut(kind)[index] = by_kind;
            }
        }
    }
}

/// Whether the generic is referenced in the type where it's covariant, and where it's
/// contravariant (a function's parameter types, flipped again for each function they're in). The
/// generics of an alias, struct or application count as both.
fn polarity(arena: &Arena<Type>, ty: TypeId, generic: TypeId) -> (bool, bool) {
    let mut result = (false, false);
    let mut visited = HashSet::new();
    let mut stack = vec![(ty, true)];

    while let Some((id, covariant)) = stack.pop() {
        if !visited.insert((id, covariant)) {
            continue;
        }

        if id == generic {
            if covariant {
                result.0 = true;
            } else {
                result.1 = true;
            }
            continue;
        }

        match &arena[id] {
            Type::Unresolved | Type::Generic(_) | Type::Any | Type::Never | Type::Atom(_) => {}
            Type::Ref(inner) => stack.push((*inner, covariant)),
            Type::Pair(pair) => stack.extend([(pair.first, covariant), (pair.rest, covariant)]),
            Type::Alias(alias) => {
                stack.push((alias.inner, covariant));
                for &ty in &alias.generics {
                    stack.extend([(ty, true), (ty, false)]);
                }
            }
            Type::Struct(ty) => {
                stack.push((ty.inner, covariant));
                for &ty in &ty.generics {
                    stack.extend([(ty, true), (ty, false)]);
                }
            }
            Type::Apply(apply) => {
                for &ty in apply.generics.values() {
                    stack.extend([(ty, true), (ty, false)]);
                }
            }
            Type::Function(function) => {
                stack.push((function.ret, covariant));
                stack.extend(function.param_types().map(|ty| (ty, !covariant)));
            }
            Type::Union(union) => stack.extend(union.types.iter().map(|&ty| (ty, covariant))),
        }
    }

    result
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

/// The union of the types. A member that's a union is replaced by its members, an atom that's the
/// same as an earlier one is left out, and so is a compound type that two earlier members are
/// copies of (see `same_structure`), so that unions built from each other, such as those of
/// generics inferred through a chain of others, don't double in size each time (#337).
///
/// At most two copies of a compound type are kept, not one, since a union with one copy of `(5, 5)`
/// doesn't type the same as one with two. A field of a union of pairs is the field of its only pair
/// if it has one (`pairs.len() == 1` in the compiler's `field.rs`), so it's a lone `5`, which
/// inference widens to `Int`, where two pairs give the union `5 | 5`, which it doesn't. Keeping one
/// copy rejected the `x4.first` in a program that main compiles (infer-fuzz generator 1, seed 4600,
/// pinned in `tests/types/generic_inference_union_copies.rue`). Two copies say the same as any
/// number more, so the doubling still stops.
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

    let mut flattened = FxHashSet::default();

    while let Some(ty) = stack.pop() {
        match &arena[ty] {
            Type::Union(union) => {
                if flattened.insert(ty) {
                    stack.extend(union.types.iter().rev().copied());
                }
            }
            // The same as `same_structure`'s check for atoms, by key, since there can be many
            Type::Atom(atom) => {
                let key = (atom.semantic, atom.restriction.clone());

                if !atoms.contains(&key) {
                    atoms.push(key);
                    members.insert(ty);
                }
            }
            _ => {
                let copies = members
                    .iter()
                    .filter(|&&member| same_structure(arena, member, ty))
                    .count();

                if copies < 2 {
                    members.insert(ty);
                }
            }
        }
    }

    arena.alloc(Type::Union(Union::new(members.into_iter().collect())))
}

/// Whether the types are copies of each other: atoms with the same semantic and restriction, or
/// pairs, unions and references whose parts are copies, member by member, or the same type. It's
/// conservative, so other types are only copies if they're the same type.
fn same_structure(arena: &Arena<Type>, lhs: TypeId, rhs: TypeId) -> bool {
    // The pairs of types that have been reached. One that's reached again is either being checked
    // further down, as with a recursive type that unfolds the same way, or has been checked
    let mut reached: FxHashSet<(TypeId, TypeId)> = FxHashSet::default();
    let mut stack = vec![(lhs, rhs)];

    while let Some((lhs, rhs)) = stack.pop() {
        if lhs == rhs || !reached.insert((lhs, rhs)) {
            continue;
        }

        match (&arena[lhs], &arena[rhs]) {
            (Type::Atom(lhs), Type::Atom(rhs))
                if lhs.semantic == rhs.semantic && lhs.restriction == rhs.restriction => {}
            (Type::Pair(lhs), Type::Pair(rhs)) => {
                stack.extend([(lhs.first, rhs.first), (lhs.rest, rhs.rest)]);
            }
            (Type::Union(lhs), Type::Union(rhs)) if lhs.types.len() == rhs.types.len() => {
                stack.extend(lhs.types.iter().copied().zip(rhs.types.iter().copied()));
            }
            (Type::Ref(lhs), Type::Ref(rhs)) => stack.push((*lhs, *rhs)),
            _ => return false,
        }
    }

    true
}

/// Replaces a literal atom type with the type it's a value of, such as `1` with `Int`.
pub fn widen_literal(arena: &mut Arena<Type>, builtins: &BuiltinTypes, ty: TypeId) -> TypeId {
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

/// A candidate, as the position of its generic in the inference, its kind and its index in that
/// kind's list, which sort in the order `Inference::resolve_instances` visits them.
type CandidateKey = (usize, CandidateKind, usize);

/// Records that the candidate can reference each of the vars that `ty` can (see `mentioned_vars`).
fn note_mentions(
    arena: &Arena<Type>,
    mentions: &mut HashMap<TypeId, BTreeSet<CandidateKey>>,
    vars: &HashSet<TypeId>,
    ty: TypeId,
    key: CandidateKey,
) {
    for var in mentioned_vars(arena, ty, vars) {
        mentions.entry(var).or_default().insert(key);
    }
}

/// The vars among `vars` that the type can reference, as `references_generic` finds them. This
/// over-approximates them, since it also looks inside function types that bind them.
fn mentioned_vars(arena: &Arena<Type>, id: TypeId, vars: &HashSet<TypeId>) -> Vec<TypeId> {
    let mut found = Vec::new();
    let mut visited = HashSet::new();
    let mut stack = vec![id];

    while let Some(id) = stack.pop() {
        if !visited.insert(id) {
            continue;
        }

        if vars.contains(&id) {
            found.push(id);
        }

        match &arena[id] {
            Type::Unresolved | Type::Generic(_) | Type::Any | Type::Never | Type::Atom(_) => {}
            Type::Ref(id) => stack.push(*id),
            Type::Pair(pair) => stack.extend([pair.first, pair.rest]),
            Type::Alias(alias) => stack.push(alias.inner),
            Type::Struct(ty) => stack.push(ty.inner),
            Type::Apply(apply) => {
                stack.push(apply.inner);
                stack.extend(apply.generics.values().copied());
            }
            Type::Function(function) => {
                stack.push(function.ret);
                stack.extend(function.param_types());
            }
            Type::Union(union) => stack.extend(union.types.iter().copied()),
        }
    }

    found
}

/// Whether the type can be what a generic of the component is resolved to: another generic of the
/// component, which a cycle with nothing else leaves it as (#336), or a type that names none of
/// them. One that names the generic itself, as `A | nil` does for `A ≤ A | nil`, or another of the
/// component inside of something else, as `T | nil` does for `V` with `T ≤ V ≤ T | nil`, would
/// leave a generic of the call in its type (#362, 13k4 and 13k5).
fn is_solution(
    arena: &Arena<Type>,
    component: &IndexSet<TypeId>,
    generic: TypeId,
    ty: TypeId,
) -> bool {
    if component.contains(&ty) {
        ty != generic
    } else {
        !free_generics(arena, ty)
            .iter()
            .any(|other| component.contains(other))
    }
}

/// The members of the union that the type is, directly or behind a reference or an alias, with
/// the alias's generic arguments substituted, if it's one.
fn union_members(arena: &mut Arena<Type>, ty: TypeId) -> Option<Vec<TypeId>> {
    let ty = match &arena[ty] {
        Type::Union(_) => ty,
        Type::Ref(_) | Type::Alias(_) | Type::Apply(_) => unwrap_semantic(arena, ty, true),
        _ => return None,
    };

    match &arena[ty] {
        Type::Union(union) => Some(union.types.clone()),
        _ => None,
    }
}

/// Whether the type is a union with a member that the predicate holds for, or one of whose members
/// is such a union in turn (see `union_members`).
fn has_union_member(
    arena: &mut Arena<Type>,
    ty: TypeId,
    predicate: impl Fn(TypeId) -> bool,
) -> bool {
    let mut stack = vec![ty];
    let mut visited = HashSet::new();

    while let Some(ty) = stack.pop() {
        if !visited.insert(ty) {
            continue;
        }
        let Some(members) = union_members(arena, ty) else {
            continue;
        };
        if members.iter().any(|&member| predicate(member)) {
            return true;
        }
        stack.extend(members);
    }

    false
}
