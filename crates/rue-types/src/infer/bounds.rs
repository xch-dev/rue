use std::collections::{HashMap, HashSet};

use id_arena::Arena;
use indexmap::{IndexMap, IndexSet};

use crate::{
    BranchId, BuiltinTypes, CandidateKind, Comparison, Inference, Type, TypeId, compare_renamed,
    compare_with_inference, free_generics, infer_from_expected, resolve_inference_around,
    resolve_inference_with_fallback, strongly_connected_components, substitute_with_mappings,
    union_of,
};

/// An upper bound for a generic, along with the union members it was collected under.
pub(crate) type Bound = (TypeId, TypeId, Vec<BranchId>);

/// Adds the upper bounds that were inferred from the type expected of a call to the generics whose
/// lower bounds satisfy them (as they do if there are none), so that `let x: 5 = id(5);` doesn't
/// widen the literal to `Int`. A bound that isn't satisfied is left out, since the mismatch is
/// reported where it's checked.
///
/// A generic inferred through another, such as `B` with the candidate `A` in `map(identity, 5)`,
/// passes the bound on to it, since what flows into `B` through `A` must satisfy it too. Its lower
/// bounds are checked through those of the generics it's inferred through.
pub fn add_expected_bounds(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    infer: &mut Inference,
    expected: &Inference,
) {
    let bounds = infer
        .generics
        .keys()
        .filter_map(|&generic| Some((generic, expected.candidates(generic)?)))
        .flat_map(|(generic, expected)| {
            expected
                .upper
                .iter()
                .map(move |&upper| (generic, upper, Vec::new()))
        })
        .collect();

    pass_upper_bounds(arena, builtins, infer, bounds, Vec::new(), |_| true);
}

/// Like `resolve_inference_with_fallback`, but a generic that it can't infer is also given the
/// upper bounds of the generics it's inferred through, which it must satisfy too. In
/// `feed(identity, inc)`, `identity` gives `B` the lower bound `A` and `inc` gives it the upper
/// bound `Int`, so `A` is bounded by `Int` as well, and both are inferred as `Int` (#285).
///
/// A generic that was inferred is given them too, so that it isn't widened past them: in
/// `run(identity, five_only, 5)`, `A` has the lower bound `5` and reaches `five_only`'s `B ≤ 5`
/// through `A ≤ B`, so it's `5` rather than `Int` (#337). Only the upper bounds of union members
/// that matched are resolved from (`Inference::keep_matched`), one is only added if the generic's
/// lower bounds satisfy it, and a mapping is only replaced by a narrower one (see
/// `narrow_inferred`).
///
/// A generic that wasn't inferred, or was inferred as a type that references another generic
/// being inferred that wasn't, also takes the lower bounds passed down through its compound upper
/// bounds (see `pass_lower_bounds`).
pub fn resolve_inference_with_bounds(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    infer: &Inference,
    fallback: &HashMap<TypeId, TypeId>,
) -> HashMap<TypeId, TypeId> {
    let derived = with_added_var_lower_bounds(arena, builtins, infer);
    let infer = derived.as_ref().unwrap_or(infer);
    let mut mappings = resolve_inference_with_fallback(arena, builtins, infer, fallback);
    narrow_inferred(arena, builtins, infer, fallback, &mut mappings);

    let uninferred: HashSet<TypeId> = infer
        .generics
        .keys()
        .copied()
        .filter(|generic| {
            mappings.get(generic).is_none_or(|&ty| {
                free_generics(arena, ty)
                    .into_iter()
                    .any(|other| infer.passes_bounds(other) && !mappings.contains_key(&other))
            })
        })
        .collect();

    if uninferred.is_empty() {
        return mappings;
    }

    let mut bounded = infer.clone();

    pass_upper_bounds(
        arena,
        builtins,
        &mut bounded,
        Vec::new(),
        upper_bounds(infer),
        |generic| uninferred.contains(&generic),
    );

    pass_lower_bounds(arena, builtins, &mut bounded, |generic| {
        uninferred.contains(&generic)
    });

    // The generics that were inferred keep their mappings, including what `narrow_inferred` narrowed
    // them to, so that the ones resolved through them see those, rather than what they were inferred
    // as before they were narrowed. Resolving them again would only give what they were first
    // inferred as, since their own candidates are the same and don't depend on the ones being
    // resolved here
    let fixed: HashMap<TypeId, TypeId> = mappings
        .iter()
        .filter(|(generic, _)| !uninferred.contains(*generic))
        .map(|(&generic, &ty)| (generic, ty))
        .collect();

    let resolved = resolve_inference_around(arena, builtins, &bounded, fallback, &fixed);

    for generic in uninferred {
        // A cycle through compound bounds can resolve a generic to a type that references itself,
        // which no instantiation satisfies, so it's left uninferred instead
        if let Some(&ty) = resolved.get(&generic)
            && !free_generics(arena, ty).contains(&generic)
        {
            mappings.insert(generic, ty);
        }
    }

    mappings
}

/// The inference with each var added along with the generics (see `Inference::add_var`), such as
/// an untyped lambda parameter's type, given the generics it's an upper candidate of as lower
/// candidates, collected under the same union members, where it has no lower candidates and none
/// of its upper candidates is assignable to all the others. `None` if there are none.
///
/// `A ≤ X` is only recorded on `A`'s side when `A`'s value flows into the var `X`. So `X` would be
/// picked from its upper candidates alone, and with none the most specific, such as `Int` (the
/// type the lambda was compiled with, where `A` had been widened) and `5 | Bytes` (passed on from
/// the type expected of the call), the first one would be picked, and `A` would be widened to it.
/// With `A` as a lower candidate, `X` and `A` are resolved together, from `A`'s value and all of
/// `X`'s upper candidates, as the explicit instantiation would be.
fn with_added_var_lower_bounds(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    infer: &Inference,
) -> Option<Inference> {
    if infer.added.is_empty() {
        return None;
    }

    let mut derived = Vec::new();

    for &var in &infer.added {
        let candidates = &infer.generics[&var];

        if !candidates.lower.is_empty()
            || candidates.upper.iter().any(|&ty| {
                candidates.upper.iter().all(|&other| {
                    compare_renamed(arena, builtins, &infer.renamed, ty, other)
                        == Comparison::Assign
                })
            })
        {
            continue;
        }

        for (&generic, candidates) in &infer.generics {
            for (index, &upper) in candidates.upper.iter().enumerate() {
                if upper == var && generic != var {
                    let branches = infer
                        .branches(generic, CandidateKind::Upper, index)
                        .to_vec();
                    derived.push((var, generic, branches));
                }
            }
        }
    }

    if derived.is_empty() {
        return None;
    }

    let mut infer = infer.clone();

    for (var, generic, branches) in derived {
        if !infer.generics[&var].lower.contains(&generic) {
            infer.push(var, CandidateKind::Lower, generic, branches);
        }
    }

    Some(infer)
}

/// Replaces the mapping of each generic that was inferred, as a type that doesn't reference one
/// that wasn't, with what it's inferred as once the upper bounds of the generics it's inferred
/// through are passed on to it, if that's narrower (see `resolve_inference_with_bounds`).
fn narrow_inferred(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    infer: &Inference,
    fallback: &HashMap<TypeId, TypeId>,
    mappings: &mut HashMap<TypeId, TypeId>,
) {
    let inferred: HashSet<TypeId> = infer
        .generics
        .keys()
        .copied()
        .filter(|generic| {
            mappings.get(generic).is_some_and(|&ty| {
                !free_generics(arena, ty)
                    .into_iter()
                    .any(|other| infer.passes_bounds(other) && !mappings.contains_key(&other))
            })
        })
        .collect();

    if inferred.is_empty() {
        return;
    }

    // Only the upper bounds that don't reference a generic being inferred are passed on. One that
    // does, such as `B | 5`, can't be checked until that generic is resolved, and the widened type
    // could still satisfy it then
    let bounds: Vec<Bound> = upper_bounds(infer)
        .into_iter()
        .filter(|(_, upper, _)| {
            !free_generics(arena, *upper)
                .into_iter()
                .any(|other| infer.is_target(other))
        })
        .collect();

    if bounds.is_empty() {
        return;
    }

    let mut bounded = infer.clone();

    pass_upper_bounds(
        arena,
        builtins,
        &mut bounded,
        Vec::new(),
        bounds,
        |generic| inferred.contains(&generic),
    );

    // Bounds are only added to the generics in `inferred`, so if none was added to, resolving again
    // gives the same mappings
    if inferred
        .iter()
        .all(|generic| bounded.generics[generic].upper.len() == infer.generics[generic].upper.len())
    {
        return;
    }

    let resolved = resolve_inference_with_fallback(arena, builtins, &bounded, fallback);

    // A mapping is only replaced by a narrower one, since the bounds can only rule out what it was
    // widened to. A generic that falls back to `5` and is given the bound `Int` stays `5`, and one
    // with only held candidates, such as `L` when `nil` is passed to `L | nil`, keeps `nil` (#333)
    for generic in inferred {
        if let Some(&ty) = resolved.get(&generic)
            && !free_generics(arena, ty).contains(&generic)
            && compare_renamed(arena, builtins, &infer.renamed, ty, mappings[&generic])
                == Comparison::Assign
        {
            mappings.insert(generic, ty);
        }
    }
}

/// Gives the generics in each generic's compound upper bounds that `accepts` allows the lower
/// bounds that its own lower bounds imply. In `map(first_of, (1, 2))` with
/// `fn first_of<T>(pair: (T, T)) -> T`, `A` has the lower bound `(1, 2)` and the upper bound
/// `(B, B)`, so `(1, 2) ≤ (B, B)` gives `B` the lower bounds `1` and `2` (#338). This is the
/// direction `pass_upper_bounds` doesn't cover: a generic's lower bounds flowing into the generics
/// in its upper bounds. An upper bound that's just a generic is left out, since resolution already
/// resolves through it.
///
/// It's repeated until nothing new is passed on, so that a lower bound passed to a generic reaches
/// the generics in that generic's compound upper bounds too.
///
/// Each lower bound that's passed on is collected under the union members its upper bound was. The
/// lower bounds it comes from are taken together (`expanded_lower_bounds`), whatever members they
/// were collected under.
pub(crate) fn pass_lower_bounds(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    infer: &mut Inference,
    accepts: impl Fn(TypeId) -> bool,
) {
    let targets: Vec<TypeId> = infer
        .generics
        .keys()
        .copied()
        .filter(|&generic| infer.passes_bounds(generic))
        .collect();
    let renamed = infer.renamed.clone();

    // A lower bound that's passed on can reach a generic with a compound upper bound of its own,
    // as in `map2(first_of, first_of, ((1, 2), (3, 4)))`, so this is repeated until nothing new is
    // passed on. A chain takes one round per compound, but a cycle through compound bounds can
    // keep making deeper types, so the rounds are capped at one per generic. After the first
    // round, only the generics whose lower bounds, taken together, can have changed are compared
    // again: the ones that were given a lower bound, and the ones whose lower bounds reach them.
    let mut changed: Option<HashSet<TypeId>> = None;

    // The members of each generic's expanded lower bounds that have been compared with each of its
    // upper bounds, by the upper bound's index. A union is compared with a type one member at a
    // time, so comparing the members again would only find the bounds they found before, which
    // aren't passed on again. Only the ones that are new to an expansion are compared, including
    // the ones that reach it through another generic's expansion
    let mut compared: HashMap<(TypeId, usize), HashSet<TypeId>> = HashMap::new();

    // Whether each pair of bounds that has been checked is equivalent. The same bound can be passed
    // on from several generics, and again in later rounds
    let mut equivalent: HashMap<(TypeId, TypeId), bool> = HashMap::new();

    for _ in 0..infer.generics.len() {
        let expanded = expanded_lower_bounds(arena, infer);
        let mut passed = Vec::new();

        for (&generic, candidates) in &infer.generics {
            if changed
                .as_ref()
                .is_some_and(|changed| !changed.contains(&generic))
            {
                continue;
            }

            let Some(lower) = expanded[&generic] else {
                continue;
            };

            let members = union_members(arena, lower);

            for (index, &upper) in candidates.upper.iter().enumerate() {
                if infer.passes_bounds(upper)
                    || !free_generics(arena, upper)
                        .into_iter()
                        .any(|other| infer.passes_bounds(other) && accepts(other))
                {
                    continue;
                }

                let compared = compared.entry((generic, index)).or_default();
                let mut inner = Inference::new(targets.iter().copied());

                for &member in &members {
                    if compared.insert(member) {
                        compare_with_inference(arena, builtins, member, upper, Some(&mut inner));
                    }
                }
                let branches = infer.branches(generic, CandidateKind::Upper, index);

                for (&other, implied) in &inner.generics {
                    if !accepts(other) {
                        continue;
                    }

                    for kind in [CandidateKind::Lower, CandidateKind::Held] {
                        for &ty in implied.list(kind) {
                            passed.push((other, kind, ty, branches.to_vec()));
                        }
                    }
                }
            }
        }

        // A bound the generic already has, or one that's equivalent to it (assignable both ways),
        // isn't passed on again, so that the rounds stop
        let mut grown = HashSet::new();

        for (generic, kind, ty, branches) in passed {
            let existing = infer.generics[&generic].list(kind);

            if existing.contains(&ty)
                || existing.iter().any(|&other| {
                    *equivalent.entry((other, ty)).or_insert_with(|| {
                        compare_renamed(arena, builtins, &renamed, other, ty) == Comparison::Assign
                            && compare_renamed(arena, builtins, &renamed, ty, other)
                                == Comparison::Assign
                    })
                })
            {
                continue;
            }

            infer.push(generic, kind, ty, branches);
            grown.insert(generic);
        }

        if grown.is_empty() {
            break;
        }

        changed = Some(reaching(arena, infer, grown));
    }
}

/// The members of the type if it's a union, and of the unions among them in turn, in the order
/// comparing it with another type visits them, or the type itself if it isn't one.
fn union_members(arena: &Arena<Type>, ty: TypeId) -> Vec<TypeId> {
    let mut members = Vec::new();
    let mut visited = HashSet::new();
    let mut stack = vec![ty];

    while let Some(ty) = stack.pop() {
        if !visited.insert(ty) {
            continue;
        }

        match &arena[ty] {
            Type::Union(union) => stack.extend(union.types.iter().rev().copied()),
            _ => members.push(ty),
        }
    }

    members
}

/// The generics whose lower bounds, taken together (see `expanded_lower_bounds`), reach any of the
/// given ones, including them.
fn reaching(arena: &Arena<Type>, infer: &Inference, generics: HashSet<TypeId>) -> HashSet<TypeId> {
    let mut reaching = generics;
    let mut added = true;

    while added {
        added = false;

        for (&generic, candidates) in &infer.generics {
            if reaching.contains(&generic) {
                continue;
            }

            if candidates.lower.iter().any(|&ty| {
                free_generics(arena, ty)
                    .iter()
                    .any(|other| reaching.contains(other))
            }) {
                reaching.insert(generic);
                added = true;
            }
        }
    }

    reaching
}

/// Every generic's upper bounds, with the union members each was collected under.
pub(crate) fn upper_bounds(infer: &Inference) -> Vec<Bound> {
    infer
        .generics
        .iter()
        .flat_map(|(&generic, candidates)| {
            candidates
                .upper
                .iter()
                .enumerate()
                .map(move |(index, &upper)| {
                    let branches = infer
                        .branches(generic, CandidateKind::Upper, index)
                        .to_vec();
                    (generic, upper, branches)
                })
        })
        .collect()
}

/// Adds each of the `added` bounds to its generic if the generic's lower bounds satisfy it (or
/// reference a var that isn't decided yet, see `Inference::is_undecided`), and
/// passes each bound that's added, as well as each of the `existing` ones (which the generics
/// already have), on to the generics in the generic's lower bounds that `accepts` allows, in the
/// same way. A lower bound that's a generic gets the bound directly, and one that references
/// generics in a compound type passes on what comparing it to the bound infers for them.
///
/// Each bound comes with the union members it was collected under (see `CandidateBranches`). One
/// that's passed on through a lower bound was collected under the members of both.
pub(crate) fn pass_upper_bounds(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    infer: &mut Inference,
    added: Vec<Bound>,
    existing: Vec<Bound>,
    accepts: impl Fn(TypeId) -> bool,
) {
    let targets: Vec<TypeId> = infer
        .generics
        .keys()
        .copied()
        .filter(|&generic| infer.passes_bounds(generic))
        .collect();
    let renamed = infer.renamed.clone();

    // The bounds given to each generic directly, rather than passed on from another
    let given_directly: HashSet<(TypeId, TypeId)> = added
        .iter()
        .map(|&(generic, upper, _)| (generic, upper))
        .collect();

    // Each entry is a bound and whether it still has to be added to its generic. A bound passed on
    // is part of the one it was passed on from
    let mut stack: Vec<(Bound, bool)> = existing
        .into_iter()
        .map(|bound| (bound, false))
        .chain(added.into_iter().map(|bound| (bound, true)))
        .collect();
    stack.reverse();

    // The bounds each generic has been given, including the ones it already had. A bound that's the
    // same as one of them (or equivalent, assignable both ways) is skipped, since passing it on
    // again can create the same type with a new id (such as a union being substituted), which
    // would otherwise go around a cycle of generics forever.
    //
    // It's skipped unless it holds and the one it's the same as doesn't, since the bounds of union
    // members that didn't match are left out (`Inference::keep_matched`). So each generic is given
    // at most two of the same bound, and a cycle still ends.
    let results = infer.choice_results();
    let mut given: HashMap<TypeId, Vec<(TypeId, bool)>> = HashMap::new();

    for ((generic, upper, branches), add) in &stack {
        if !add {
            let holds = infer.holds(&results, branches);
            given.entry(*generic).or_default().push((*upper, holds));
        }
    }

    // A generic's lower bounds don't change here, so they're only expanded once
    let expanded = expanded_lower_bounds(arena, infer);

    while let Some(((generic, upper, branches), add)) = stack.pop() {
        if add {
            // A renamed generic with no lower candidates is either the generic it was renamed from
            // (see `Inference::rename`) or what its held candidates are, as before bounds were passed
            // on to it, so it isn't passed one that the generic it was renamed from already
            // satisfies, which would otherwise decide its type (#361)
            let held_by_original = !given_directly.contains(&(generic, upper))
                && infer
                    .candidates(generic)
                    .is_some_and(|candidates| candidates.lower.is_empty())
                && renamed.get(&generic).is_some_and(|&original| {
                    compare_renamed(arena, builtins, &renamed, original, upper)
                        == Comparison::Assign
                });

            // It isn't recorded as given either, so that an equivalent bound given directly still
            // is. It has no lower candidates, so there's nothing to pass it on to
            if held_by_original {
                continue;
            }

            let given = given.entry(generic).or_default();

            let holds = infer.holds(&results, &branches);

            if given.iter().any(|&(other, other_holds)| {
                (other_holds || !holds)
                    && (other == upper
                        || compare_renamed(arena, builtins, &renamed, other, upper)
                            == Comparison::Assign
                            && compare_renamed(arena, builtins, &renamed, upper, other)
                                == Comparison::Assign)
            }) {
                continue;
            }

            given.push((upper, holds));

            // Lower bounds that reference a var for a passed generic function's instance, or for an
            // untyped lambda parameter's type, can't be checked until it's decided, which these
            // bounds are passed on to do (see `Inference::is_undecided`)
            if expanded[&generic].is_some_and(|lower| {
                !(infer.has_undecided()
                    && free_generics(arena, lower)
                        .iter()
                        .any(|&other| infer.is_undecided(other)))
                    && compare_renamed(arena, builtins, &renamed, lower, upper)
                        != Comparison::Assign
            }) {
                continue;
            }

            infer.push(generic, CandidateKind::Upper, upper, branches.clone());
        }

        // The generics that flow into this one through its lower bounds must satisfy it too
        for (index, ty) in infer.generics[&generic]
            .lower
            .clone()
            .into_iter()
            .enumerate()
        {
            let mut passed = branches.clone();
            passed.extend_from_slice(infer.branches(generic, CandidateKind::Lower, index));
            passed.sort_unstable();
            passed.dedup();

            if infer.passes_bounds(ty) {
                if accepts(ty) {
                    stack.push(((ty, upper, passed), true));
                }
                continue;
            }

            if !free_generics(arena, ty)
                .iter()
                .any(|&other| infer.passes_bounds(other))
            {
                continue;
            }

            let mut inner = Inference::new(targets.iter().copied());
            infer_from_expected(arena, builtins, ty, upper, &mut inner);

            // A bound collected under a member of a union in the upper bound is only passed on if
            // that member is the only one the lower bound matches. In `(T, T) ≤ (Int, Int) | (5, 5)`,
            // both match, so `T` isn't bounded by `5`. In `(T, nil) ≤ (Int, nil) | nil`, only the
            // pair does, so `T` is bounded by `Int`
            let results = inner.choice_results();

            for (&other, candidates) in &inner.generics {
                if accepts(other) {
                    stack.extend(
                        candidates
                            .upper
                            .iter()
                            .enumerate()
                            .filter(|&(index, _)| {
                                inner.required(
                                    &results,
                                    inner.branches(other, CandidateKind::Upper, index),
                                )
                            })
                            .map(|(_, &upper)| ((other, upper, passed.clone()), true)),
                    );
                }
            }
        }
    }
}

/// Returns the union of each generic's lower bounds, with each generic being inferred that they
/// reference replaced by the union of that generic's own expanded lower bounds (if it has any), so
/// that they can be checked against a bound, or `None` if there are none. A lower bound
/// that's just such a generic adds nothing if that generic has none, and one that only references
/// it is left as is.
///
/// The generics that reach each other through their lower bounds (a strongly connected component)
/// have the same expansion, made of what they have other than each other, so they're expanded
/// together, after the ones they reach. A lower bound that references one of them in a compound
/// type is left as is, since expanding it would be an infinite type.
fn expanded_lower_bounds(
    arena: &mut Arena<Type>,
    infer: &Inference,
) -> HashMap<TypeId, Option<TypeId>> {
    // Only the lower bounds collected under union members that matched are checked against, as
    // they're the ones the generics are resolved from (see `Inference::keep_matched`)
    let infer = &infer.keep_matched();

    let edges: IndexMap<TypeId, Vec<TypeId>> = infer
        .generics
        .iter()
        .map(|(&generic, candidates)| {
            let edges = candidates
                .lower
                .iter()
                .flat_map(|&ty| free_generics(arena, ty))
                .filter(|&other| infer.passes_bounds(other))
                .collect();
            (generic, edges)
        })
        .collect();

    let mut expanded: HashMap<TypeId, Option<TypeId>> = HashMap::new();

    // The components are in reverse topological order, so the ones a component reaches are
    // expanded before it
    for component in strongly_connected_components(&edges) {
        let mut result = IndexSet::new();

        for &generic in &component {
            let mappings: HashMap<TypeId, TypeId> = edges[&generic]
                .iter()
                .filter_map(|&other| Some((other, (*expanded.get(&other)?)?)))
                .collect();

            for &ty in &infer.generics[&generic].lower {
                if component.contains(&ty) {
                    continue;
                }

                if infer.passes_bounds(ty) {
                    // A generic without lower bounds doesn't add any yet
                    if let Some(&union) = mappings.get(&ty) {
                        result.insert(union);
                    }
                } else if mappings.is_empty() {
                    result.insert(ty);
                } else {
                    result.insert(substitute_with_mappings(arena, ty, &mappings));
                }
            }
        }

        let union = (!result.is_empty()).then(|| union_of(arena, result));

        for generic in component {
            expanded.insert(generic, union);
        }
    }

    expanded
}
