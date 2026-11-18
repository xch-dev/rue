use std::collections::{HashMap, HashSet};

use id_arena::Arena;
use indexmap::{IndexMap, IndexSet};

use crate::{
    BranchId, BuiltinTypes, CandidateKind, Comparison, Inference, Type, TypeId, compare_renamed,
    free_generics, infer_from_expected, resolve_inference_with_fallback, substitute_with_mappings,
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
/// This only replaces the mappings of the generics that weren't inferred, or were inferred as a type
/// that references another generic being inferred that wasn't, since those are reported as errors.
/// Every other mapping is kept, since upper bounds that nothing enforces can be left over from a
/// union member that didn't match, and passing those on would change what's inferred.
pub fn resolve_inference_with_bounds(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    infer: &Inference,
    fallback: &HashMap<TypeId, TypeId>,
) -> HashMap<TypeId, TypeId> {
    let mut mappings = resolve_inference_with_fallback(arena, builtins, infer, fallback);

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

    let resolved = resolve_inference_with_fallback(arena, builtins, &bounded, fallback);

    for generic in uninferred {
        if let Some(&ty) = resolved.get(&generic) {
            mappings.insert(generic, ty);
        }
    }

    mappings
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

/// Adds each of the `added` bounds to its generic if the generic's lower bounds satisfy it, and
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
    // A bound that's skipped this way keeps the union members of the one it's the same as, so if
    // the two were collected under different members, the one that's kept holds only under its
    // own. Nothing reads which members a bound was collected under here yet; the step that leaves
    // out the bounds of members that didn't match (#362) has to account for it.
    let mut given: HashMap<TypeId, Vec<TypeId>> = HashMap::new();

    for ((generic, upper, _), add) in &stack {
        if !add {
            given.entry(*generic).or_default().push(*upper);
        }
    }

    // A generic's lower bounds don't change here, so they're only expanded once
    let expanded = expanded_lower_bounds(arena, infer);

    while let Some(((generic, upper, branches), add)) = stack.pop() {
        if add {
            let given = given.entry(generic).or_default();

            if given.contains(&upper)
                || given.iter().any(|&other| {
                    compare_renamed(arena, builtins, &renamed, other, upper) == Comparison::Assign
                        && compare_renamed(arena, builtins, &renamed, upper, other)
                            == Comparison::Assign
                })
            {
                continue;
            }

            given.push(upper);

            if expanded[&generic].is_some_and(|lower| {
                compare_renamed(arena, builtins, &renamed, lower, upper) != Comparison::Assign
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

            for (&other, candidates) in &inner.generics {
                if accepts(other) {
                    stack.extend(
                        candidates
                            .upper
                            .iter()
                            .map(|&upper| ((other, upper, passed.clone()), true)),
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

/// Returns the strongly connected components of the graph with Tarjan's algorithm, each one after
/// the components it has edges to. An edge to a node that isn't in the graph is ignored.
fn strongly_connected_components(edges: &IndexMap<TypeId, Vec<TypeId>>) -> Vec<Vec<TypeId>> {
    struct State<'a> {
        edges: &'a IndexMap<TypeId, Vec<TypeId>>,
        index: HashMap<TypeId, usize>,
        low: HashMap<TypeId, usize>,
        stack: Vec<TypeId>,
        on_stack: HashSet<TypeId>,
        components: Vec<Vec<TypeId>>,
    }

    fn visit(state: &mut State<'_>, node: TypeId) {
        let index = state.index.len();
        state.index.insert(node, index);
        state.low.insert(node, index);
        state.stack.push(node);
        state.on_stack.insert(node);

        for &next in &state.edges[&node] {
            if !state.edges.contains_key(&next) {
                continue;
            }

            if !state.index.contains_key(&next) {
                visit(state, next);
                let low = state.low[&node].min(state.low[&next]);
                state.low.insert(node, low);
            } else if state.on_stack.contains(&next) {
                let low = state.low[&node].min(state.index[&next]);
                state.low.insert(node, low);
            }
        }

        if state.low[&node] == index {
            let mut component = Vec::new();

            loop {
                let member = state.stack.pop().unwrap();
                state.on_stack.remove(&member);
                component.push(member);

                if member == node {
                    break;
                }
            }

            state.components.push(component);
        }
    }

    let mut state = State {
        edges,
        index: HashMap::new(),
        low: HashMap::new(),
        stack: Vec::new(),
        on_stack: HashSet::new(),
        components: Vec::new(),
    };

    for &node in edges.keys() {
        if !state.index.contains_key(&node) {
            visit(&mut state, node);
        }
    }

    state.components
}
