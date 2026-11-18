use std::{
    cmp::{max, min},
    collections::{HashMap, HashSet},
    mem,
};

use id_arena::Arena;
use indexmap::IndexMap;
use log::{Level, debug, log_enabled, trace};

use crate::{
    AtomRestriction, AtomSemantic, BranchId, BuiltinTypes, CandidateKind, FALLBACK, FixpointFrame,
    FixpointMemo, Frame as MemoFrame, FxHashMap, FxIndexSet, Inference, InferenceCandidates, Memo,
    Type, TypeId, compare_memo_enabled, free_generics, grow_stack, has_non_contractive_cycle,
    pick_inferred_type, reachable, references_generic, step, stringify_impl, substitute,
    substitute_with_mappings,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Comparison {
    Assign,
    Cast,
    Invalid,
}

/// A candidate that references the generics of a generic function on the other side that's being
/// instantiated, such as `(T, T)` for `B` when `fn<T>(x: T) -> (T, T)` is compared to
/// `fn(a: A) -> B`. Those generics aren't in scope where the candidate is used, so once the
/// comparison has finished, they're replaced with what they're instantiated as (`A` here).
///
/// A lower or held candidate takes the least instantiation, from the instance's lower bounds, and
/// an upper candidate the greatest, from its upper bounds. An instance without bounds of that kind
/// is instantiated as `Never` or `Any` respectively, such as `(Never, Never)` for `B` when
/// `fn<T>() -> (T, T)` is compared to `fn() -> B` (#339). That's the least or greatest type for the
/// generic itself, regardless of where the candidate references it, so a generic that's only a
/// parameter type in the candidate gives it the greatest function type instead. An instance that's
/// left out of `ComparisonContext::instantiations` is left as is.
#[derive(Debug, Clone, Copy)]
struct DeferredCandidate {
    generic: TypeId,
    kind: CandidateKind,
    index: usize,
    /// The environment that the candidate's generics are resolved in.
    env: Option<usize>,
}

/// The types being compared, whether the comparison is flipped, what the generics they reference
/// are bound to (interned in `ComparisonContext::bindings`), and the semantics passed down to it.
/// This is everything that the result depends on other than the stack, so that a comparison is
/// only assumed to hold when it's revisited exactly, and its result can be reused (see `Memo`).
type ComparisonKey = (TypeId, TypeId, bool, usize, Option<TypeId>, Option<TypeId>);

/// The memo that a comparison in progress will be cached in (see `ComparisonContext::fixpoint`).
enum Frame {
    Exact(MemoFrame),
    Fixpoint(FixpointFrame),
}

/// A bound of a generic being instantiated, along with the environment it's resolved in.
type Bound = (TypeId, Option<usize>);

/// What a generic referenced by a type being compared is bound to. The binders are numbered in
/// the order they're referenced, so that a comparison can be revisited with other binders in
/// their place (such as a recursive type that binds the same generic each time it's reached).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Binding {
    Instance(usize),
    Rigid(usize),
}

/// A generic bound by a function type that's being compared, which shadows any generic with the
/// same id that's bound outside of it (or free) on the same side.
#[derive(Debug, Clone)]
struct Binder {
    generic: TypeId,
    /// The binder enclosing this one on the same side.
    parent: Option<usize>,
    /// The index in `ComparisonContext::instances` of the generic's instance, if it's on the left
    /// hand side, since a generic function is assignable to a function type if it can be
    /// instantiated to it. Otherwise it's rigid, since the function must work for any of them.
    instance: Option<usize>,
    /// The binder that this one was merged into, which it's treated as from then on.
    merged: Option<usize>,
    /// The rigid binders that are renamed in the bounds that flow into this binder's instance
    /// once it's merged, from the ones on its side of the comparison it was merged at to the ones
    /// on the other side.
    renaming: Vec<(usize, usize)>,
}

/// The bounds collected for a generic being instantiated, which are checked against each other
/// once the comparison that bound it has finished.
#[derive(Debug, Default)]
struct Instance {
    lower: Vec<Bound>,
    upper: Vec<Bound>,
    /// Set if a comparison was assumed to hold for it when it was revisited, so it's missing the
    /// bounds that it would have collected from the revisit. Those are copies of its bounds in
    /// which the binders from this index on (which were bound since the comparison was entered)
    /// and the `renamed` ones are other generics, so it's also checked with them replaced by new
    /// ones.
    fresh: Option<usize>,
    /// The binders that were renamed when it was revisited, which were bound before then.
    renamed: Vec<usize>,
}

#[derive(Debug, Default)]
pub(crate) struct ComparisonContext<'a> {
    infer: Option<&'a mut Inference>,
    stack: FxIndexSet<ComparisonKey>,
    /// The number of binders when each comparison in `stack` was entered, and the binders it
    /// references, in the order they're numbered.
    stack_binders: Vec<(usize, Vec<usize>)>,
    /// The indices in `stack` of comparisons between pairs or functions. A cycle is only
    /// coinductively assignable if it passes through one of these, since otherwise the
    /// recursion isn't bounded by the structure of the value.
    guards: Vec<usize>,
    /// Whether the comparison has been flipped an odd number of times by function parameters.
    /// When this is set, the generics being inferred are on the left hand side rather than the
    /// right hand side.
    flipped: bool,
    /// Every binder that has been entered. Generics are resolved to them by scope rather than by
    /// id, so that two generic functions are equivalent up to renaming of their generics, and a
    /// generic bound by one is never confused with a free generic or one bound elsewhere.
    binders: Vec<Binder>,
    /// The innermost binders of the left and right hand sides.
    lhs_env: Option<usize>,
    rhs_env: Option<usize>,
    /// The bounds of the generics being instantiated, indexed by `Binder::instance`.
    instances: Vec<Instance>,
    /// The instances before this index are rigid, since they belong to enclosing comparisons.
    instantiable_from: usize,
    /// What the generics referenced by the types in each `ComparisonKey` are bound to, for each
    /// side and generic.
    bindings: FxIndexSet<Vec<(bool, TypeId, Binding)>>,
    /// The generics that each type references, which are cached since the types don't change
    /// while comparing.
    references: FxHashMap<TypeId, Vec<TypeId>>,
    /// The rigid generics of functions in the type that the generics are being inferred from (the
    /// right hand side unless flipped), such as `U` in a parameter of type `fn<U>(x: U) -> T`.
    /// They aren't in scope where the inferred generics are used, so they can't be inferred as
    /// them. They're kept afterward, since they can still reach the bounds of enclosing functions'
    /// generics.
    rigid: FxIndexSet<TypeId>,
    /// The number of inference candidates that have been collected.
    candidates: usize,
    /// Whether the value is already held by another member of a union being compared against,
    /// so the generics being inferred only collect it as a last resort.
    holding: bool,
    /// The innermost union member that the value is being compared with while inferring, which
    /// the candidates collected now are recorded under (see `Inference::matched_candidates`).
    branch: Option<BranchId>,
    /// The candidates that reference the generics of a function being instantiated, which are
    /// replaced with what those generics are instantiated as once the comparison has finished.
    deferred: Vec<DeferredCandidate>,
    /// Once a candidate has been deferred, the bounds of each instance that finishes afterward, by
    /// binder. An instance is left out if one of its bounds references a generic bound in the
    /// comparison, since that generic isn't in scope where the candidate is used.
    instantiations: FxHashMap<usize, InferenceCandidates>,
    /// The generics being inferred that were renamed from another generic (see `Inference::rename`),
    /// kept here so that comparisons without the inference see them too.
    renamed: FxHashMap<TypeId, TypeId>,
    /// The context for comparisons that leave the generics being inferred rigid, which is kept
    /// so that its results are memoized for the whole comparison.
    uninferred: Option<Box<ComparisonContext<'static>>>,
    /// Completed comparisons, reused only where computing them again would do the same thing.
    memo: Memo<ComparisonKey, ComparisonKey, Comparison>,
    /// Whether the comparison that started from an empty stack is the least fixed point of
    /// monotone equations (see `is_monotone`), so `fixpoint_memo` is used instead of `memo`.
    fixpoint: bool,
    /// Completed comparisons, reused while the comparisons they relied on are in progress, or
    /// approximations of them while iterating (see `FixpointMemo`).
    fixpoint_memo: FixpointMemo<ComparisonKey, Comparison>,
    /// Whether each type that a comparison started from is monotone (see `is_monotone`).
    monotone: FxHashMap<TypeId, bool>,
}

impl ComparisonContext<'_> {
    /// Whether the generic is free in the environment and being inferred.
    fn is_target(&self, generic: TypeId, env: Option<usize>) -> bool {
        self.infer
            .as_ref()
            .is_some_and(|infer| infer.is_target(generic))
            && self.resolve(generic, env).is_none()
    }

    /// Whether the generics are the same generic before one of them was renamed (see
    /// `Inference::rename`).
    fn is_renaming(&self, lhs: TypeId, rhs: TypeId) -> bool {
        self.renamed.get(&lhs) == Some(&rhs) || self.renamed.get(&rhs) == Some(&lhs)
    }

    /// Whether the generics being inferred are on the right hand side.
    fn is_inferring(&self) -> bool {
        !self.flipped
            && self
                .infer
                .as_ref()
                .is_some_and(|infer| !infer.generics.is_empty())
    }

    fn references_rigid(&self, arena: &Arena<Type>, ty: TypeId) -> bool {
        self.rigid
            .iter()
            .any(|&generic| references_generic(arena, ty, generic, |_| Vec::new()))
    }

    /// Caches the generics that the type references (see `referenced_generics`) in `references`.
    fn cache_references(&mut self, arena: &Arena<Type>, ty: TypeId) {
        self.references
            .entry(ty)
            .or_insert_with(|| referenced_generics(arena, ty));
    }

    /// Adds a candidate for a generic being inferred. If it references the generics of a function
    /// being instantiated, it's deferred until they've been instantiated (see `DeferredCandidate`).
    fn add_candidate(
        &mut self,
        arena: &Arena<Type>,
        generic: TypeId,
        kind: CandidateKind,
        ty: TypeId,
        env: Option<usize>,
    ) {
        let deferred = env.is_some() && {
            self.cache_references(arena, ty);
            self.references[&ty]
                .iter()
                .any(|&other| self.is_instance(other, env))
        };

        let branches = self.branch.into_iter().collect();
        let index = self
            .infer
            .as_mut()
            .unwrap()
            .push(generic, kind, ty, branches);

        if deferred {
            self.deferred.push(DeferredCandidate {
                generic,
                kind,
                index,
                env,
            });
        }

        self.candidates += 1;
    }

    /// Starts comparing the value with the members of a union while inferring, so that the
    /// candidates collected under each member are recorded under it, and returns the choice.
    fn open_choice(&mut self, members: usize) -> Option<usize> {
        let parent = self.branch;
        self.infer
            .as_mut()
            .map(|infer| infer.open_choice(parent, members))
    }

    /// Compares the value with a member of the union (or its fallback) with the candidates it
    /// collects recorded under it, and records what it gave.
    fn compare_member(
        &mut self,
        choice: Option<usize>,
        member: usize,
        compare: impl FnOnce(&mut Self) -> Comparison,
    ) -> Comparison {
        let Some(choice) = choice else {
            return compare(self);
        };

        let branch = BranchId { choice, member };
        let parent = self.branch.replace(branch);
        let result = compare(self);
        self.branch = parent;
        self.infer.as_mut().unwrap().record(branch, result);
        result
    }

    /// Records the bounds of a finished instance once a candidate has been deferred, unless one of
    /// them references a generic bound in the comparison.
    fn record_instantiation(&mut self, arena: &Arena<Type>, binder: usize, instance: &Instance) {
        if self.deferred.is_empty() {
            return;
        }

        let mut free = true;

        for &(ty, env) in instance.lower.iter().chain(&instance.upper) {
            self.cache_references(arena, ty);
            free &= self.references[&ty]
                .iter()
                .all(|&generic| self.resolve(generic, env).is_none());
        }

        if free {
            self.instantiations.insert(
                binder,
                InferenceCandidates {
                    lower: instance.lower.iter().map(|&(ty, _)| ty).collect(),
                    upper: instance.upper.iter().map(|&(ty, _)| ty).collect(),
                    held: Vec::new(),
                },
            );
        }
    }

    /// Replaces the generics of the instantiated functions that the deferred candidates reference
    /// with what they're instantiated as (see `DeferredCandidate`).
    fn resolve_deferred(&mut self, arena: &mut Arena<Type>, builtins: &BuiltinTypes) {
        // What each binder is instantiated as, for lower (and held) and upper candidates
        let mut picked: FxHashMap<(usize, bool), TypeId> = FxHashMap::default();

        for DeferredCandidate {
            generic,
            kind,
            index,
            env,
        } in mem::take(&mut self.deferred)
        {
            let infer = self.infer.as_mut().unwrap();
            let ty = infer.candidates_mut(generic).list_mut(kind)[index];
            let upper = matches!(kind, CandidateKind::Upper);
            let mut mappings = HashMap::new();

            self.cache_references(arena, ty);

            for &other in &self.references[&ty] {
                let Some(binder) = self.resolve(other, env) else {
                    continue;
                };

                let Some(instance) = self.instantiations.get(&binder) else {
                    continue;
                };

                let instantiated = *picked.entry((binder, upper)).or_insert_with(|| {
                    // Without bounds of the candidate's kind, the least instantiation is `Never`
                    // and the greatest is `Any`
                    if upper {
                        let upper = InferenceCandidates {
                            upper: instance.upper.clone(),
                            ..Default::default()
                        };
                        pick_inferred_type(arena, builtins, &self.renamed, &upper)
                            .unwrap_or(builtins.permissive_any)
                    } else if instance.lower.is_empty() {
                        builtins.never
                    } else {
                        pick_inferred_type(arena, builtins, &self.renamed, instance).unwrap()
                    }
                });

                mappings.insert(other, instantiated);
            }

            if !mappings.is_empty() {
                let ty = substitute_with_mappings(arena, ty, &mappings);
                let infer = self.infer.as_mut().unwrap();
                infer.candidates_mut(generic).list_mut(kind)[index] = ty;
            }
        }
    }

    /// Pushes binders for a function's generics onto the environment, and returns the new one.
    fn bind(
        &mut self,
        mut env: Option<usize>,
        generics: &[TypeId],
        instantiate: bool,
    ) -> Option<usize> {
        for &generic in generics {
            let instance = instantiate.then(|| {
                self.instances.push(Instance::default());
                self.instances.len() - 1
            });

            self.binders.push(Binder {
                generic,
                parent: env,
                instance,
                merged: None,
                renaming: Vec::new(),
            });
            env = Some(self.binders.len() - 1);
        }

        env
    }

    /// Returns the binder that the generic is bound to in the environment, or `None` if it's free.
    fn resolve(&self, generic: TypeId, env: Option<usize>) -> Option<usize> {
        resolve(&self.binders, generic, env)
    }

    /// Returns the instance of the binder, if it's being instantiated.
    fn instance_of(&self, binder: usize) -> Option<usize> {
        let instance = self.binders[binder].instance?;
        (instance >= self.instantiable_from).then_some(instance)
    }

    /// Whether the generic is being instantiated.
    fn is_instance(&self, generic: TypeId, env: Option<usize>) -> bool {
        self.resolve(generic, env)
            .is_some_and(|binder| self.instance_of(binder).is_some())
    }

    /// Adds a bound to the instance of the generic, renaming it into the binders of each instance
    /// that the generic's has been merged into.
    fn add_bound(&mut self, generic: TypeId, env: Option<usize>, mut bound: Bound, upper: bool) {
        let mut binder = binder_of(&self.binders, generic, env).unwrap();

        while let Some(into) = self.binders[binder].merged {
            bound.1 = self.rename(bound.1, binder);
            binder = into;
        }

        let instance = &mut self.instances[self.binders[binder].instance.unwrap()];

        if upper {
            instance.upper.push(bound);
        } else {
            instance.lower.push(bound);
        }
    }

    /// Returns an environment in which the rigid binders that the merged binder renames resolve
    /// to the ones they're renamed to.
    fn rename(&mut self, env: Option<usize>, binder: usize) -> Option<usize> {
        let mut renamed = env;

        for (from, into) in self.binders[binder].renaming.clone() {
            let generic = self.binders[from].generic;

            if self.resolve(generic, env) == Some(from) {
                self.binders.push(Binder {
                    generic,
                    parent: renamed,
                    instance: None,
                    merged: Some(into),
                    renaming: Vec::new(),
                });
                renamed = Some(self.binders.len() - 1);
            }
        }

        renamed
    }

    /// Merges the instances of two binders, so that they're the same generic from then on. The
    /// rigid binders that the comparison they're merged at renames from `b`'s side to `a`'s are
    /// renamed in their bounds, since each instance's bounds are in terms of its own side's. They're
    /// merged into the one that was bound first, since its comparison finishes last.
    fn merge(&mut self, a: usize, b: usize, renaming: &[(usize, usize)]) {
        let (a, b) = (root(&self.binders, a), root(&self.binders, b));

        if a == b {
            return;
        }

        let (into, from) = (a.min(b), a.max(b));

        self.binders[from].merged = Some(into);
        self.binders[from].renaming = if from == b {
            renaming.to_vec()
        } else {
            renaming.iter().map(|&(new, old)| (old, new)).collect()
        };

        let instance = self.binders[from].instance.unwrap();
        let Instance {
            lower,
            upper,
            fresh,
            mut renamed,
        } = mem::take(&mut self.instances[instance]);
        let generic = self.binders[from].generic;

        // The bounds are renamed, so the binders they're renamed to may be renamed as well
        if fresh.is_some() {
            let renaming = &self.binders[from].renaming;
            renamed.extend(renaming.iter().flat_map(|&(from, into)| [from, into]));
        }

        let into = &mut self.instances[self.binders[into].instance.unwrap()];
        into.fresh = into.fresh.into_iter().chain(fresh).min();
        into.renamed.extend(renamed);

        for (bounds, upper) in [(lower, false), (upper, true)] {
            for bound in bounds {
                self.add_bound(generic, Some(from), bound, upper);
            }
        }
    }

    /// Returns an environment in which the binders that the instance's missing bounds have other
    /// generics in place of resolve to new rigid binders.
    fn refresh(&mut self, env: Option<usize>, instance: &Instance) -> Option<usize> {
        let (Some(fresh), mut refreshed, mut current) = (instance.fresh, env, env) else {
            return env;
        };

        while let Some(index) = current {
            let generic = self.binders[index].generic;
            let binder = root(&self.binders, index);

            if (binder >= fresh || instance.renamed.contains(&binder))
                && binder_of(&self.binders, generic, env) == Some(index)
            {
                self.binders.push(Binder {
                    generic,
                    parent: refreshed,
                    instance: None,
                    merged: None,
                    renaming: Vec::new(),
                });
                refreshed = Some(self.binders.len() - 1);
            }

            current = self.binders[index].parent;
        }

        refreshed
    }

    /// Whether comparing the type (and the types it contains) is the least fixed point of monotone
    /// equations over the comparisons it reaches, which is what `FixpointMemo` needs. Comparing is
    /// built from `max`, `min` and constants (the fallbacks that are only compared when a union
    /// doesn't hold are a `min` whose result doesn't depend on them otherwise), and returning
    /// early at a comparison in progress gives `Assign` (the least value), or its approximation
    /// while `FixpointMemo` is iterating, as long as:
    ///
    /// - The type is contractive, since a cycle that doesn't pass through a pair or function gives
    ///   `Invalid` instead.
    /// - Each generic function's parameters and return type either are one of its generics or
    ///   don't reference them. The generics being instantiated then only collect bounds directly,
    ///   from the same comparisons whenever the function is compared, so the bounds that are
    ///   checked against each other are always the same. Otherwise, which bounds they collect
    ///   would depend on what's in progress (and they would be merged when revisited).
    ///
    /// Inference also collects candidates as a side effect, so it isn't used then.
    fn is_monotone(&mut self, arena: &Arena<Type>, id: TypeId) -> bool {
        if let Some(&monotone) = self.monotone.get(&id) {
            return monotone;
        }

        let reachable = reachable(arena, id);
        let monotone = !has_non_contractive_cycle(arena, &reachable)
            && reachable.iter().all(|&id| instantiates_directly(arena, id));
        self.monotone.insert(id, monotone);
        monotone
    }

    /// Interns what the generics referenced by the types are bound to, and returns the binders in
    /// the order they're numbered.
    fn bindings(&mut self, arena: &Arena<Type>, lhs: TypeId, rhs: TypeId) -> (usize, Vec<usize>) {
        let mut bindings = Vec::new();
        let mut binders = Vec::new();

        for (is_rhs, ty, env) in [(false, lhs, self.lhs_env), (true, rhs, self.rhs_env)] {
            if env.is_none() {
                continue;
            }

            self.cache_references(arena, ty);

            for &generic in &self.references[&ty] {
                let Some(binder) = self.resolve(generic, env) else {
                    continue;
                };

                let index = number(&mut binders, binder);

                let binding = if self.instance_of(binder).is_some() {
                    Binding::Instance(index)
                } else {
                    Binding::Rigid(index)
                };

                bindings.push((is_rhs, generic, binding));
            }
        }

        (self.bindings.insert_full(bindings).0, binders)
    }
}

/// Returns the binder that the generic is bound to in the environment (or the one it was merged
/// into), or `None` if it's free.
fn resolve(binders: &[Binder], generic: TypeId, env: Option<usize>) -> Option<usize> {
    binder_of(binders, generic, env).map(|index| root(binders, index))
}

/// Returns the binder that the generic is bound to in the environment, even if it has since been
/// merged into another one, or `None` if it's free.
fn binder_of(binders: &[Binder], generic: TypeId, mut env: Option<usize>) -> Option<usize> {
    while let Some(index) = env {
        if binders[index].generic == generic {
            return Some(index);
        }

        env = binders[index].parent;
    }

    None
}

/// Returns the binder that the binder has been merged into, or itself if it hasn't been.
fn root(binders: &[Binder], mut index: usize) -> usize {
    while let Some(merged) = binders[index].merged {
        index = merged;
    }

    index
}

/// Returns `false` if the type is a generic function type with a parameter or return type that
/// references its generics without being one of them.
fn instantiates_directly(arena: &Arena<Type>, id: TypeId) -> bool {
    let Type::Function(function) = &arena[id] else {
        return true;
    };

    function.param_types().chain([function.ret]).all(|ty| {
        function.generics.contains(&ty)
            || !function
                .generics
                .iter()
                .any(|&generic| references_generic(arena, ty, generic, |_| Vec::new()))
    })
}

/// Returns the generics that the type references, other than where they're bound by a function
/// type, sorted by id.
fn referenced_generics(arena: &Arena<Type>, ty: TypeId) -> Vec<TypeId> {
    let mut generics: Vec<TypeId> = free_generics(arena, ty)
        .into_iter()
        .filter(|&generic| references_generic(arena, ty, generic, |_| Vec::new()))
        .collect();
    generics.sort_unstable();
    generics
}

/// Returns the position of the id in the list, adding it if it's not there yet.
fn number(ids: &mut Vec<usize>, id: usize) -> usize {
    ids.iter()
        .position(|&other| other == id)
        .unwrap_or_else(|| {
            ids.push(id);
            ids.len() - 1
        })
}

/// Returns `true` if the value is assignable to the type regardless of what the generics being
/// inferred are.
fn holds_without_inference(
    arena: &Arena<Type>,
    builtins: &BuiltinTypes,
    ctx: &mut ComparisonContext<'_>,
    lhs: TypeId,
    rhs: TypeId,
    lhs_semantic: Option<TypeId>,
    rhs_semantic: Option<TypeId>,
) -> bool {
    let mut uninferred = ctx.uninferred.take().unwrap_or_else(|| {
        Box::new(ComparisonContext {
            renamed: ctx.renamed.clone(),
            ..Default::default()
        })
    });

    // The binders are shared, but none of them are instantiated
    mem::swap(&mut uninferred.binders, &mut ctx.binders);
    mem::swap(&mut uninferred.instances, &mut ctx.instances);
    uninferred.lhs_env = ctx.lhs_env;
    uninferred.rhs_env = ctx.rhs_env;
    uninferred.instantiable_from = uninferred.instances.len();

    let result = compare_impl(
        arena,
        builtins,
        &mut uninferred,
        lhs,
        rhs,
        lhs_semantic,
        rhs_semantic,
    );
    mem::swap(&mut uninferred.binders, &mut ctx.binders);
    mem::swap(&mut uninferred.instances, &mut ctx.instances);
    ctx.uninferred = Some(uninferred);
    result == Comparison::Assign
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

    let mut ctx = ComparisonContext {
        renamed: infer
            .as_ref()
            .map(|infer| infer.renamed.clone())
            .unwrap_or_default(),
        infer,
        ..Default::default()
    };
    let result = compare_impl(arena, builtins, &mut ctx, lhs, rhs, None, None);
    ctx.resolve_deferred(arena, builtins);

    if let Some((lhs_name, rhs_name)) = &names {
        trace!("Comparison from {lhs_name} to {rhs_name} yielded {result:?}");
    }

    result
}

/// Collects inference candidates for the generics in `ty` from the type it's expected to be
/// assignable to, such as a function's return type and the type expected of the call.
pub fn infer_from_expected(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    ty: TypeId,
    expected: TypeId,
    infer: &mut Inference,
) {
    let ty = substitute(arena, ty);
    let expected = substitute(arena, expected);

    // The generics being inferred are on the left hand side, so the comparison starts flipped
    let mut ctx = ComparisonContext {
        renamed: infer.renamed.clone(),
        infer: Some(infer),
        flipped: true,
        ..Default::default()
    };
    compare_impl(arena, builtins, &mut ctx, ty, expected, None, None);
    ctx.resolve_deferred(arena, builtins);
}

pub fn compare(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    lhs: TypeId,
    rhs: TypeId,
) -> Comparison {
    compare_with_inference(arena, builtins, lhs, rhs, None)
}

/// Like `compare`, but the generics that were renamed from another generic are compared as it (see
/// `Inference::rename`), as they are while inferring.
pub(crate) fn compare_renamed(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    renamed: &FxHashMap<TypeId, TypeId>,
    lhs: TypeId,
    rhs: TypeId,
) -> Comparison {
    if renamed.is_empty() {
        return compare(arena, builtins, lhs, rhs);
    }

    let lhs = substitute(arena, lhs);
    let rhs = substitute(arena, rhs);
    let mut ctx = ComparisonContext {
        renamed: renamed.clone(),
        ..Default::default()
    };
    compare_impl(arena, builtins, &mut ctx, lhs, rhs, None, None)
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
    if ctx.stack.is_empty() {
        ctx.fixpoint = ctx.infer.is_none()
            && ctx.lhs_env.is_none()
            && ctx.rhs_env.is_none()
            && ctx.is_monotone(arena, lhs)
            && ctx.is_monotone(arena, rhs);

        if ctx.fixpoint {
            ctx.fixpoint_memo.start();

            let result = grow_stack(|| {
                compare_impl_inner(arena, builtins, ctx, lhs, rhs, lhs_semantic, rhs_semantic)
            });

            if !ctx.fixpoint_memo.abandoned() {
                return result;
            }

            // The single pass took too long, so it's repeated until the approximations it reaches
            // stop changing instead (see `FixpointMemo`)
            loop {
                ctx.fixpoint_memo.start_round();

                let result = grow_stack(|| {
                    compare_impl_inner(arena, builtins, ctx, lhs, rhs, lhs_semantic, rhs_semantic)
                });

                if ctx.fixpoint_memo.finish_round() {
                    return result;
                }
            }
        }
    }

    grow_stack(|| compare_impl_inner(arena, builtins, ctx, lhs, rhs, lhs_semantic, rhs_semantic))
}

fn compare_impl_inner(
    arena: &Arena<Type>,
    builtins: &BuiltinTypes,
    ctx: &mut ComparisonContext<'_>,
    lhs: TypeId,
    rhs: TypeId,
    lhs_semantic: Option<TypeId>,
    rhs_semantic: Option<TypeId>,
) -> Comparison {
    step();

    // The comparison that started from an empty stack is unwinding, to iterate instead
    if ctx.fixpoint && ctx.fixpoint_memo.abandoned() {
        return Comparison::Assign;
    }

    let (bindings, binders) = ctx.bindings(arena, lhs, rhs);
    let key = (lhs, rhs, ctx.flipped, bindings, lhs_semantic, rhs_semantic);

    // Generics that are being instantiated outside of the comparison collect bounds from it as a
    // side effect, so it's only memoized if it doesn't refer to any. The ones bound inside of it
    // are checked before it finishes.
    let memoize = compare_memo_enabled()
        && !ctx.bindings[bindings]
            .iter()
            .any(|&(_, _, binding)| matches!(binding, Binding::Instance(_)));

    if let Some(index) = ctx.stack.get_index_of(&key) {
        // In fixpoint mode, it's the least value in a single pass, or its approximation from the
        // previous round while iterating
        let value = if ctx.fixpoint {
            let key = memoize.then_some(&key);
            ctx.fixpoint_memo.reached(index, key, Comparison::Assign)
        } else {
            ctx.memo.reached(index);
            Comparison::Assign
        };

        // Revisiting a comparison is only sound to treat as assignable if we've passed through
        // a pair or function since, otherwise a non-contractive type like `type A = B; type B = A;`
        // would be assignable to anything. Those should be rejected before reaching this point.
        if ctx.guards.last().is_none_or(|&guard| guard < index) {
            return Comparison::Invalid;
        }

        // If it's revisited with other binders, the bounds that their instances would collect
        // from it are only collected for the original ones, so they're merged. Rigid binders
        // aren't merged, since they can be different types, but the comparison holds for them
        // just the same as long as the instances' bounds are renamed to match. That's not
        // possible for an instance that's revisited as itself, so it's marked instead.
        let (bound, original_binders) = ctx.stack_binders[index].clone();
        let mut instances = Vec::new();
        let mut renaming = Vec::new();

        for &(_, _, binding) in &ctx.bindings[bindings] {
            let (Binding::Instance(i) | Binding::Rigid(i)) = binding;
            let original = root(&ctx.binders, original_binders[i]);
            let binder = root(&ctx.binders, binders[i]);

            match binding {
                Binding::Instance(_) => instances.push((original, binder)),
                Binding::Rigid(_) if original != binder => renaming.push((binder, original)),
                Binding::Rigid(_) => {}
            }
        }

        for (original, binder) in instances {
            // An earlier merge may have made them the same
            if root(&ctx.binders, original) == root(&ctx.binders, binder) {
                let instance = ctx.binders[root(&ctx.binders, original)].instance.unwrap();
                let instance = &mut ctx.instances[instance];
                instance.fresh = instance.fresh.into_iter().chain([bound]).min();
                let renamed = renaming
                    .iter()
                    .flat_map(|&(binder, original)| [binder, original]);
                instance.renamed.extend(renamed);
            } else {
                ctx.merge(original, binder, &renaming);
            }
        }

        return value;
    }

    if memoize {
        let result = if ctx.fixpoint {
            ctx.fixpoint_memo.get(&key, &ctx.stack)
        } else {
            ctx.memo.get(&key, ctx.stack.iter().copied())
        };

        if let Some(result) = result {
            return result;
        }
    }

    let frame = if ctx.fixpoint {
        Frame::Fixpoint(ctx.fixpoint_memo.enter(ctx.stack.len()))
    } else {
        Frame::Exact(ctx.memo.enter(ctx.stack.len()))
    };
    let candidates = ctx.candidates;

    ctx.stack.insert(key);
    ctx.stack_binders.push((ctx.binders.len(), binders));

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
        (Type::Generic(_), _) if ctx.is_instance(lhs, ctx.lhs_env) => {
            ctx.add_bound(lhs, ctx.lhs_env, (rhs, ctx.rhs_env), true);
            Comparison::Assign
        }
        (_, Type::Generic(_)) if ctx.is_instance(rhs, ctx.rhs_env) => {
            ctx.add_bound(rhs, ctx.rhs_env, (lhs, ctx.lhs_env), false);
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
                if !ctx.flipped {
                    ctx.rigid.extend(rhs.generics.iter().copied());
                }

                // The left hand side's generics are instantiated with whatever the right hand
                // side needs, whereas the right hand side's generics stay rigid, since the
                // function must work for any of them.
                let (lhs_env, rhs_env) = (ctx.lhs_env, ctx.rhs_env);
                let first = ctx.binders.len();
                ctx.lhs_env = ctx.bind(lhs_env, &lhs.generics, true);
                ctx.rhs_env = ctx.bind(rhs_env, &rhs.generics, false);

                // The return type is covariant, since the value returned by the function flows
                // out to whoever expects the right hand side's return type.
                let mut result = compare_impl(arena, builtins, ctx, lhs.ret, rhs.ret, None, None);

                // The parameters are contravariant, since the arguments passed according to the
                // right hand side's parameter types flow into the left hand side's function.
                // A `Cast` is still fine here, since casting a function's type just reinterprets
                // its arguments (which requires the same representation) at the call site.
                ctx.flipped = !ctx.flipped;
                mem::swap(&mut ctx.lhs_env, &mut ctx.rhs_env);

                for (lhs_param, rhs_param) in lhs.params.iter().zip(&rhs.params) {
                    result = max(
                        result,
                        compare_impl(arena, builtins, ctx, rhs_param.ty, lhs_param.ty, None, None),
                    );
                }

                ctx.flipped = !ctx.flipped;

                // The instances that were merged into another are checked along with it
                let mut bounds = Vec::new();

                for index in first..first + lhs.generics.len() {
                    let binder = &ctx.binders[index];

                    if binder.merged.is_none() {
                        let instance = mem::take(&mut ctx.instances[binder.instance.unwrap()]);
                        ctx.record_instantiation(arena, index, &instance);
                        bounds.push(instance);
                    }
                }

                // There is an instantiation of each generic if every type that flows into it is
                // assignable to every type it flows into (such as their union, or `Never`).
                // The bounds come from the other side, where the generics of any enclosing
                // functions being instantiated are rigid, so they're hidden while checking.
                let instantiable_from =
                    mem::replace(&mut ctx.instantiable_from, ctx.instances.len());

                for instance in bounds {
                    for &(lower, lower_env) in &instance.lower {
                        for &(upper, upper_env) in &instance.upper {
                            let mut envs = vec![(lower_env, upper_env)];

                            // Each missing bound is checked against the others like the original
                            if instance.fresh.is_some() {
                                envs.push((ctx.refresh(lower_env, &instance), upper_env));
                                envs.push((lower_env, ctx.refresh(upper_env, &instance)));
                            }

                            for (lower_env, upper_env) in envs {
                                ctx.lhs_env = lower_env;
                                ctx.rhs_env = upper_env;
                                result = max(
                                    result,
                                    compare_impl(arena, builtins, ctx, lower, upper, None, None),
                                );
                            }
                        }
                    }
                }

                ctx.instantiable_from = instantiable_from;
                ctx.lhs_env = lhs_env;
                ctx.rhs_env = rhs_env;
                ctx.guards.pop().unwrap();

                result
            }
        }
        (Type::Generic(_), Type::Generic(_))
            if (lhs == rhs && ctx.resolve(lhs, ctx.lhs_env) == ctx.resolve(rhs, ctx.rhs_env))
                || (ctx.is_renaming(lhs, rhs)
                    && ctx.resolve(lhs, ctx.lhs_env).is_none()
                    && ctx.resolve(rhs, ctx.rhs_env).is_none()) =>
        {
            Comparison::Assign
        }
        (Type::Generic(_), _)
            if ctx.flipped
                && ctx.is_target(lhs, ctx.lhs_env)
                && !ctx.references_rigid(arena, rhs) =>
        {
            debug!(
                "Inferring {} must be assignable to {}",
                stringify_impl(arena, lhs, &mut IndexMap::new()),
                stringify_impl(arena, rhs, &mut IndexMap::new())
            );
            ctx.add_candidate(arena, lhs, CandidateKind::Upper, rhs, ctx.rhs_env);
            Comparison::Assign
        }
        (_, Type::Generic(_))
            if !ctx.flipped
                && ctx.is_target(rhs, ctx.rhs_env)
                && !ctx.references_rigid(arena, lhs) =>
        {
            debug!(
                "Inferring {} could include {}",
                stringify_impl(arena, rhs, &mut IndexMap::new()),
                stringify_impl(arena, lhs, &mut IndexMap::new())
            );
            let kind = if ctx.holding {
                CandidateKind::Held
            } else {
                CandidateKind::Lower
            };

            ctx.add_candidate(arena, rhs, kind, lhs, ctx.lhs_env);
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
        // Aliases are unwrapped before structs, so that structs are compared to each other
        // directly rather than to each other's inner types, which would lose their semantics
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
        (Type::Struct(lhs), Type::Struct(rhs)) => max(
            compare_impl(arena, builtins, ctx, lhs.inner, rhs.inner, None, None),
            if lhs.semantic == rhs.semantic {
                Comparison::Assign
            } else {
                Comparison::Cast
            },
        ),
        // Likewise, a union on the left hand side is split into its members before a struct on
        // the right hand side is unwrapped
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
        (Type::Generic(_) | Type::Function(_), Type::Union(rhs_union)) => {
            // A generic is opaque, so it can only be assigned to a union if it's assignable to
            // one of the members (such as `T` to `T | nil`). Otherwise, fall back to whatever
            // the generic could be at runtime, as with any other type. Likewise, a function's
            // runtime structure is only a fallback for when no member holds it, so that it
            // doesn't add to the generics in the union (#349).
            let choice = ctx.open_choice(rhs_union.types.len());
            let mut result = Comparison::Invalid;

            for (member, &id) in rhs_union.types.iter().enumerate() {
                result = min(
                    result,
                    ctx.compare_member(choice, member, |ctx| {
                        compare_impl(arena, builtins, ctx, lhs, id, lhs_semantic, rhs_semantic)
                    }),
                );
            }

            if result == Comparison::Assign {
                result
            } else {
                min(
                    result,
                    ctx.compare_member(choice, FALLBACK, |ctx| {
                        compare_impl(
                            arena,
                            builtins,
                            ctx,
                            builtins.recursive_any,
                            rhs,
                            lhs_semantic,
                            rhs_semantic,
                        )
                    }),
                )
            }
        }
        (Type::Generic(_) | Type::Any | Type::Function(_), _) => compare_impl(
            arena,
            builtins,
            ctx,
            builtins.recursive_any,
            rhs,
            lhs_semantic,
            rhs_semantic,
        ),
        (_, Type::Union(rhs_union)) => {
            // A generic being inferred doesn't need to include a value that a member already
            // holds regardless of the generic, such as `nil` when passing `Int | nil` to
            // `T | nil` or `Tree<T> | nil`, so it's only collected as a last resort.
            let holding = ctx.holding;

            ctx.holding |= ctx.is_inferring()
                && rhs_union.types.iter().any(|&id| {
                    holds_without_inference(
                        arena,
                        builtins,
                        ctx,
                        lhs,
                        id,
                        lhs_semantic,
                        rhs_semantic,
                    )
                });

            let choice = ctx.open_choice(rhs_union.types.len());
            let mut result = Comparison::Invalid;

            for (member, &id) in rhs_union.types.iter().enumerate() {
                result = min(
                    result,
                    ctx.compare_member(choice, member, |ctx| {
                        compare_impl(arena, builtins, ctx, lhs, id, lhs_semantic, rhs_semantic)
                    }),
                );
            }

            ctx.holding = holding;

            // A struct can still be cast to a union that holds its inner type as a whole, such as
            // `struct W { ...value: Int | (Int, Int) }` to `Int | (Int, Int)`
            if result == Comparison::Invalid
                && let Type::Struct(lhs) = arena[lhs].clone()
            {
                result = ctx.compare_member(choice, FALLBACK, |ctx| {
                    max(
                        compare_impl(arena, builtins, ctx, lhs.inner, rhs, None, None),
                        Comparison::Cast,
                    )
                });
            }

            result
        }
        // A struct is only unwrapped once a union on the right hand side has been split into its
        // members, so that it's compared to any struct among them directly
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
        (_, Type::Never) => Comparison::Invalid,
        (_, Type::Function(_)) => Comparison::Invalid,
    };

    ctx.stack.pop().unwrap();
    ctx.stack_binders.pop().unwrap();

    // Inference candidates are collected as a side effect of comparing, so a comparison that
    // collected any can't be skipped
    let memo_key = (memoize && ctx.candidates == candidates).then_some(key);

    match frame {
        Frame::Exact(frame) => ctx.memo.exit(frame, key, memo_key, result),
        Frame::Fixpoint(frame) => ctx.fixpoint_memo.exit(frame, memo_key, result, &ctx.stack),
    }

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
    grow_stack(|| semantics_of_impl_inner(arena, id, visited))
}

fn semantics_of_impl_inner(
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

    use crate::{Atom, FunctionParameter, FunctionType, Generic, Pair, Type, compare};

    use super::*;

    #[test]
    fn test_instantiates_directly() {
        let mut arena = Arena::new();
        let builtins = BuiltinTypes::new(&mut arena);
        let generic = arena.alloc(Type::Generic(Generic { name: None }));

        let function = |arena: &mut Arena<Type>, param: TypeId, ret: TypeId| {
            arena.alloc(Type::Function(FunctionType {
                generics: vec![generic],
                params: vec![FunctionParameter::new("x", param)],
                nil_terminated: true,
                ret,
            }))
        };

        // `fn<T>(x: T) -> Int`
        let direct = function(&mut arena, generic, builtins.int);

        // `fn<T>(x: fn<T>(x: T) -> Int) -> T`, where the inner function binds its own `T`
        let shadowed = function(&mut arena, direct, generic);

        // `fn<T>(x: (T, Int)) -> Int`, where `T` is only part of the parameter's type
        let pair = arena.alloc(Type::Pair(Pair::new(generic, builtins.int)));
        let nested = function(&mut arena, pair, builtins.int);

        assert!(instantiates_directly(&arena, direct));
        assert!(instantiates_directly(&arena, shadowed));
        assert!(!instantiates_directly(&arena, nested));
    }

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
