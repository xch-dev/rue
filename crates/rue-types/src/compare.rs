use std::{
    cmp::{max, min},
    collections::{HashMap, HashSet},
    mem,
    rc::Rc,
};

use id_arena::Arena;
use indexmap::IndexMap;
use log::{Level, debug, log_enabled, trace};

use crate::{
    AtomRestriction, AtomSemantic, BranchId, BuiltinTypes, CandidateKind, Choices, FALLBACK,
    FixpointFrame, FixpointMemo, Frame as MemoFrame, FxHashMap, FxIndexSet, Generic, Inference,
    InferenceCandidates, InferenceSnapshot, InstancePolicy, Join, Memo, Type, TypeId,
    compare_memo_enabled, comparison_step_limit, free_generics, grow_stack,
    has_non_contractive_cycle, reachable, references_generic, step, stringify_impl, substitute,
    substitute_with_mappings,
    summary::{Constraint, Summary, Var},
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
/// comparison has finished, each of them is replaced with a var that stands for its instance (see
/// `ComparisonContext::finish`), which the inference resolves like its other vars (see
/// `Inference::add_instance`). An instance that's left out of
/// `ComparisonContext::instantiations` is left as is.
#[derive(Debug, Clone, Copy)]
struct DeferredCandidate {
    generic: TypeId,
    kind: CandidateKind,
    index: usize,
    /// The environment that the candidate's generics are resolved in.
    env: Option<usize>,
}

/// Where the generics that a bound of an instance references are bound, which decides whether it's
/// in scope where the candidates that reference the instance are used (see `instance_bounds`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BoundScope {
    /// Outside of the comparison (or nowhere), so it's in scope.
    Free,
    /// Inside of it, by functions that are being instantiated themselves, such as `Id`'s `U` in
    /// #357.
    Instantiated,
    /// Inside of it, and at least one of them is rigid, such as `U` in `fn<U>(a: U) -> B`.
    Rigid,
}

/// The bounds that the var for an instance takes, or `None` if the instance is left as is in the
/// candidates that reference it. Without joint solving, an instance with a bound that isn't in
/// scope is left as is (#357). With it, such a bound is left out if the generics it references are
/// instantiated themselves, since they can be instantiated as whatever this instance is. One that
/// references a rigid generic is only met by `Any` from below (or `Never` from above) in scope.
fn instance_bounds(
    builtins: &BuiltinTypes,
    policy: InstancePolicy,
    bounds: &[(CandidateKind, TypeId, BoundScope)],
) -> Option<InferenceCandidates> {
    let mut candidates = InferenceCandidates::default();

    for &(kind, ty, scope) in bounds {
        let ty = match (scope, policy) {
            (BoundScope::Free, _) => ty,
            (_, InstancePolicy::Eager) => return None,
            (BoundScope::Instantiated, InstancePolicy::Joint) => continue,
            // An instance's bounds are only lower and upper bounds
            (BoundScope::Rigid, InstancePolicy::Joint) => match kind {
                CandidateKind::Upper => builtins.never,
                CandidateKind::Lower | CandidateKind::Held => builtins.permissive_any,
            },
        };

        candidates.list_mut(kind).push(ty);
    }

    Some(candidates)
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

/// A bound of a generic being instantiated, along with the environment it's resolved in and the
/// union members it was collected under.
type Bound = (TypeId, Option<usize>, BoundBranches);

/// The innermost union members that a bound of a generic being instantiated was collected under:
/// in `ComparisonContext::choices`, which decides whether the bound is checked (#360), and in the
/// inference, if there is one, which the candidates that checking it collects are recorded under.
#[derive(Debug, Default, Clone, Copy)]
struct BoundBranches {
    local: Option<BranchId>,
    infer: Option<BranchId>,
}

/// A union that a value is being compared with the members of (see
/// `ComparisonContext::open_choice`).
#[derive(Debug, Clone, Copy)]
struct OpenChoice {
    local: Option<usize>,
    infer: Option<usize>,
}

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
    /// The binder that this one stands for, if it's an alias: one of the binders that an
    /// environment built for a bound applied from a summary resolves its generics to (see
    /// `ComparisonContext::apply_summary`).
    alias: Option<usize>,
}

/// The bounds collected for a generic being instantiated, which are checked against each other
/// once the comparison that bound it has finished, and again by an enclosing function comparison
/// if checking that one's bounds adds to them.
#[derive(Debug, Default)]
struct Instance {
    lower: Vec<Bound>,
    upper: Vec<Bound>,
}

/// A comparison in progress.
#[derive(Debug)]
struct StackFrame {
    /// The number of binders when it was entered. The ones from here on were bound inside of it.
    entered: usize,
    /// The binders that its key references, in the order they're numbered (see `Binding`).
    binders: Vec<usize>,
    /// What a revisit of it is given while it's in progress: the bounds that it puts on the
    /// instances its key refers to, as far as they're known (see `Summary`). Each round of
    /// computing it that revisits it computes it again with the bounds that the round put on them,
    /// until that adds nothing (see `compare_impl_inner`). That's in exact mode: while iterating in
    /// fixpoint mode, `FixpointMemo`'s rounds do the same for every comparison (see `Outcome`).
    ///
    /// It starts out empty, or as what the rounds stopped at the last time the same comparison was
    /// computed (`ComparisonContext::approximations`). Otherwise, a comparison nested in many
    /// others that are revisited would be computed in a round of its own for each round of each of
    /// them. Every summary computed this way is at most the comparison's own (the least fixed
    /// point, which only depends on its key), since it starts from at most that and adding bounds
    /// only adds bounds, so starting from one still ends at it. The exception is a bound collected
    /// under a union member that matches with fewer bounds and not with more: the summary keeps it
    /// as a bound that matched, which can only make a revisit's result worse, never better.
    approximation: Summary,
    /// The result that a revisit of it is given while it's in progress: `Assign`, or in exact mode
    /// for types that nest generic functions' generics (see
    /// `ComparisonContext::root_monotonicity`), its result from the previous round, computed again
    /// like `approximation` until it stops changing.
    value: Comparison,
    /// Whether it was revisited during the current round.
    revisited: bool,
}

/// The state outside of a comparison when it was entered, from which `restore` undoes, and
/// `summary_since` reads, what a round of computing it changed (see `StackFrame::approximation`).
#[derive(Debug)]
struct Snapshot {
    /// For each instance that the key refers to, its position in the key's numbering, its index
    /// in `ComparisonContext::instances`, and its numbers of lower and upper bounds.
    instances: Vec<(usize, usize, usize, usize)>,
    /// The numbers of binders and instances.
    binders: usize,
    instance_count: usize,
    choices: usize,
    deferred: usize,
    infer: Option<InferenceSnapshot>,
}

/// A comparison's result and the bounds it puts on the instances its key refers to, if any, which
/// are what's cached for it, and what a revisit of it is given while iterating in fixpoint mode
/// (see `Summary`).
type Outcome = (Comparison, Option<Rc<Summary>>);

impl Join for Outcome {
    fn join(&self, other: &Self) -> Self {
        let summary = match (&self.1, &other.1) {
            (None, summary) | (summary, None) => summary.clone(),
            (Some(a), Some(b)) if Rc::ptr_eq(a, b) || a == b => Some(a.clone()),
            (Some(a), Some(b)) => Some(Rc::new(a.join(b))),
        };

        (self.0.max(other.0), summary)
    }
}

/// What the types that a comparison starts from are like, which decides how it's computed (see
/// `ComparisonContext::monotonicity`), from the most to the least that can be reused.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Monotone {
    /// Contractive, and every generic function's parameters and return type either are one of its
    /// generics or don't reference them. It's computed in fixpoint mode, starting in a single pass.
    Direct,
    /// Contractive, with generic functions that nest their generics. It's computed in fixpoint mode
    /// iterating from the start, with the summaries of the comparisons that refer to instances as
    /// part of their values, or in exact mode with a revisit given the previous round's result
    /// (see `StackFrame::value`).
    Nested,
    /// Not contractive. It's computed in exact mode.
    #[default]
    No,
}

/// The most rounds that a comparison is computed in before it gives up (see
/// `StackFrame::approximation`). Each round adds bounds to its summary, which is finite, so this
/// is only reached by a summary that's too symmetric to be put in a canonical form (see
/// `Summary::canonicalize`). No test reaches it, since no such summary has been found.
const MAX_ROUNDS: usize = 32;

#[derive(Debug, Default)]
pub(crate) struct ComparisonContext<'a> {
    infer: Option<&'a mut Inference>,
    stack: FxIndexSet<ComparisonKey>,
    /// For each comparison in `stack`, the binders it references and the result and summary that a
    /// revisit of it is given (see `StackFrame`).
    frames: Vec<StackFrame>,
    /// The result and summary that each comparison's rounds last stopped at, which the first round
    /// starts from the next time it's computed (see `StackFrame::value` and
    /// `StackFrame::approximation`). Starting from them only saves rounds: they're at most the
    /// comparison's own, so the rounds end at the same result and summary as from the start.
    approximations: FxHashMap<ComparisonKey, (Comparison, Summary)>,
    /// What the types that the comparison that started from an empty stack started from are like
    /// (see `monotonicity`). If they nest generic functions' generics (`Monotone::Nested`), a
    /// revisit in exact mode is given the previous round's result of the comparison it revisits,
    /// rather than `Assign` (see `StackFrame::value`). That's needed then: a union member that
    /// holds by revisiting a comparison can leave out another member's bounds (see
    /// `Choices::is_held`), so a comparison's result can depend on what its revisits are given in
    /// more than one step, and `Assign` isn't its least fixed point. Otherwise, comparing is built
    /// from `max`, `min` and constants, for which one round, with a revisit given `Assign`, already
    /// gives the least fixed point (see `FixpointMemo`).
    root_monotonicity: Monotone,
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
    /// The instances before this index are rigid: they belong to enclosing comparisons, or to the
    /// one whose bounds are being checked.
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
    /// The unions that values have been compared with the members of, whether inferring or not,
    /// which decide which bounds of the generics being instantiated are checked (#360).
    choices: Choices,
    /// The innermost union member in `choices` that the value is being compared with, which the
    /// bounds collected now are recorded under.
    local_branch: Option<BranchId>,
    /// The lowest index in `instances` of an instance that a bound has been added to, since
    /// `compare_member` last reset it.
    bounded: usize,
    /// The candidates that reference the generics of a function being instantiated, which are
    /// replaced with vars for their instances once the comparison has finished.
    deferred: Vec<DeferredCandidate>,
    /// Once a candidate has been deferred, the bounds of each instance that finishes afterward, by
    /// binder, as handed over by `ComparisonContext::instance_finished`, along with where the
    /// generics they reference are bound.
    instantiations: FxHashMap<usize, Vec<(CandidateKind, TypeId, BoundScope)>>,
    /// The generics being inferred that were renamed from another generic (see `Inference::rename`),
    /// kept here so that comparisons without the inference see them too.
    renamed: FxHashMap<TypeId, TypeId>,
    /// The context for comparisons that leave the generics being inferred rigid, which is kept
    /// so that its results are memoized for the whole comparison.
    uninferred: Option<Box<ComparisonContext<'static>>>,
    /// Completed comparisons, reused only where computing them again would do the same thing, along
    /// with the bounds they put on the instances their keys refer to, if any, which are put on the
    /// ones they're reused with.
    memo: Memo<ComparisonKey, ComparisonKey, Outcome>,
    /// Whether the comparison that started from an empty stack is the least fixed point of
    /// monotone equations (see `monotonicity`), so `fixpoint_memo` is used instead of `memo`.
    fixpoint: bool,
    /// Completed comparisons, reused while the comparisons they relied on are in progress, or
    /// approximations of them while iterating (see `FixpointMemo`).
    fixpoint_memo: FixpointMemo<ComparisonKey, Outcome>,
    /// How a comparison that starts from each type can be computed (see `monotonicity`).
    monotone: FxHashMap<TypeId, Monotone>,
    /// The steps taken by the comparison that started from an empty stack, which gives up once
    /// there are more than `COMPARISON_STEP_LIMIT`.
    steps: usize,
    /// Whether a comparison that started from an empty stack gave up, since it took too many
    /// steps.
    budget: Budget,
}

/// Whether a comparison that started from an empty stack gave up (see `COMPARISON_STEP_LIMIT`).
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
enum Budget {
    #[default]
    Within,
    /// The comparison in progress is giving up. Every comparison still in progress returns right
    /// away, and nothing is cached.
    GivingUp,
    /// A comparison gave up since this was last taken (see `take_too_complex`). Until then, every
    /// other one gives up right away.
    GaveUp,
}

/// A comparison that starts from an empty stack gives up after this many steps, rather than
/// running for as long as it takes, since some recursive types take exponential time to compare
/// (#344). It's then `Invalid`, and the compiler reports it as too complex. Steps count each
/// comparison, and the work that can grow with the size of the stack or the number of binders.
/// The examples and tests take at most about 260,000 steps for a comparison, and the 2,500 random
/// programs of #344 that finish at most about 4,300,000.
pub(crate) const COMPARISON_STEP_LIMIT: usize = 20_000_000;

impl ComparisonContext<'_> {
    /// Returns whether a comparison gave up since this was last called, since it took too many
    /// steps (see `COMPARISON_STEP_LIMIT`), and lets the context compare again.
    pub(crate) fn take_too_complex(&mut self) -> bool {
        mem::take(&mut self.budget) == Budget::GaveUp
    }

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

    /// Whether the type, in its environment, refers to the binder that the generic resolves to in
    /// its own.
    fn refers_to(
        &mut self,
        arena: &Arena<Type>,
        (ty, env): (TypeId, Option<usize>),
        (generic, generic_env): (TypeId, Option<usize>),
    ) -> bool {
        let binder = self.resolve(generic, generic_env);
        self.cache_references(arena, ty);
        self.references[&ty]
            .iter()
            .any(|&other| self.resolve(other, env) == binder)
    }

    /// Caches the generics that the type references (see `referenced_generics`) in `references`.
    fn cache_references(&mut self, arena: &Arena<Type>, ty: TypeId) {
        self.references
            .entry(ty)
            .or_insert_with(|| referenced_generics(arena, ty));
    }

    /// Adds a candidate for a generic being inferred. If it references the generics of a function
    /// being instantiated, it's deferred until their instances have finished (see
    /// `DeferredCandidate`).
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
    fn open_choice(&mut self, members: usize) -> OpenChoice {
        let parent = self.branch;
        OpenChoice {
            // Only a bound can depend on it, so it's left out while there are no instances
            local: (self.instances.len() > self.instantiable_from)
                .then(|| self.choices.open(self.local_branch, members)),
            infer: self
                .infer
                .as_mut()
                .map(|infer| infer.open_choice(parent, members)),
        }
    }

    /// Compares the value with a member of the union (or its fallback) with the candidates it
    /// collects recorded under it, and records what it gave.
    fn compare_member(
        &mut self,
        choice: OpenChoice,
        member: usize,
        compare: impl FnOnce(&mut Self) -> Comparison,
    ) -> Comparison {
        let local = choice.local.map(|choice| BranchId { choice, member });
        let infer = choice.infer.map(|choice| BranchId { choice, member });

        let local_parent = if local.is_some() {
            mem::replace(&mut self.local_branch, local)
        } else {
            self.local_branch
        };
        let parent = if infer.is_some() {
            mem::replace(&mut self.branch, infer)
        } else {
            self.branch
        };

        // Only the bounds of the instances that already exist count, since those of an instance
        // created and checked within the member aren't collected again when a comparison is
        // reused from the memo, and they don't depend on the member. A member that holds by
        // assuming a comparison in progress gets the bounds that comparison puts on its instances
        // (its summary), so it counts as bounded if they bound one of these, and otherwise holds
        // the value whatever they are (see `Choices::mark_bounded`)
        let existing = self.instances.len();
        let outer_bounded = mem::replace(&mut self.bounded, usize::MAX);
        let result = compare(self);
        let bounded = self.bounded < existing;
        self.bounded = self.bounded.min(outer_bounded);

        self.local_branch = local_parent;
        self.branch = parent;

        if let Some(local) = local {
            if bounded {
                self.choices.mark_bounded(local);
            }

            self.choices.record(local, result);
        }

        if let Some(infer) = infer
            && bounded
        {
            self.infer.as_mut().unwrap().mark_bounded(infer);
        }

        if let Some(infer) = infer {
            self.infer.as_mut().unwrap().record(infer, result);
        }

        result
    }

    /// The union members that a bound collected now is recorded under.
    fn bound_branches(&self) -> BoundBranches {
        BoundBranches {
            local: self.local_branch,
            infer: self.branch,
        }
    }

    /// Hands the bounds of an instance whose function comparison has finished to the inference,
    /// once a candidate has been deferred, along with where the generics they reference are bound.
    /// They become the bounds of the instance's var, as far as they're in scope where the candidate
    /// is used (see `ComparisonContext::finish` and `instance_bounds`).
    fn instance_finished(
        &mut self,
        arena: &Arena<Type>,
        binder: usize,
        lower: &[Bound],
        upper: &[Bound],
    ) {
        if self.deferred.is_empty() {
            return;
        }

        // Like the pairs that are checked, only the bounds collected under union members that
        // matched are used, unless none were (see `Inference::keep_matched`)
        let holds = |bound: &&Bound| {
            bound
                .2
                .local
                .is_none_or(|branch| self.choices.matched_so_far(branch))
        };
        let mut kept_lower: Vec<&Bound> = lower.iter().filter(holds).collect();
        let mut kept_upper: Vec<&Bound> = upper.iter().filter(holds).collect();

        if kept_lower.is_empty() && kept_upper.is_empty() {
            kept_lower = lower.iter().collect();
            kept_upper = upper.iter().collect();
        }

        let mut bounds = Vec::new();

        for (kind, kept) in [
            (CandidateKind::Lower, &kept_lower),
            (CandidateKind::Upper, &kept_upper),
        ] {
            for &&(ty, env, _) in kept {
                self.cache_references(arena, ty);

                let inner: Vec<usize> = self.references[&ty]
                    .iter()
                    .filter_map(|&generic| self.resolve(generic, env))
                    .collect();

                let scope = if inner.is_empty() {
                    BoundScope::Free
                } else if inner
                    .iter()
                    .all(|&binder| self.binders[binder].instance.is_some())
                {
                    BoundScope::Instantiated
                } else {
                    BoundScope::Rigid
                };

                bounds.push((kind, ty, scope));
            }
        }

        self.instantiations.insert(binder, bounds);
    }

    /// Replaces the generics of the instantiated functions that the deferred candidates reference
    /// with a new var for each instance, which the inference takes along with the instance's
    /// bounds, and then resolves the instances (see `Inference::add_instance`).
    fn finish(&mut self, arena: &mut Arena<Type>, builtins: &BuiltinTypes) {
        // It's taken for the loop, so the context's own helpers can still be used
        let Some(infer) = self.infer.take() else {
            return;
        };

        // The bounds of each binder's instance that can be instantiated in scope
        let policy = infer.instance_policy();
        let instantiable: FxHashMap<usize, InferenceCandidates> = self
            .instantiations
            .iter()
            .filter_map(|(&binder, bounds)| {
                Some((binder, instance_bounds(builtins, policy, bounds)?))
            })
            .collect();

        // The var that stands for each binder's instance
        let mut vars: FxHashMap<usize, TypeId> = FxHashMap::default();

        for DeferredCandidate {
            generic,
            kind,
            index,
            env,
        } in mem::take(&mut self.deferred)
        {
            let ty = infer.candidates_mut(generic).list_mut(kind)[index];
            let mut mappings = HashMap::new();

            self.cache_references(arena, ty);

            for &other in &self.references[&ty] {
                let Some(binder) = self.resolve(other, env) else {
                    continue;
                };

                let Some(bounds) = instantiable.get(&binder) else {
                    continue;
                };

                let var = *vars.entry(binder).or_insert_with(|| {
                    let var = arena.alloc(Type::Generic(Generic { name: None }));
                    infer.add_instance(var, bounds);
                    var
                });

                mappings.insert(other, var);
            }

            if !mappings.is_empty() {
                let ty = substitute_with_mappings(arena, ty, &mappings);
                infer.candidates_mut(generic).list_mut(kind)[index] = ty;
            }
        }

        if policy == InstancePolicy::Eager {
            infer.resolve_instances(arena, builtins);
        }

        self.infer = Some(infer);
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
                alias: None,
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

    /// Adds a bound to the instance of the generic.
    fn add_bound(&mut self, generic: TypeId, env: Option<usize>, bound: Bound, upper: bool) {
        let binder = self.resolve(generic, env).unwrap();
        self.push_bound(binder, bound, upper);
    }

    /// Adds a bound to the instance of the binder.
    fn push_bound(&mut self, binder: usize, bound: Bound, upper: bool) {
        let index = self.binders[binder].instance.unwrap();
        let instance = &mut self.instances[index];
        self.bounded = self.bounded.min(index);

        if upper {
            instance.upper.push(bound);
        } else {
            instance.lower.push(bound);
        }
    }

    /// Records what a comparison that's being entered can change outside of itself: the bounds of
    /// the instances its key refers to (the binders it numbers), the choices and the deferred
    /// candidates, and the inference's candidates.
    fn snapshot(&self, binders: &[usize]) -> Snapshot {
        let instances = binders
            .iter()
            .enumerate()
            .filter_map(|(position, &binder)| {
                let index = self.instance_of(binder)?;
                let instance = &self.instances[index];
                Some((position, index, instance.lower.len(), instance.upper.len()))
            })
            .collect();

        Snapshot {
            instances,
            binders: self.binders.len(),
            instance_count: self.instances.len(),
            choices: self.choices.len(),
            deferred: self.deferred.len(),
            infer: self.infer.as_ref().map(|infer| infer.snapshot()),
        }
    }

    /// Undoes what the comparison changed since the snapshot, so that it can be computed again.
    /// The binders and instances bound inside of it are removed too, since nothing refers to them
    /// anymore, so that rounds don't add up.
    fn restore(&mut self, snapshot: &Snapshot) {
        for &(_, index, lower, upper) in &snapshot.instances {
            let instance = &mut self.instances[index];
            instance.lower.truncate(lower);
            instance.upper.truncate(upper);
        }

        self.binders.truncate(snapshot.binders);
        self.instances.truncate(snapshot.instance_count);
        self.instantiations
            .retain(|&binder, _| binder < snapshot.binders);

        self.choices.truncate(snapshot.choices);
        self.deferred.truncate(snapshot.deferred);

        if let (Some(infer), Some(snapshot)) = (self.infer.as_mut(), &snapshot.infer) {
            infer.restore(snapshot);
        }
    }

    /// The bounds that the comparison in progress at the top of the stack put on the instances its
    /// key refers to since the snapshot, as a summary in its coordinates: the binders its key
    /// numbers, and local ones for those bound inside of it. It isn't canonical yet.
    ///
    /// A local binder that's the instance of a generic function compared inside of the comparison
    /// can still be instantiated while an enclosing comparison checks the bounds that refer to it
    /// (#363), so the summary also has its own bounds, as constraints on the local binder, which is
    /// what makes it an instance where the summary is applied. One without bounds of its own gets
    /// `≤ Any`, which always holds. That's only for those that the bounds of the key's instances
    /// refer to: their own bounds are checked again with every instance rigid, so the instances
    /// that those refer to don't need theirs.
    fn summary_since(
        &mut self,
        arena: &Arena<Type>,
        builtins: &BuiltinTypes,
        snapshot: &Snapshot,
    ) -> Summary {
        let frame = self.frames.last().unwrap();
        let (entered, numbering) = (frame.entered, frame.binders.clone());
        let mut summary = Summary::default();
        let mut locals: FxHashMap<usize, Var> = FxHashMap::default();

        // The local binders that are instances, which the bounds of the key's instances refer to
        let mut exported: Vec<(usize, Var)> = Vec::new();

        for &(position, index, lower_from, upper_from) in &snapshot.instances {
            let instance = &self.instances[index];
            let bounds: Vec<(Bound, bool)> = instance.lower[lower_from..]
                .iter()
                .map(|&bound| (bound, false))
                .chain(
                    instance.upper[upper_from..]
                        .iter()
                        .map(|&bound| (bound, true)),
                )
                .collect();

            for (bound, upper) in bounds {
                let (constraint, referenced) = self.constraint_of(
                    arena,
                    snapshot,
                    (entered, &numbering),
                    (Var::Key(position), bound, upper),
                    &mut summary,
                    &mut locals,
                );

                for local in referenced {
                    if self.binders[local.0].instance.is_some() && !exported.contains(&local) {
                        exported.push(local);
                    }
                }

                summary.add(constraint);
            }
        }

        for (binder, target) in exported {
            let instance = &self.instances[self.binders[binder].instance.unwrap()];
            let mut bounds: Vec<(Bound, bool)> = instance
                .lower
                .iter()
                .map(|&bound| (bound, false))
                .chain(instance.upper.iter().map(|&bound| (bound, true)))
                .collect();

            if bounds.is_empty() {
                bounds.push((
                    (builtins.permissive_any, None, BoundBranches::default()),
                    true,
                ));
            }

            for (bound, upper) in bounds {
                let (constraint, _) = self.constraint_of(
                    arena,
                    snapshot,
                    (entered, &numbering),
                    (target, bound, upper),
                    &mut summary,
                    &mut locals,
                );
                summary.add(constraint);
            }
        }

        summary
    }

    /// The constraint for a bound in the summary of the comparison at the top of the stack (see
    /// `summary_since`), whose key's binders are `numbering`, and whose own were bound from
    /// `entered` on, along with the local binders it refers to.
    fn constraint_of(
        &mut self,
        arena: &Arena<Type>,
        snapshot: &Snapshot,
        (entered, numbering): (usize, &[usize]),
        (target, (ty, env, branches), upper): (Var, Bound, bool),
        summary: &mut Summary,
        locals: &mut FxHashMap<usize, Var>,
    ) -> (Constraint, Vec<(usize, Var)>) {
        self.cache_references(arena, ty);
        let mut vars = Vec::new();
        let mut referenced = Vec::new();

        for &generic in &self.references[&ty] {
            let Some(binder) = self.resolve(generic, env) else {
                continue;
            };

            let outside = (binder < entered)
                .then(|| numbering.iter().position(|&other| other == binder))
                .flatten();
            debug_assert!(
                binder >= entered || outside.is_some(),
                "a bound refers to a binder outside of the comparison that its key doesn't"
            );

            let var = if let Some(position) = outside {
                Var::Key(position)
            } else {
                let var = *locals.entry(binder).or_insert_with(|| summary.local());
                referenced.push((binder, var));
                var
            };
            vars.push((generic, var));
        }

        // A bound collected under a union member inside of the comparison that didn't match
        // doesn't decide the result wherever the summary is applied either
        let unmatched = branches
            .local
            .is_some_and(|branch| !self.choices.matched_since(branch, snapshot.choices));

        let constraint = Constraint {
            var: target,
            upper,
            ty,
            env: vars,
            unmatched,
        };
        (constraint, referenced)
    }

    /// Puts the bounds of a summary on the instances of the binders that a comparison's key
    /// numbers, which are `args` in the summary's coordinates. Each of its local binders is a new
    /// binder: an instance if a constraint bounds it (see `summary_since`), and rigid otherwise.
    /// Each bound's environment is made of aliases of the binders that its generics resolve to.
    fn apply_summary(&mut self, summary: &Summary, args: &[usize]) {
        if summary.constraints().is_empty() {
            return;
        }

        // The local binders, with the first generic each one stands for, which is only kept for
        // the binder to have one, since it's reached through aliases rather than by its generic
        let mut generics: Vec<Option<TypeId>> = vec![None; summary.locals()];
        let mut instances = vec![false; summary.locals()];

        for constraint in summary.constraints() {
            if let Var::Local(local) = constraint.var {
                instances[local] = true;
            }

            for &(generic, var) in &constraint.env {
                if let Var::Local(local) = var {
                    generics[local].get_or_insert(generic);
                }
            }
        }

        let locals: Vec<usize> = generics
            .into_iter()
            .zip(instances)
            .map(|(generic, instance)| {
                let instance = instance.then(|| {
                    self.instances.push(Instance::default());
                    self.instances.len() - 1
                });
                self.binders.push(Binder {
                    generic: generic.unwrap_or(summary.constraints()[0].ty),
                    parent: None,
                    instance,
                    alias: None,
                });
                self.binders.len() - 1
            })
            .collect();

        let mut unmatched = None;

        for constraint in summary.constraints() {
            let target = match constraint.var {
                Var::Key(position) => {
                    // The binders a key numbers are the ones its generics resolve to, so none is
                    // an alias
                    debug_assert!(self.binders[args[position]].alias.is_none());
                    args[position]
                }
                Var::Local(local) => locals[local],
            };

            let mut env = None;

            for &(generic, var) in &constraint.env {
                let alias = match var {
                    Var::Key(position) => args[position],
                    Var::Local(local) => locals[local],
                };

                self.binders.push(Binder {
                    generic,
                    parent: env,
                    instance: None,
                    alias: Some(alias),
                });
                env = Some(self.binders.len() - 1);
            }

            // A bound that didn't match goes under a member of a choice that's only there to not
            // match, in `choices` and in the inference, so that it doesn't decide the result, and
            // the candidates that checking it collects are left out like the original's
            let branches = if constraint.unmatched {
                *unmatched.get_or_insert_with(|| self.unmatched_branches())
            } else {
                self.bound_branches()
            };

            let bound = (constraint.ty, env, branches);

            // A bound that didn't match doesn't decide anything, so it doesn't count as bounding
            // the instance for the union member it's applied under either (see `compare_member`)
            let bounded = self.bounded;
            self.push_bound(target, bound, constraint.upper);

            if constraint.unmatched {
                self.bounded = bounded;
            }
        }
    }

    /// Opens a choice with two members under the current ones, the first of which didn't match,
    /// and returns it (see `ComparisonContext::apply_summary`).
    fn unmatched_branches(&mut self) -> BoundBranches {
        let choice = self.open_choice(2);
        let missed = |choice| BranchId { choice, member: 0 };
        let matched = |choice| BranchId { choice, member: 1 };

        if let Some(local) = choice.local {
            self.choices.record(missed(local), Comparison::Invalid);
            self.choices.record(matched(local), Comparison::Assign);
        }

        if let (Some(infer), Some(choice)) = (self.infer.as_mut(), choice.infer) {
            infer.record(missed(choice), Comparison::Invalid);
            infer.record(matched(choice), Comparison::Assign);
        }

        BoundBranches {
            local: choice.local.map(missed),
            infer: choice.infer.map(missed),
        }
    }

    /// Counts a step towards the comparison that started from an empty stack, and returns `true`
    /// if it has taken too many, so it's giving up (see `COMPARISON_STEP_LIMIT`).
    fn spend(&mut self) -> bool {
        self.spend_many(1)
    }

    /// Counts the steps towards the comparison that started from an empty stack, like `spend`.
    fn spend_many(&mut self, steps: usize) -> bool {
        self.steps += steps;

        if self.steps > comparison_step_limit(COMPARISON_STEP_LIMIT) {
            self.budget = Budget::GivingUp;
        }

        self.budget == Budget::GivingUp
    }

    /// Whether comparing the type (and the types it contains) is the least fixed point of monotone
    /// equations over the comparisons it reaches, which is what `FixpointMemo` needs, and whether
    /// it can start in a single pass. Comparing is built from `max`, `min` and constants (the
    /// fallbacks that are only compared when a union doesn't hold are a `min` whose result doesn't
    /// depend on them otherwise), and returning early at a comparison in progress gives `Assign`
    /// (the least value), or its approximation while `FixpointMemo` is iterating, as long as the
    /// type is contractive, since a cycle that doesn't pass through a pair or function gives
    /// `Invalid` instead.
    ///
    /// A comparison that refers to an instance also puts bounds on it, which are part of its value
    /// (`Outcome`), and adding bounds only makes the results worse, so the equations are still
    /// monotone. But `FixpointMemo`'s single pass relies on values being a chain of three, which
    /// summaries aren't, and on comparing being built from `max`, `min` and constants, which
    /// `Choices::is_held` isn't once a union member can bound an instance. So it's only used when
    /// each generic function's parameters and return type either are one of its generics or don't
    /// reference them (`Monotone::Direct`). The
    /// comparisons that refer to instances are then only bounds on them, which reach no other
    /// comparison, so they're never revisited, and their summaries are never approximated.
    /// Otherwise it iterates from the start (`Monotone::Nested`).
    ///
    /// Inference also collects candidates as a side effect, so it isn't used then.
    fn monotonicity(&mut self, arena: &Arena<Type>, id: TypeId) -> Monotone {
        if let Some(&monotone) = self.monotone.get(&id) {
            return monotone;
        }

        let reachable = reachable(arena, id);
        let monotone = if has_non_contractive_cycle(arena, &reachable) {
            Monotone::No
        } else if reachable.iter().all(|&id| instantiates_directly(arena, id)) {
            Monotone::Direct
        } else {
            Monotone::Nested
        };
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

/// Returns the binder that the generic is bound to in the environment (or the one it's an alias
/// of), or `None` if it's free.
fn resolve(binders: &[Binder], generic: TypeId, mut env: Option<usize>) -> Option<usize> {
    while let Some(index) = env {
        if binders[index].generic == generic {
            return Some(root(binders, index));
        }

        env = binders[index].parent;
    }

    None
}

/// Returns the binder that the binder is an alias of, or itself if it isn't one.
fn root(binders: &[Binder], mut index: usize) -> usize {
    while let Some(alias) = binders[index].alias {
        index = alias;
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

/// A comparison gave up, since it took too many steps (see `COMPARISON_STEP_LIMIT`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TooComplex;

/// Like `compare_with_inference_limited`, but a comparison that gives up is `Invalid`.
pub fn compare_with_inference(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    lhs: TypeId,
    rhs: TypeId,
    infer: Option<&mut Inference>,
) -> Comparison {
    compare_with_inference_limited(arena, builtins, lhs, rhs, infer).unwrap_or(Comparison::Invalid)
}

/// Compares the types, collecting candidates for the generics being inferred, and returns
/// `TooComplex` if it took too many steps (see `COMPARISON_STEP_LIMIT`).
pub fn compare_with_inference_limited(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    lhs: TypeId,
    rhs: TypeId,
    infer: Option<&mut Inference>,
) -> Result<Comparison, TooComplex> {
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
    ctx.finish(arena, builtins);

    if let Some((lhs_name, rhs_name)) = &names {
        trace!("Comparison from {lhs_name} to {rhs_name} yielded {result:?}");
    }

    if ctx.take_too_complex() {
        return Err(TooComplex);
    }

    Ok(result)
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
    ctx.finish(arena, builtins);
}

pub fn compare(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    lhs: TypeId,
    rhs: TypeId,
) -> Comparison {
    compare_with_inference(arena, builtins, lhs, rhs, None)
}

/// Like `compare`, but returns `TooComplex` if it took too many steps (see
/// `COMPARISON_STEP_LIMIT`).
pub fn compare_limited(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    lhs: TypeId,
    rhs: TypeId,
) -> Result<Comparison, TooComplex> {
    compare_with_inference_limited(arena, builtins, lhs, rhs, None)
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
        // Once a comparison gave up, the others that share its context (such as those of a
        // `check`) give up as well, rather than each taking as long
        if ctx.budget == Budget::GaveUp {
            return Comparison::Invalid;
        }

        ctx.steps = 0;

        let result =
            compare_from_empty_stack(arena, builtins, ctx, lhs, rhs, lhs_semantic, rhs_semantic);

        if ctx.budget == Budget::GivingUp {
            ctx.budget = Budget::GaveUp;
            return Comparison::Invalid;
        }

        return result;
    }

    grow_stack(|| compare_impl_inner(arena, builtins, ctx, lhs, rhs, lhs_semantic, rhs_semantic))
}

/// Compares the types from an empty stack, in fixpoint mode if possible (see `FixpointMemo`).
fn compare_from_empty_stack(
    arena: &Arena<Type>,
    builtins: &BuiltinTypes,
    ctx: &mut ComparisonContext<'_>,
    lhs: TypeId,
    rhs: TypeId,
    lhs_semantic: Option<TypeId>,
    rhs_semantic: Option<TypeId>,
) -> Comparison {
    let monotonicity = ctx
        .monotonicity(arena, lhs)
        .max(ctx.monotonicity(arena, rhs));
    ctx.root_monotonicity = monotonicity;

    // How it's computed, which is exact mode whenever fixpoint mode can't be used
    let mode = if ctx.infer.is_none() && ctx.lhs_env.is_none() && ctx.rhs_env.is_none() {
        match monotonicity {
            // Without the memo, iterating wouldn't reuse anything, and exact mode gives the same
            // results
            Monotone::Nested if !compare_memo_enabled() => Monotone::No,
            mode => mode,
        }
    } else {
        Monotone::No
    };

    ctx.fixpoint = mode != Monotone::No;

    if ctx.fixpoint {
        ctx.fixpoint_memo.start();

        if mode == Monotone::Direct {
            let result = grow_stack(|| {
                compare_impl_inner(arena, builtins, ctx, lhs, rhs, lhs_semantic, rhs_semantic)
            });

            if !ctx.fixpoint_memo.abandoned() {
                return result;
            }
        }

        // The single pass took too long, or can't be used, so it's repeated until the
        // approximations it reaches stop changing instead (see `FixpointMemo`)
        loop {
            ctx.fixpoint_memo.start_round();

            let result = grow_stack(|| {
                compare_impl_inner(arena, builtins, ctx, lhs, rhs, lhs_semantic, rhs_semantic)
            });

            // A round that was cut short stops iterating without finishing the round, so its
            // approximations aren't kept, and the result is discarded as `Invalid`
            if ctx.budget == Budget::GivingUp || ctx.fixpoint_memo.finish_round() {
                return result;
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

    // The comparison that started from an empty stack is giving up, so it unwinds
    if ctx.spend() {
        return Comparison::Invalid;
    }

    // The comparison that started from an empty stack is unwinding, to iterate instead
    if ctx.fixpoint && ctx.fixpoint_memo.abandoned() {
        return Comparison::Assign;
    }

    let (bindings, binders) = ctx.bindings(arena, lhs, rhs);
    let key = (lhs, rhs, ctx.flipped, bindings, lhs_semantic, rhs_semantic);

    // Generics that are being instantiated outside of the comparison collect bounds from it, which
    // are cached with its result as its summary, and put on the instances it's reused with (the
    // ones bound inside of it are checked before it finishes). A summary is in a canonical order,
    // without duplicates, which doesn't change whether the bounds hold, but does change the order
    // of the candidates that checking them collects, so it isn't cached while inferring. Nor in
    // fixpoint mode's single pass, whose values must be a chain of three, where a comparison that
    // refers to an instance is only ever a bound on it (see `ComparisonContext::monotonicity`).
    let instances = ctx.bindings[bindings]
        .iter()
        .any(|&(_, _, binding)| matches!(binding, Binding::Instance(_)));
    let memoize = compare_memo_enabled()
        && !(instances
            && (ctx.infer.is_some() || (ctx.fixpoint && !ctx.fixpoint_memo.iterating())));

    if let Some(index) = ctx.stack.get_index_of(&key) {
        // In fixpoint mode, it's the least value in a single pass, or its approximation from the
        // previous round while iterating, along with the bounds it puts on its instances
        let (value, summary) = if ctx.fixpoint {
            let key = memoize.then_some(&key);
            ctx.fixpoint_memo
                .reached(index, key, (Comparison::Assign, None))
        } else {
            ctx.memo.reached(index);
            (ctx.frames[index].value, None)
        };

        // Revisiting a comparison is only sound to treat as assignable if we've passed through
        // a pair or function since, otherwise a non-contractive type like `type A = B; type B = A;`
        // would be assignable to anything. Those should be rejected before reaching this point.
        if ctx.guards.last().is_none_or(|&guard| guard < index) {
            return Comparison::Invalid;
        }

        // The bounds that it puts on the instances its key refers to are put on the ones it's
        // revisited with, as far as they're known, so that each revisit instantiates its own
        // generic functions (see `StackFrame::approximation`)
        let frame = &mut ctx.frames[index];
        frame.revisited = true;

        if let Some(summary) = summary {
            ctx.apply_summary(&summary, &binders);
        } else if instances && !ctx.fixpoint {
            let approximation = mem::take(&mut frame.approximation);
            ctx.apply_summary(&approximation, &binders);
            ctx.frames[index].approximation = approximation;
        }

        return value;
    }

    if memoize {
        let result = if ctx.fixpoint {
            ctx.fixpoint_memo.get(&key, &ctx.stack)
        } else {
            let result = ctx.memo.get(&key, ctx.stack.iter().copied());

            // Reusing a result can take as long as the stack, so it counts towards the steps
            let work = ctx.memo.take_work();
            ctx.spend_many(work);
            result
        };

        let result = result.map(|(result, summary)| {
            if let Some(summary) = summary {
                ctx.spend_many(summary.constraints().len());
                ctx.apply_summary(&summary, &binders);
            }

            result
        });

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
    // What it puts on its instances is its summary in either mode. It's only computed again in
    // rounds in exact mode: when it puts bounds on instances, or when its result can need more
    // than one round (see `ComparisonContext::root_monotonicity`)
    let iterate_values = ctx.root_monotonicity == Monotone::Nested;
    let rounds_needed = !ctx.fixpoint && (instances || iterate_values);
    let snapshot = (instances || rounds_needed).then(|| ctx.snapshot(&binders));
    let (value, approximation) = if rounds_needed {
        let (value, approximation) = ctx
            .approximations
            .get(&key)
            .cloned()
            .unwrap_or((Comparison::Assign, Summary::default()));
        let value = if iterate_values {
            value
        } else {
            Comparison::Assign
        };
        (value, approximation)
    } else {
        (Comparison::Assign, Summary::default())
    };
    ctx.frames.push(StackFrame {
        entered: ctx.binders.len(),
        binders,
        approximation,
        value,
        revisited: false,
    });

    let mut rounds = 0;

    let result = loop {
        let result = compare_structure(arena, builtins, ctx, lhs, rhs, lhs_semantic, rhs_semantic);

        // If it was revisited, the revisits were given its summary (and maybe its result) from
        // the previous round, so it's computed again until this round's adds nothing to them.
        // While iterating in fixpoint mode, `FixpointMemo` does this for every comparison
        if !rounds_needed || !ctx.frames.last().unwrap().revisited || ctx.budget == Budget::GivingUp
        {
            break result;
        }

        let snapshot = snapshot.as_ref().unwrap();
        let summary = ctx.summary_since(arena, builtins, snapshot);
        let frame = ctx.frames.last_mut().unwrap();
        let next = frame.approximation.join(&summary);
        let value = if iterate_values {
            frame.value.max(result)
        } else {
            frame.value
        };

        if next == frame.approximation && value == frame.value {
            if !next.constraints().is_empty() || value != Comparison::Assign {
                ctx.approximations.insert(key, (value, next));
            }

            break result;
        }

        rounds += 1;

        if rounds >= MAX_ROUNDS {
            ctx.budget = Budget::GivingUp;
            break Comparison::Invalid;
        }

        frame.approximation = next;
        frame.value = value;
        frame.revisited = false;
        ctx.restore(snapshot);
    };

    // Inference candidates are collected as a side effect of comparing, so a comparison that
    // collected any can't be skipped
    // A comparison that's giving up doesn't have its actual result
    let memo_key =
        (memoize && ctx.candidates == candidates && ctx.budget != Budget::GivingUp).then_some(key);

    // What the last round put on the instances its key refers to, which the rounds before it don't
    // add to, since they're undone
    let summary = snapshot
        .as_ref()
        .filter(|_| instances && memo_key.is_some())
        .map(|snapshot| {
            let mut summary = ctx.summary_since(arena, builtins, snapshot);
            summary.canonicalize();
            summary
        })
        .filter(|summary| !summary.constraints().is_empty())
        .map(Rc::new);

    ctx.stack.pop().unwrap();
    ctx.frames.pop().unwrap();

    match frame {
        Frame::Exact(frame) => {
            ctx.memo.exit(frame, key, memo_key, (result, summary));
            let work = ctx.memo.take_work();
            ctx.spend_many(work);
        }
        Frame::Fixpoint(frame) => {
            ctx.fixpoint_memo
                .exit(frame, memo_key, (result, summary), &ctx.stack);
        }
    }

    result
}

/// Compares the types by their structure, once `compare_impl_inner` has entered the comparison.
fn compare_structure(
    arena: &Arena<Type>,
    builtins: &BuiltinTypes,
    ctx: &mut ComparisonContext<'_>,
    lhs: TypeId,
    rhs: TypeId,
    lhs_semantic: Option<TypeId>,
    rhs_semantic: Option<TypeId>,
) -> Comparison {
    match (arena[lhs].clone(), arena[rhs].clone()) {
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
        // An instance isn't bounded by a type that refers to it, which an inner instance can be
        // compared with while an enclosing comparison checks bounds that refer to it on both
        // sides, such as `(T, T | nil)` against `(T, T)`. That would conflict with its other
        // bounds once it's rigid, so it's compared like a rigid generic instead, which holds for
        // `T ≤ T | nil`
        (Type::Generic(_), _)
            if ctx.is_instance(lhs, ctx.lhs_env)
                && !ctx.refers_to(arena, (rhs, ctx.rhs_env), (lhs, ctx.lhs_env)) =>
        {
            ctx.add_bound(
                lhs,
                ctx.lhs_env,
                (rhs, ctx.rhs_env, ctx.bound_branches()),
                true,
            );
            Comparison::Assign
        }
        (_, Type::Generic(_))
            if ctx.is_instance(rhs, ctx.rhs_env)
                && !ctx.refers_to(arena, (lhs, ctx.lhs_env), (rhs, ctx.rhs_env)) =>
        {
            ctx.add_bound(
                rhs,
                ctx.rhs_env,
                (lhs, ctx.lhs_env, ctx.bound_branches()),
                false,
            );
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
                let own = ctx.instances.len();
                ctx.lhs_env = ctx.bind(lhs_env, &lhs.generics, true);
                ctx.rhs_env = ctx.bind(rhs_env, &rhs.generics, false);
                // The instances of the generic functions compared inside of this one come after
                // its own
                let inner = ctx.instances.len();

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

                // This one's own instances keep their bounds, since an enclosing comparison can
                // instantiate them again while it checks its own (below)
                for index in first..first + lhs.generics.len() {
                    let slot = ctx.binders[index].instance.unwrap();
                    let instance = mem::take(&mut ctx.instances[slot]);
                    ctx.instance_finished(arena, index, &instance.lower, &instance.upper);
                    ctx.instances[slot] = instance;
                }

                // There is an instantiation of each generic if every type that flows into it is
                // assignable to every type it flows into (such as their union, or `Never`).
                // The bounds come from the other side, where the generics of any enclosing
                // functions being instantiated are rigid, so they're hidden while checking. The
                // generic functions compared inside of this one can still be instantiated, though:
                // a bound such as `B ≥ (T, T)`, from `fn<T>() -> (T, T)` compared with
                // `fn() -> B`, holds for `B ≤ (Int, Int)` with `T` as `Int` (#363). Each of them is
                // instantiated once for all of the bounds, and its own bounds are checked again
                // with what that adds to them
                let end = ctx.instances.len();
                let before: Vec<(usize, usize)> = ctx.instances[inner..end]
                    .iter()
                    .map(|instance| (instance.lower.len(), instance.upper.len()))
                    .collect();
                // This one's own instances, and those of enclosing comparisons, stay rigid: the
                // pairs being checked are their own bounds
                let instantiable_from = mem::replace(&mut ctx.instantiable_from, inner);
                result = max(result, check_bounds(arena, builtins, ctx, own..inner));

                let grown: Vec<usize> = (inner..end)
                    .zip(before)
                    .filter(|&(index, (lower, upper))| {
                        let instance = &ctx.instances[index];
                        instance.lower.len() > lower || instance.upper.len() > upper
                    })
                    .map(|(index, _)| index)
                    .collect();

                ctx.instantiable_from = ctx.instances.len();
                result = max(result, check_bounds(arena, builtins, ctx, grown));

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
            // A renamed generic being inferred still takes the generic it was renamed from, which
            // is what it's explicitly instantiated as, so that a union it's in doesn't leave only
            // the other members' values, from below (#370) or above (#378)
            if lhs != rhs && !ctx.flipped && ctx.is_target(rhs, ctx.rhs_env) {
                ctx.add_candidate(arena, rhs, CandidateKind::Lower, lhs, ctx.lhs_env);
            } else if lhs != rhs && ctx.flipped && ctx.is_target(lhs, ctx.lhs_env) {
                ctx.add_candidate(arena, lhs, CandidateKind::Upper, rhs, ctx.rhs_env);
            }

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
    }
}

/// Checks each lower bound of each instance against each of its upper bounds, and returns the
/// worst result of the pairs that decide anything (see `compare_structure`'s function arm).
fn check_bounds(
    arena: &Arena<Type>,
    builtins: &BuiltinTypes,
    ctx: &mut ComparisonContext<'_>,
    instances: impl IntoIterator<Item = usize>,
) -> Comparison {
    let mut result = Comparison::Assign;

    // The instances are rigid while checking, so their bounds don't change
    'bounds: for index in instances {
        for lower in 0..ctx.instances[index].lower.len() {
            for upper in 0..ctx.instances[index].upper.len() {
                let (lower, lower_env, lower_branches) = ctx.instances[index].lower[lower];
                let (upper, upper_env, upper_branches) = ctx.instances[index].upper[upper];
                // There can be many bounds, so this stops once it's giving up
                if ctx.spend() {
                    break 'bounds;
                }

                // A bound collected under a union member that didn't match isn't needed, so a
                // pair with one doesn't decide the result (#360). It's still compared, under that
                // member, since the candidates it collects are what a generic being inferred falls
                // back to when nothing else gives it any (see `Inference::keep_matched`)
                let unmatched = [lower_branches, upper_branches]
                    .into_iter()
                    .find(|branches| {
                        branches
                            .local
                            .is_some_and(|branch| !ctx.choices.matched_so_far(branch))
                    });
                let (branch, local_branch) = match unmatched {
                    Some(branches) => (
                        mem::replace(&mut ctx.branch, branches.infer),
                        mem::replace(&mut ctx.local_branch, branches.local),
                    ),
                    None => (ctx.branch, ctx.local_branch),
                };

                ctx.lhs_env = lower_env;
                ctx.rhs_env = upper_env;
                let pair = compare_impl(arena, builtins, ctx, lower, upper, None, None);

                if unmatched.is_none() {
                    result = max(result, pair);
                }

                ctx.branch = branch;
                ctx.local_branch = local_branch;
            }
        }
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

    /// Joining two outcomes takes the worse result and both summaries' bounds, which is what the
    /// approximations of `FixpointMemo` grow by from one round to the next.
    #[test]
    fn test_outcome_join() {
        let mut arena = Arena::new();
        let builtins = BuiltinTypes::new(&mut arena);
        let summary = |bound: TypeId| {
            let mut summary = Summary::default();
            summary.add(Constraint {
                var: Var::Key(0),
                upper: true,
                ty: bound,
                env: Vec::new(),
                unmatched: false,
            });
            Rc::new(summary)
        };
        let (int, bytes) = (summary(builtins.int), summary(builtins.bytes));

        let joined = (Comparison::Assign, Some(int.clone())).join(&(Comparison::Cast, Some(bytes)));
        assert_eq!(joined.0, Comparison::Cast);
        let bounds: Vec<TypeId> = joined
            .1
            .unwrap()
            .constraints()
            .iter()
            .map(|constraint| constraint.ty)
            .collect();
        assert_eq!(bounds.len(), 2);
        assert!(bounds.contains(&builtins.int) && bounds.contains(&builtins.bytes));

        // Without a summary on one side, the other's is kept
        let joined = (Comparison::Invalid, None).join(&(Comparison::Assign, Some(int.clone())));
        assert_eq!(joined, (Comparison::Invalid, Some(int)));
    }

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

    /// A candidate that references a passed generic function's generic is resolved through its
    /// instance's var, which is gone from the inference afterward (see
    /// `Inference::resolve_instances`).
    #[rstest]
    #[case::from_lower_bounds(true)]
    #[case::without_lower_bounds(false)]
    fn test_instance_var(#[case] param: bool) {
        let mut arena = Arena::new();
        let builtins = BuiltinTypes::new(&mut arena);
        let mut generic = || arena.alloc(Type::Generic(Generic { name: None }));
        let (t, a, b) = (generic(), generic(), generic());
        let pair = arena.alloc(Type::Pair(Pair::new(t, t)));

        let function = |arena: &mut Arena<Type>, generics, param: Option<TypeId>, ret| {
            arena.alloc(Type::Function(FunctionType {
                generics,
                params: param
                    .map(|ty| FunctionParameter::new("x", ty))
                    .into_iter()
                    .collect(),
                nil_terminated: true,
                ret,
            }))
        };

        // `fn<T>(x: T) -> (T, T)` to `fn(x: A) -> B` gives `B` the candidate `(A, A)`, and
        // `fn<T>() -> (T, T)` to `fn() -> B` gives it `(Never, Never)` (#339)
        let lhs = function(&mut arena, vec![t], param.then_some(t), pair);
        let rhs = function(&mut arena, vec![], param.then_some(a), b);

        let mut infer = Inference::new([a, b]);
        let result = compare_with_inference(&mut arena, &builtins, lhs, rhs, Some(&mut infer));
        assert_eq!(result, Comparison::Assign);
        assert_eq!(infer.generics.keys().copied().collect::<Vec<_>>(), [a, b]);

        let lower = &infer.candidates(b).unwrap().lower;
        assert_eq!(lower.len(), 1);
        let Type::Pair(pair) = &arena[lower[0]] else {
            panic!("expected a pair");
        };
        let instantiated = if param { a } else { builtins.never };
        assert_eq!((pair.first, pair.rest), (instantiated, instantiated));
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
