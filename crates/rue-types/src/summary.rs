//! Summaries of the bounds that a comparison puts on generics being instantiated, for #362's step
//! 13b, which isn't using them yet.
//!
//! A comparison with a binding to a generic being instantiated currently collects bounds into it
//! as a side effect, and merges two instances when it's revisited. What it does then depends on
//! which comparisons are in progress, so its result can't be reused exactly (#344). A summary
//! records those bounds as data instead, in the comparison's own coordinates: the binders that its
//! key numbers (see `Binding`), and the ones bound inside of it. A comparison's result is then its
//! value and its summary, which only depend on its key. Reusing it, or reaching it again while
//! it's in progress, applies the summary to the binders it's reached with.

#![cfg_attr(
    not(test),
    expect(
        dead_code,
        reason = "comparisons start using summaries in #362's step 13b.1"
    )
)]

use std::mem;

use crate::{FxHashMap, FxIndexMap, TypeId};

/// A binder in a summary's coordinates.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum Var {
    /// The binder that the comparison's key numbers this way, which is bound outside of it.
    Key(usize),
    /// A binder that's bound inside of the comparison. Each time the summary is applied, these are
    /// new binders, since each place it's applied binds its own.
    Local(usize),
}

/// What the generics that a bound's type references are bound to, sorted by generic. A generic
/// that isn't listed is free.
pub(crate) type Env = Vec<(TypeId, Var)>;

/// A bound of a binder being instantiated: the type flows into it (a lower bound), or it flows
/// into the type (an upper bound).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Constraint {
    pub(crate) var: Var,
    pub(crate) upper: bool,
    pub(crate) ty: TypeId,
    pub(crate) env: Env,
}

impl Constraint {
    /// Returns the constraint with each binder it refers to replaced.
    fn map_vars(&self, mut f: impl FnMut(Var) -> Var) -> Self {
        Self {
            var: f(self.var),
            upper: self.upper,
            ty: self.ty,
            env: self
                .env
                .iter()
                .map(|&(generic, var)| (generic, f(var)))
                .collect(),
        }
    }

    /// The local binders that the constraint refers to.
    fn locals(&self) -> impl Iterator<Item = usize> + '_ {
        [self.var]
            .into_iter()
            .chain(self.env.iter().map(|&(_, var)| var))
            .filter_map(|var| match var {
                Var::Key(_) => None,
                Var::Local(index) => Some(index),
            })
    }
}

/// The bounds that a comparison puts on the binders it refers to (see the module documentation).
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct Summary {
    /// The number of local binders.
    locals: usize,
    constraints: Vec<Constraint>,
}

impl Summary {
    pub(crate) fn constraints(&self) -> &[Constraint] {
        &self.constraints
    }

    /// Binds a new local binder, and returns it.
    pub(crate) fn local(&mut self) -> Var {
        self.locals += 1;
        Var::Local(self.locals - 1)
    }

    pub(crate) fn add(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
    }

    /// Adds the constraints of a comparison that this one reached, where `args` are what the
    /// binders its key numbers are in this one's coordinates. Its local binders are new ones.
    pub(crate) fn apply(&mut self, other: &Self, args: &[Var]) {
        let offset = self.locals;
        self.locals += other.locals;

        for constraint in &other.constraints {
            let constraint = constraint.map_vars(|var| match var {
                Var::Key(index) => args[index],
                Var::Local(index) => Var::Local(offset + index),
            });
            self.constraints.push(constraint);
        }
    }

    /// Removes and returns the bounds of the binder, such as those of a function's generics once
    /// the comparison that bound them has checked them against each other. Other constraints can
    /// still refer to it, in which case it's rigid from then on.
    pub(crate) fn take(&mut self, var: Var) -> Vec<Constraint> {
        let (taken, kept) = mem::take(&mut self.constraints)
            .into_iter()
            .partition(|constraint| constraint.var == var);
        self.constraints = kept;
        taken
    }

    /// Puts the summary in a canonical form, so that summaries that only differ in the order of
    /// their constraints and the numbering of their local binders are equal, and the copies of a
    /// summary that's applied again and again don't grow it without bound.
    ///
    /// The local binders are grouped into components that share a constraint, and the constraints
    /// that refer to none are kept once each. Each component is renumbered from 0 (its shape), and
    /// at most two components of each shape are kept. That keeps every pair of bounds that can be
    /// checked against each other, up to renaming the local binders: two bounds from one copy (with
    /// the same local binders), and two from different copies (with different ones). A third copy
    /// only adds more pairs like those. A shape is renumbered in the order that its constraints
    /// sort in with the local binders left out, so two components with the same constraints in the
    /// same order always have the same shape, but two whose constraints are only the same up to
    /// a different numbering might not, which keeps more than needed but loses nothing.
    pub(crate) fn canonicalize(&mut self) {
        let mut parents: Vec<usize> = (0..self.locals).collect();

        for constraint in &self.constraints {
            let mut locals = constraint.locals();

            if let Some(first) = locals.next() {
                let first = find(&mut parents, first);

                for other in locals {
                    let other = find(&mut parents, other);
                    parents[other] = first;
                }
            }
        }

        let mut ground = Vec::new();
        let mut components: FxIndexMap<usize, Vec<Constraint>> = FxIndexMap::default();

        for constraint in mem::take(&mut self.constraints) {
            let local = constraint.locals().next();

            match local {
                Some(local) => {
                    let root = find(&mut parents, local);
                    components.entry(root).or_default().push(constraint);
                }
                None => ground.push(constraint),
            }
        }

        ground.sort();
        ground.dedup();

        let mut shapes: Vec<(Vec<Constraint>, usize)> =
            components.into_values().map(shape).collect();
        shapes.sort();

        self.constraints = ground;
        self.locals = 0;

        for (index, (constraints, locals)) in shapes.iter().enumerate() {
            if index >= 2 && shapes[index - 2].0 == *constraints {
                continue;
            }

            let offset = self.locals;
            self.locals += locals;
            self.constraints
                .extend(constraints.iter().map(|constraint| {
                    constraint.map_vars(|var| match var {
                        Var::Key(_) => var,
                        Var::Local(index) => Var::Local(offset + index),
                    })
                }));
        }
    }
}

/// Returns the representative of the local binder's component.
fn find(parents: &mut [usize], mut local: usize) -> usize {
    while parents[local] != local {
        parents[local] = parents[parents[local]];
        local = parents[local];
    }

    local
}

/// Renumbers the local binders of a component from 0, and returns its sorted constraints and the
/// number of local binders (see `Summary::canonicalize`).
fn shape(mut constraints: Vec<Constraint>) -> (Vec<Constraint>, usize) {
    // A stable sort, so that constraints that are the same apart from their local binders stay in
    // the order they were added in
    constraints.sort_by_cached_key(|constraint| {
        constraint.map_vars(|var| match var {
            Var::Key(_) => var,
            Var::Local(_) => Var::Local(0),
        })
    });

    let mut numbers: FxHashMap<usize, usize> = FxHashMap::default();
    let mut constraints: Vec<Constraint> = constraints
        .iter()
        .map(|constraint| {
            constraint.map_vars(|var| match var {
                Var::Key(_) => var,
                Var::Local(index) => {
                    let next = numbers.len();
                    Var::Local(*numbers.entry(index).or_insert(next))
                }
            })
        })
        .collect();

    constraints.sort();
    constraints.dedup();
    (constraints, numbers.len())
}

#[cfg(test)]
mod tests {
    use id_arena::Arena;

    use super::*;
    use crate::{Generic, Type};

    struct Types {
        arena: Arena<Type>,
    }

    impl Types {
        fn new() -> Self {
            Self {
                arena: Arena::new(),
            }
        }

        fn generic(&mut self) -> TypeId {
            self.arena.alloc(Type::Generic(Generic { name: None }))
        }
    }

    fn bound(var: Var, upper: bool, ty: TypeId, env: Env) -> Constraint {
        Constraint {
            var,
            upper,
            ty,
            env,
        }
    }

    /// `K0 ≥ ty` and `K0 ≤ ty`, where `ty` is a generic bound inside the comparison, like the
    /// bounds of an instance `U` from comparing it with a rigid `V` both ways.
    fn local_both_ways(types: &mut Types) -> Summary {
        let generic = types.generic();
        let mut summary = Summary::default();
        let local = summary.local();
        summary.add(bound(Var::Key(0), false, generic, vec![(generic, local)]));
        summary.add(bound(Var::Key(0), true, generic, vec![(generic, local)]));
        summary
    }

    #[test]
    fn test_apply_maps_key_binders_and_renames_locals() {
        let mut types = Types::new();
        let child = local_both_ways(&mut types);
        let generic = child.constraints()[0].ty;

        let mut parent = Summary::default();
        let own = parent.local();
        parent.apply(&child, &[own]);
        parent.apply(&child, &[Var::Key(3)]);

        assert_eq!(
            parent.constraints(),
            [
                bound(
                    Var::Local(0),
                    false,
                    generic,
                    vec![(generic, Var::Local(1))]
                ),
                bound(Var::Local(0), true, generic, vec![(generic, Var::Local(1))]),
                bound(Var::Key(3), false, generic, vec![(generic, Var::Local(2))]),
                bound(Var::Key(3), true, generic, vec![(generic, Var::Local(2))]),
            ]
        );
    }

    #[test]
    fn test_take() {
        let mut types = Types::new();
        let ty = types.generic();
        let mut summary = Summary::default();
        let local = summary.local();
        summary.add(bound(Var::Key(0), false, ty, vec![]));
        summary.add(bound(local, true, ty, vec![]));
        summary.add(bound(Var::Key(1), true, ty, vec![(ty, local)]));
        summary.add(bound(local, false, ty, vec![(ty, Var::Key(0))]));

        assert_eq!(
            summary.take(local),
            [
                bound(local, true, ty, vec![]),
                bound(local, false, ty, vec![(ty, Var::Key(0))]),
            ]
        );

        // A constraint that only refers to it in its environment stays
        assert_eq!(
            summary.constraints(),
            [
                bound(Var::Key(0), false, ty, vec![]),
                bound(Var::Key(1), true, ty, vec![(ty, local)]),
            ]
        );
    }

    #[test]
    fn test_canonicalize_dedupes_constraints_without_locals() {
        let mut types = Types::new();
        let [a, b] = [types.generic(), types.generic()];
        let mut summary = Summary::default();
        summary.add(bound(Var::Key(1), true, b, vec![]));
        summary.add(bound(Var::Key(0), false, a, vec![(a, Var::Key(1))]));
        summary.add(bound(Var::Key(1), true, b, vec![]));
        summary.canonicalize();

        assert_eq!(
            summary.constraints(),
            [
                bound(Var::Key(0), false, a, vec![(a, Var::Key(1))]),
                bound(Var::Key(1), true, b, vec![]),
            ]
        );
    }

    /// Applying a summary again and again keeps two copies of its local binders, which have
    /// different bounds to check against each other than one copy has.
    #[test]
    fn test_canonicalize_keeps_two_copies() {
        let mut types = Types::new();
        let child = local_both_ways(&mut types);
        let mut twice = Summary::default();

        for _ in 0..2 {
            twice.apply(&child, &[Var::Key(0)]);
        }

        twice.canonicalize();
        assert_eq!(twice.locals, 2);
        assert_eq!(twice.constraints().len(), 4);

        let mut five_times = Summary::default();

        for _ in 0..5 {
            five_times.apply(&child, &[Var::Key(0)]);
        }

        five_times.canonicalize();
        assert_eq!(five_times, twice);

        // Once is different, since then there's only one local binder
        let mut once = child.clone();
        once.canonicalize();
        assert_ne!(once, twice);
        assert_eq!(once.locals, 1);
    }

    /// Copies that bound different key binders aren't the same shape.
    #[test]
    fn test_canonicalize_keeps_copies_of_other_shapes() {
        let mut types = Types::new();
        let child = local_both_ways(&mut types);
        let mut summary = Summary::default();

        for key in [0, 1, 2, 0, 1, 2, 0] {
            summary.apply(&child, &[Var::Key(key)]);
        }

        summary.canonicalize();
        assert_eq!(summary.locals, 6);
        assert_eq!(summary.constraints().len(), 12);
    }

    /// Local binders that share a constraint are one component, which is kept or dropped whole.
    #[test]
    fn test_canonicalize_keeps_components_whole() {
        let mut types = Types::new();
        let [a, b] = [types.generic(), types.generic()];
        let mut child = Summary::default();
        let (x, y) = (child.local(), child.local());
        child.add(bound(Var::Key(0), false, a, vec![(a, x)]));
        child.add(bound(x, true, b, vec![(b, y)]));
        child.add(bound(Var::Key(0), true, b, vec![(b, y)]));

        let mut summary = Summary::default();

        for _ in 0..3 {
            summary.apply(&child, &[Var::Key(0)]);
        }

        summary.canonicalize();
        assert_eq!(summary.locals, 4);
        assert_eq!(summary.constraints().len(), 6);

        for constraint in summary.constraints() {
            assert!(constraint.locals().all(|local| local < 4));
        }
    }

    /// The order that constraints were added in, and how the local binders were numbered, doesn't
    /// change the canonical form.
    #[test]
    fn test_canonicalize_is_independent_of_order() {
        let mut types = Types::new();
        let both_ways = local_both_ways(&mut types);
        let ty = types.generic();
        let mut lower = Summary::default();
        let local = lower.local();
        lower.add(bound(Var::Key(0), false, ty, vec![(ty, local)]));

        let mut first = Summary::default();
        first.apply(&both_ways, &[Var::Key(1)]);
        first.apply(&lower, &[Var::Key(0)]);
        first.add(bound(Var::Key(1), true, ty, vec![]));

        let mut second = Summary::default();
        second.add(bound(Var::Key(1), true, ty, vec![]));
        second.apply(&lower, &[Var::Key(0)]);
        second.apply(&both_ways, &[Var::Key(1)]);

        assert_ne!(first, second);
        first.canonicalize();
        second.canonicalize();
        assert_eq!(first, second);

        // It's already canonical
        let canonical = first.clone();
        first.canonicalize();
        assert_eq!(first, canonical);
    }
}
