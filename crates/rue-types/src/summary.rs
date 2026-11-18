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

use crate::{FxHashMap, TypeId};

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
    /// that refer to none are kept once each. Each component is renumbered from 0 in a way that
    /// doesn't depend on how it was numbered before (its shape, see `shape`), and at most two
    /// components of each shape are kept. That keeps every pair of bounds that can be checked
    /// against each other, up to renaming the local binders: two bounds from one copy (with the
    /// same local binders), and two from different copies (with different ones). A third copy only
    /// adds more pairs like those.
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
        let mut components: FxHashMap<usize, Vec<Constraint>> = FxHashMap::default();

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

        let mut shapes: Vec<(Vec<Constraint>, usize)> = components
            .values()
            .map(|constraints| shape(constraints))
            .collect();
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
///
/// The numbering is the least one, by the sorted constraints it gives, among those that colour
/// refinement allows: each local binder is told apart by the constraints it's in and its positions
/// in them, then by the colours of the binders it shares them with, until that stops telling more
/// of them apart. Binders that are still alike are tried in each order, which is rarely more than
/// a few, since a component comes from one comparison. A component whose binders are alike in so
/// many ways that trying them takes more than `SHAPE_SEARCH_LIMIT` numberings keeps the least one
/// found by then, so that its shape can depend on how it was numbered, which keeps more copies
/// than needed but loses nothing.
fn shape(constraints: &[Constraint]) -> (Vec<Constraint>, usize) {
    let mut locals: Vec<usize> = constraints.iter().flat_map(Constraint::locals).collect();
    locals.sort_unstable();
    locals.dedup();

    let constraints: Vec<Constraint> = constraints
        .iter()
        .map(|constraint| {
            constraint.map_vars(|var| match var {
                Var::Key(_) => var,
                Var::Local(local) => Var::Local(locals.binary_search(&local).unwrap()),
            })
        })
        .collect();

    let mut search = ShapeSearch {
        constraints: &constraints,
        best: None,
        numberings: 0,
    };
    search.search(vec![0; locals.len()]);
    (search.best.unwrap(), locals.len())
}

/// The most numberings of a component's local binders that `shape` tries.
const SHAPE_SEARCH_LIMIT: usize = 64;

struct ShapeSearch<'a> {
    /// The component's constraints, with its local binders numbered from 0.
    constraints: &'a [Constraint],
    /// The least sorted constraints found so far.
    best: Option<Vec<Constraint>>,
    numberings: usize,
}

impl ShapeSearch<'_> {
    /// Tries each numbering that the colours of the local binders allow, after refining them.
    fn search(&mut self, colours: Vec<usize>) {
        let colours = self.refine(colours);
        let count = colours.len();

        // The first binder of the least colour that more than one binder has
        let tied = (0..count)
            .filter(|&local| colours.iter().filter(|&&c| c == colours[local]).count() > 1)
            .min_by_key(|&local| colours[local]);

        let Some(tied) = tied else {
            let mut constraints: Vec<Constraint> = self
                .constraints
                .iter()
                .map(|constraint| recolour(constraint, &colours))
                .collect();
            constraints.sort();
            constraints.dedup();

            if self.best.as_ref().is_none_or(|best| constraints < *best) {
                self.best = Some(constraints);
            }

            self.numberings += 1;
            return;
        };

        // Each binder of that colour is tried first, before the others of the same colour
        let colour = colours[tied];

        for local in (0..count).filter(|&local| colours[local] == colour) {
            if self.best.is_some() && self.numberings >= SHAPE_SEARCH_LIMIT {
                return;
            }

            let individualized = colours
                .iter()
                .enumerate()
                .map(|(other, &c)| 2 * c + usize::from(c == colour && other != local))
                .collect();
            self.search(individualized);
        }
    }

    /// Tells the local binders apart by the colours of the constraints they're in (with each
    /// binder replaced by its colour), and their positions in them, until that stops telling more
    /// of them apart. Colours are numbered in the order of what tells them apart, which doesn't
    /// depend on how the binders are numbered, and a binder that had a lesser colour than another
    /// keeps one.
    fn refine(&self, mut colours: Vec<usize>) -> Vec<usize> {
        loop {
            let signatures: Vec<Signature> = (0..colours.len())
                .map(|local| {
                    let mut uses: Vec<(Constraint, Vec<usize>)> = self
                        .constraints
                        .iter()
                        .filter_map(|constraint| {
                            let positions = positions(constraint, local);
                            (!positions.is_empty())
                                .then(|| (recolour(constraint, &colours), positions))
                        })
                        .collect();
                    uses.sort();
                    (colours[local], uses)
                })
                .collect();

            let mut distinct = signatures.clone();
            distinct.sort();
            distinct.dedup();

            let before = {
                let mut before = colours.clone();
                before.sort_unstable();
                before.dedup();
                before.len()
            };

            colours = signatures
                .iter()
                .map(|signature| distinct.binary_search(signature).unwrap())
                .collect();

            if distinct.len() == before {
                return colours;
            }
        }
    }
}

/// What tells a local binder apart in `ShapeSearch::refine`: its colour, and the constraints it's in
/// (with each local binder replaced by its colour) along with its positions in them.
type Signature = (usize, Vec<(Constraint, Vec<usize>)>);

/// Returns the constraint with each local binder replaced by its colour.
fn recolour(constraint: &Constraint, colours: &[usize]) -> Constraint {
    constraint.map_vars(|var| match var {
        Var::Key(_) => var,
        Var::Local(local) => Var::Local(colours[local]),
    })
}

/// The positions of the local binder in the constraint: 0 for the binder it bounds, and 1 onward
/// for its environment.
fn positions(constraint: &Constraint, local: usize) -> Vec<usize> {
    [constraint.var]
        .into_iter()
        .chain(constraint.env.iter().map(|&(_, var)| var))
        .enumerate()
        .filter(|&(_, var)| var == Var::Local(local))
        .map(|(position, _)| position)
        .collect()
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

    /// Local binders in a component that are only told apart by the binders they share
    /// constraints with are numbered the same way whatever order the constraints are in, so
    /// copies of them in each order are one shape.
    #[test]
    fn test_canonicalize_numbers_alike_locals_by_structure() {
        let mut types = Types::new();
        let ty = types.generic();
        let generics = [types.generic(), types.generic(), types.generic()];

        let child = |reversed: bool| {
            let mut child = Summary::default();
            let locals = [child.local(), child.local(), child.local()];
            let env = |locals: [Var; 3]| generics.into_iter().zip(locals).collect();
            let mut constraints = vec![
                bound(Var::Key(0), false, ty, env(locals)),
                bound(
                    Var::Key(0),
                    false,
                    ty,
                    env([locals[0], locals[0], locals[2]]),
                ),
            ];

            if reversed {
                constraints.reverse();
            }

            for constraint in constraints {
                child.add(constraint);
            }

            child
        };

        let mut forward = child(false);
        let mut reversed = child(true);
        forward.canonicalize();
        reversed.canonicalize();
        assert_eq!(forward, reversed);

        let canonical = forward.clone();
        forward.canonicalize();
        assert_eq!(forward, canonical);

        let mut summary = Summary::default();

        for reversed in [false, true, false, true, true, false] {
            summary.apply(&child(reversed), &[Var::Key(0)]);
        }

        summary.canonicalize();
        assert_eq!(summary.locals, 6);
        assert_eq!(summary.constraints().len(), 4);
    }

    /// A component whose local binders can't be told apart at all, such as a cycle of them, has
    /// the same shape however it's numbered.
    #[test]
    fn test_canonicalize_numbers_symmetric_locals() {
        let mut types = Types::new();
        let [ty, a] = [types.generic(), types.generic()];

        let cycle = |order: [usize; 4]| {
            let mut child = Summary::default();
            let locals = [child.local(), child.local(), child.local(), child.local()];

            for i in order {
                child.add(bound(locals[i], true, ty, vec![(a, locals[(i + 1) % 4])]));
            }

            child.canonicalize();
            child
        };

        let first = cycle([0, 1, 2, 3]);
        assert_eq!(first, cycle([2, 0, 3, 1]));
        assert_eq!(first, cycle([3, 2, 1, 0]));
        assert_eq!(first.locals, 4);
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
