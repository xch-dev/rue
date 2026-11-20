//! Summaries of the bounds that a comparison puts on generics being instantiated (#362's step 13b).
//!
//! A comparison with a binding to a generic being instantiated collects bounds into it. They used
//! to be a side effect: revisiting the comparison merged the two instances, so what it did depended
//! on which comparisons were in progress, and its result couldn't be reused exactly (#344). A
//! summary records those bounds as data instead, in the comparison's own coordinates: the binders
//! that its key numbers (see `Binding`), and the ones bound inside of it. A comparison's result is
//! then its value and its summary, which only depend on its key. Reaching it again while it's in
//! progress applies the summary to the binders it's reached with (see `StackFrame` in
//! `compare.rs`).
//!
//! A bound collected under a member of a union compared inside of the comparison records that
//! member, and the summary records the union and what its members gave (see `ChoiceRecord`), so
//! that applying it collects the bound under the same member of the same union again, and the
//! members stay alternatives (#362's step 13g).

use std::{cell::Cell, rc::Rc};

use crate::{Comparison, FALLBACK, FxHashMap, TypeId};

std::thread_local! {
    /// The work that canonicalizing summaries has done on this thread since it was last taken (see
    /// `take_work`).
    static WORK: Cell<usize> = const { Cell::new(0) };
}

/// How many items canonicalizing goes through per step of comparing that it counts as. It's only
/// there so that a summary that grows large makes the comparison give up rather than take ever
/// longer, so it's coarse enough not to count for the small summaries that most comparisons have.
const ITEMS_PER_STEP: usize = 6;

/// Counts work that canonicalizing a summary does, going through that many items, in steps of
/// comparing, so that a comparison whose summaries grow large gives up rather than taking ever
/// longer (see `take_work`).
fn work(items: usize) {
    let steps = items / ITEMS_PER_STEP;
    WORK.with(|work| work.set(work.get() + steps));
    crate::steps::step_many(steps);
}

/// Takes the work that canonicalizing summaries has done on this thread since it was last taken,
/// which the comparison in progress counts towards its step limit.
pub(crate) fn take_work() -> usize {
    WORK.with(|work| work.replace(0))
}

/// A binder in a summary's coordinates.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum Var {
    /// The binder that the comparison's key numbers this way, which is bound outside of it.
    Key(usize),
    /// A binder that's bound inside of the comparison. Each time the summary is applied, these are
    /// new binders, since each place it's applied binds its own. One that a constraint bounds is
    /// the instance of a generic function compared inside of the comparison, which an enclosing
    /// one can still instantiate, and the others are rigid.
    Local(usize),
}

/// What the generics that a bound's type references are bound to, sorted by generic. A generic
/// that isn't listed is free.
pub(crate) type Env = Vec<(TypeId, Var)>;

/// A bound of a binder being instantiated: the type flows into it (a lower bound), or it flows
/// into the type (an upper bound).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Constraint {
    /// The binder being instantiated.
    pub(crate) var: Var,
    /// Whether it's an upper bound rather than a lower one.
    pub(crate) upper: bool,
    /// The type that flows into the binder, or that it flows into.
    pub(crate) ty: TypeId,
    /// What the generics that `ty` references are bound to.
    pub(crate) env: Env,
    /// The union member inside of the comparison that it was collected under, if any. Where the
    /// summary is applied, it's collected under that member again, so whether it decides the
    /// result depends on the union's members as it did in the comparison (#360).
    pub(crate) branch: Option<Branch>,
    /// Whether it was collected under a union member inside of the comparison that gave `Invalid`
    /// (or one nested in such a member), which can never be picked, so it doesn't decide whether
    /// the instance's bounds hold wherever the summary is applied either (see
    /// `Choices::can_hold_since`). Its union isn't kept (`branch` is `None`): whichever members it
    /// was under, it only matters that none of them can hold.
    pub(crate) unmatched: bool,
}

/// A member of a union compared inside of the comparison: the union's local binder (see
/// `ChoiceRecord`), and the member's index (`FALLBACK` for the fallback).
pub(crate) type Branch = (Var, usize);

/// A union that a value was compared with inside of the comparison, which a constraint was
/// collected under a member of (see `Choices` in `infer.rs`). It's numbered like a local binder,
/// so that canonicalizing the summary renumbers it along with the binders its constraints refer
/// to, and it's a new union each time the summary is applied.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct ChoiceRecord {
    /// The union, as a local binder.
    pub(crate) choice: Var,
    /// The member of a union inside of the comparison that it was compared under, if any.
    pub(crate) parent: Option<Branch>,
    /// What comparing the value with each member gave, if it was compared.
    pub(crate) members: Vec<Option<Comparison>>,
    /// What the fallback gave, if it was compared.
    pub(crate) fallback: Option<Comparison>,
    /// The members (with the fallback last) that collected bounds for a generic being
    /// instantiated.
    pub(crate) bounded: Vec<bool>,
}

impl ChoiceRecord {
    /// Returns the record with each local binder it refers to replaced.
    fn map_locals(&self, mut f: impl FnMut(usize) -> usize) -> Self {
        let mut map = |var: Var| match var {
            Var::Key(_) => var,
            Var::Local(local) => Var::Local(f(local)),
        };

        Self {
            choice: map(self.choice),
            parent: self.parent.map(|(choice, member)| (map(choice), member)),
            ..self.clone()
        }
    }

    /// Whether the member is the only one that can hold the value: every other member that was
    /// compared, and the fallback if it was, gave `Invalid`. Then the union holds exactly when the
    /// member does, along with what's under it, so leaving the union out and putting what's under
    /// the member under the member the union is nested in decides the same (see
    /// `Choices::attributed`).
    fn alone(&self, member: usize) -> bool {
        // A member that has no result, such as one left out as alike another (see
        // `alike_member`), doesn't count as one that can't hold: the union is kept for it, which
        // is exact. The fallback is only compared when no member holds
        let members = self
            .members
            .iter()
            .enumerate()
            .all(|(other, &result)| other == member || result == Some(Comparison::Invalid));

        members
            && (member == FALLBACK
                || self
                    .fallback
                    .is_none_or(|result| result == Comparison::Invalid))
    }

    /// Whether a member (or the fallback) that gave `Assign` has nothing under it that a failed
    /// pair could be charged to wherever the summary is applied: it collected no bounds, like
    /// `Choices::needed_in`, or nothing in the summary is under it (`used` are the members that
    /// something is), such as a member whose bounds were all on the comparison's own instances,
    /// which were checked inside of it. Picking it then gives `Assign` (see
    /// `Choices::attributed`). A member left out as alike another has no result, so it doesn't
    /// hold the union (see `alike_member`).
    fn held(&self, used: Option<&Vec<usize>>) -> bool {
        self.members
            .iter()
            .enumerate()
            .chain([(FALLBACK, &self.fallback)])
            .zip(&self.bounded)
            .any(|((member, &result), &bounded)| {
                result == Some(Comparison::Assign)
                    && (!bounded || used.is_none_or(|used| !used.contains(&member)))
            })
    }

    /// The binders that the record refers to: the union, then the one it's nested in.
    fn vars(&self) -> impl Iterator<Item = Var> + '_ {
        [self.choice]
            .into_iter()
            .chain(self.parent.map(|(choice, _)| choice))
    }
}

impl Constraint {
    /// Returns the constraint with each binder it refers to replaced.
    fn map_vars(&self, mut f: impl FnMut(Var) -> Var) -> Self {
        Self {
            var: f(self.var),
            upper: self.upper,
            ty: self.ty,
            branch: self.branch.map(|(choice, member)| (f(choice), member)),
            unmatched: self.unmatched,
            env: self
                .env
                .iter()
                .map(|&(generic, var)| (generic, f(var)))
                .collect(),
        }
    }

    /// Returns the constraint with each local binder it refers to replaced.
    fn map_locals(&self, mut f: impl FnMut(usize) -> usize) -> Self {
        self.map_vars(|var| match var {
            Var::Key(_) => var,
            Var::Local(local) => Var::Local(f(local)),
        })
    }

    /// The binders that the constraint refers to: the one it bounds, then those of its
    /// environment, then the union it was collected under a member of.
    fn vars(&self) -> impl Iterator<Item = Var> + '_ {
        [self.var]
            .into_iter()
            .chain(self.env.iter().map(|&(_, var)| var))
            .chain(self.branch.map(|(choice, _)| choice))
    }
}

/// A constraint or a union, which canonicalizing a summary treats alike, since both refer to local
/// binders (see `Summary::canonicalize`).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Item {
    Choice(ChoiceRecord),
    Constraint(Constraint),
}

impl Item {
    /// Returns the item with each local binder it refers to replaced.
    fn map_locals(&self, f: impl FnMut(usize) -> usize) -> Self {
        match self {
            Self::Choice(choice) => Self::Choice(choice.map_locals(f)),
            Self::Constraint(constraint) => Self::Constraint(constraint.map_locals(f)),
        }
    }

    /// The binders that the item refers to, in order.
    fn vars(&self) -> Vec<Var> {
        match self {
            Self::Choice(choice) => choice.vars().collect(),
            Self::Constraint(constraint) => constraint.vars().collect(),
        }
    }

    /// The local binders that the item refers to, in order.
    fn locals(&self) -> Vec<usize> {
        self.vars()
            .into_iter()
            .filter_map(|var| match var {
                Var::Key(_) => None,
                Var::Local(local) => Some(local),
            })
            .collect()
    }
}

/// The bounds that a comparison puts on the binders it refers to (see the module documentation).
#[derive(Debug, Clone, Default)]
pub(crate) struct Summary {
    /// The number of local binders, including the unions (see `ChoiceRecord`).
    locals: usize,
    /// The bounds, in the order they were added until the summary is canonicalized.
    constraints: Vec<Constraint>,
    /// The unions that the bounds were collected under members of, in the order they were added
    /// until the summary is canonicalized, which puts a union before those nested in it.
    choices: Vec<ChoiceRecord>,
    /// Whether unions nested too deeply were left out of it (see `MAX_UNION_DEPTH`), so that
    /// applying it can reject more than computing the comparison again would.
    approximate: bool,
    /// Whether it's canonical (see `Summary::canonicalize`), so that joining it with another
    /// doesn't need to leave out the unions that decide nothing again.
    canonical: bool,
    /// Its items that refer to no local binder and the shapes of its components, once it's
    /// canonical, which joining it with another starts from rather than working them out again.
    components: Option<Rc<Components>>,
}

/// The items of a summary that refer to no local binder, and the shape of each of its components
/// (see `Summary::canonicalize`).
type Components = (Vec<Item>, Vec<(Vec<Item>, usize)>);

impl PartialEq for Summary {
    fn eq(&self, other: &Self) -> bool {
        self.locals == other.locals
            && self.constraints == other.constraints
            && self.choices == other.choices
            && self.approximate == other.approximate
    }
}

impl Eq for Summary {}

impl Summary {
    /// The number of local binders, including the unions (see `ChoiceRecord`).
    pub(crate) fn locals(&self) -> usize {
        self.locals
    }

    /// The bounds, in the order they were added until the summary is canonicalized.
    pub(crate) fn constraints(&self) -> &[Constraint] {
        &self.constraints
    }

    /// Binds a new local binder, and returns it.
    pub(crate) fn local(&mut self) -> Var {
        self.locals += 1;
        Var::Local(self.locals - 1)
    }

    /// Whether unions nested too deeply were left out of it (see `MAX_UNION_DEPTH`).
    pub(crate) fn is_approximate(&self) -> bool {
        self.approximate
    }

    /// The unions that the bounds were collected under members of (see `ChoiceRecord`).
    pub(crate) fn choices(&self) -> &[ChoiceRecord] {
        &self.choices
    }

    /// Adds a bound, such as one that a comparison collects for an instance it refers to.
    pub(crate) fn add(&mut self, constraint: Constraint) {
        self.canonical = false;
        self.components = None;
        self.constraints.push(constraint);
    }

    /// Adds a union that bounds are collected under members of, whose local binder is new.
    pub(crate) fn add_choice(&mut self, choice: ChoiceRecord) {
        self.canonical = false;
        self.components = None;
        self.choices.push(choice);
    }

    /// Adds the constraints of a comparison that this one reached, where `args` are what the
    /// binders its key numbers are in this one's coordinates. Its local binders are new ones.
    /// Comparisons apply a summary to the binders themselves instead (see `apply_summary` in
    /// `compare.rs`), so this is only used by the tests of canonical forms.
    #[cfg(test)]
    pub(crate) fn apply(&mut self, other: &Self, args: &[Var]) {
        self.canonical = false;
        self.components = None;
        let offset = self.locals;
        self.locals += other.locals;

        for constraint in &other.constraints {
            let constraint = constraint.map_vars(|var| match var {
                Var::Key(index) => args[index],
                Var::Local(index) => Var::Local(offset + index),
            });
            self.constraints.push(constraint);
        }

        self.choices.extend(
            other
                .choices
                .iter()
                .map(|choice| choice.map_locals(|local| offset + local)),
        );
    }

    /// The least summary that has the constraints of both, in a canonical form: each component (see
    /// `Summary::canonicalize`) is there as many times as in the one that has it more often, rather
    /// than as many times as in both. A component that's there twice isn't the same as one that's
    /// there once: two copies of a union that bound the same key binder are two unions, whose
    /// members' bounds are checked against each other across them (see `check_bounds` in
    /// `compare.rs`), which a single one doesn't have. So adding up the copies would make a summary
    /// joined with itself (the same bounds from another round) put more on the instances than it
    /// does, and a comparison computed in rounds would put more on its revisits than the same
    /// comparison reused from the memo.
    pub(crate) fn join(&self, other: &Self) -> Self {
        let (mut ground, lhs, approximate) = self.components();
        let (other_ground, rhs, other_approximate) = other.components();
        ground.extend(other_ground);
        ground.sort();
        ground.dedup();

        // Both are sorted by shape, so each shape's copies are next to each other in each
        let mut shapes = Vec::with_capacity(lhs.len().max(rhs.len()));
        let (mut lhs, mut rhs) = (lhs.into_iter().peekable(), rhs.into_iter().peekable());

        loop {
            let next = match (lhs.peek(), rhs.peek()) {
                (None, None) => break,
                (Some(_), None) => lhs.next(),
                (None, Some(_)) => rhs.next(),
                (Some(a), Some(b)) => match a.cmp(b) {
                    std::cmp::Ordering::Less => lhs.next(),
                    std::cmp::Ordering::Greater => rhs.next(),
                    // A copy that both have is kept once
                    std::cmp::Ordering::Equal => {
                        rhs.next();
                        lhs.next()
                    }
                },
            };
            shapes.extend(next);
        }

        let mut joined = Self::default();
        joined.rebuild(ground, &shapes);
        joined.approximate = approximate || other_approximate;
        joined.canonical = true;
        joined
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
    /// adds more pairs like those. Keeping two copies relies on checking a pair not adding bounds
    /// to other binders, which it doesn't to the instances outside of the function whose bounds are
    /// being checked, since they're rigid while checking them. It can add bounds to a local binder
    /// that's an instance (one that a constraint bounds), which can still be instantiated then
    /// (#363), and that one's bounds are checked against each other afterwards: a copy can get a
    /// bound involving each of two other copies, and checking those against each other needs
    /// three. A fourth only adds more like those, so a component that bounds a local binder keeps
    /// at most three copies. That's kept on this argument: no test so far shows a result that
    /// needs the third.
    ///
    /// A component that's too alike to be renumbered exactly (see `shape`) can be renumbered
    /// another way when it's canonicalized again.
    pub(crate) fn canonicalize(&mut self) {
        let (ground, shapes, approximate) = self.components();
        self.rebuild(ground, &shapes);
        self.approximate = approximate;
        self.canonical = true;
    }

    /// The items that refer to no local binder, sorted and without duplicates, and the shape of
    /// each component (see `shape`), sorted (see `Summary::canonicalize`).
    #[allow(clippy::type_complexity)]
    fn components(&self) -> (Vec<Item>, Vec<(Vec<Item>, usize)>, bool) {
        if let (true, Some(components)) = (self.canonical, &self.components) {
            let (ground, shapes) = components.as_ref().clone();
            return (ground, shapes, self.approximate);
        }

        let (choices, constraints, approximate) = if self.canonical {
            (
                self.choices.clone(),
                self.constraints.clone(),
                self.approximate,
            )
        } else {
            self.without_single_members()
        };
        let items: Vec<Item> = choices
            .into_iter()
            .map(Item::Choice)
            .chain(constraints.into_iter().map(Item::Constraint))
            .collect();

        let mut parents: Vec<usize> = (0..self.locals).collect();

        for item in &items {
            if let Some((&first, others)) = item.locals().split_first() {
                let first = find(&mut parents, first);

                for &other in others {
                    let other = find(&mut parents, other);
                    parents[other] = first;
                }
            }
        }

        let mut ground = Vec::new();
        let mut components: FxHashMap<usize, Vec<Item>> = FxHashMap::default();

        for item in items {
            match item.locals().first() {
                Some(&local) => {
                    let root = find(&mut parents, local);
                    components.entry(root).or_default().push(item);
                }
                // Only a constraint can refer to no local binder, since a union is one
                None => ground.push(item),
            }
        }

        ground.sort();
        ground.dedup();

        let mut shapes: Vec<(Vec<Item>, usize)> =
            components.values().map(|items| shape(items)).collect();
        shapes.sort();
        (ground, shapes, approximate)
    }

    /// The summary's unions and constraints, with the unions that decide nothing left out, which
    /// keeps a summary that's applied under a union member of a comparison that's summarized
    /// itself from nesting its unions one level deeper each time, without bound:
    /// - A union held by a member that gave `Assign` with nothing under it, with what's under its
    ///   other members (see `ChoiceRecord::held` and `Choices::needed_in`).
    /// - A third copy of a union under the same member (see `extra_copy`).
    /// - A member alike another of its union, with what's under it (see `alike_member`).
    /// - A union that has something under only one of its members, the only one that can hold
    ///   (see `ChoiceRecord::alone`), with what's under that member put under the member the union
    ///   is nested in instead. It holds exactly when what's under that member does.
    /// - A union nested more deeply than `MAX_UNION_DEPTH`, which the summary is approximate for.
    ///
    /// (What's under a member that gave `Invalid` isn't under a union at all, see
    /// `Constraint::unmatched`.) Each of them but the last decides the same as the summary without
    /// it, under how a failed pair of bounds is charged to the members it was collected under
    /// (see `Choices::attributed`).
    fn without_single_members(&self) -> (Vec<ChoiceRecord>, Vec<Constraint>, bool) {
        let mut choices = self.choices.clone();
        let mut constraints = self.constraints.clone();
        let mut approximate = self.approximate;

        loop {
            let mut used: FxHashMap<Var, Vec<usize>> = FxHashMap::default();

            for (choice, member) in constraints
                .iter()
                .filter_map(|constraint| constraint.branch)
                .chain(choices.iter().filter_map(|choice| choice.parent))
            {
                let members = used.entry(choice).or_default();

                if !members.contains(&member) {
                    members.push(member);
                }
            }

            // A union that a member with nothing under it holds for its best result gives that
            // result whatever is under its other members, since what fails under them only
            // makes those members worse (see `Choices::attributed`). So it decides nothing beyond
            // its result, which the member it's nested in already has, and it's left out along
            // with what's under it, like a member that holds the value whatever the generics are
            // (#360)
            let held = choices
                .iter()
                .find(|choice| choice.held(used.get(&choice.choice)))
                .map(|choice| choice.choice);

            if let Some(extra) = held.or_else(|| extra_copy(&choices, &constraints)) {
                let under = |mut branch: Option<Branch>, choices: &[ChoiceRecord]| {
                    while let Some((current, _)) = branch {
                        if current == extra {
                            return true;
                        }

                        branch = choices
                            .iter()
                            .find(|other| other.choice == current)
                            .and_then(|other| other.parent);
                    }

                    false
                };

                constraints.retain(|constraint| !under(constraint.branch, &choices));
                let kept: Vec<ChoiceRecord> = choices
                    .iter()
                    .filter(|other| other.choice != extra && !under(other.parent, &choices))
                    .cloned()
                    .collect();
                choices = kept;
                continue;
            }

            if let Some((choice, member)) = alike_member(&choices, &constraints, &used) {
                let under = |mut branch: Option<Branch>, choices: &[ChoiceRecord]| {
                    while let Some(current) = branch {
                        if current == (choice, member) {
                            return true;
                        }

                        branch = choices
                            .iter()
                            .find(|other| other.choice == current.0)
                            .and_then(|other| other.parent);
                    }

                    false
                };

                constraints.retain(|constraint| !under(constraint.branch, &choices));
                let mut kept: Vec<ChoiceRecord> = choices
                    .iter()
                    .filter(|other| !under(other.parent, &choices))
                    .cloned()
                    .collect();

                // The member is left out of its union as well, rather than kept as a member that
                // nothing is under, which nothing could be charged to (see `Choices::attributed`)
                if let Some(record) = kept.iter_mut().find(|record| record.choice == choice) {
                    if member == FALLBACK {
                        record.fallback = None;
                    } else {
                        record.members[member] = None;
                    }
                }

                choices = kept;
                continue;
            }

            let depth = |choice: &ChoiceRecord| {
                let mut depth = 0;
                let mut parent = choice.parent;

                while let Some((var, _)) = parent {
                    depth += 1;
                    parent = choices
                        .iter()
                        .find(|other| other.choice == var)
                        .and_then(|other| other.parent);
                }

                depth
            };

            let alone = |choice: &ChoiceRecord| {
                used.get(&choice.choice)
                    .is_none_or(|members| members.len() == 1 && choice.alone(members[0]))
            };

            let Some(index) = choices
                .iter()
                .position(|choice| alone(choice) || depth(choice) > MAX_UNION_DEPTH)
            else {
                break;
            };

            // Folding a union that only one member can hold into the member it's nested in decides
            // the same (see `Choices::attributed`), but leaving out a union nested too deeply can
            // reject more, so the summary is approximate then
            approximate |= !alone(&choices[index]);

            let removed = choices.remove(index);
            let lift = |branch: &mut Option<Branch>| {
                if branch.is_some_and(|(choice, _)| choice == removed.choice) {
                    *branch = removed.parent;
                }
            };

            for constraint in &mut constraints {
                lift(&mut constraint.branch);
            }

            for choice in &mut choices {
                lift(&mut choice.parent);
            }
        }

        (choices, constraints, approximate)
    }

    /// Replaces the summary's items with `ground` and the components of `shapes`, which are
    /// sorted, keeping at most as many copies of each shape as `Summary::canonicalize` says.
    fn rebuild(&mut self, ground: Vec<Item>, shapes: &[(Vec<Item>, usize)]) {
        self.constraints.clear();
        self.choices.clear();
        self.locals = 0;
        let mut kept_shapes = Vec::new();
        let mut kept = ground.clone();

        for (index, (items, locals)) in shapes.iter().enumerate() {
            // A component that bounds a local binder (an instance) keeps a third copy
            let copies = if items.iter().any(|item| {
                matches!(item, Item::Constraint(constraint) if matches!(constraint.var, Var::Local(_)))
            }) {
                3
            } else {
                2
            };

            if index >= copies && shapes[index - copies].0 == *items {
                continue;
            }

            let offset = self.locals;
            self.locals += locals;
            kept.extend(
                items
                    .iter()
                    .map(|item| item.map_locals(|local| offset + local)),
            );
            kept_shapes.push((items.clone(), *locals));
        }

        self.components = Some(Rc::new((ground, kept_shapes)));

        for item in kept {
            match item {
                Item::Choice(choice) => self.choices.push(choice),
                Item::Constraint(constraint) => self.constraints.push(constraint),
            }
        }
    }
}

/// A member of a union in the summary that's alike another member of it, which can be left out
/// along with what's under it: both gave the same result and collected bounds alike, and what's
/// under them is the same up to renaming the local binders that only it refers to. Picking either
/// is then the same, since they're alternatives (see `Choices::exclusive`). Leaving out the others
/// keeps a union whose members each reach the same comparison again (such as `T | T` in a
/// recursive type) from doubling the summary each time it's computed again, which would keep
/// iterating from settling.
fn alike_member(
    choices: &[ChoiceRecord],
    constraints: &[Constraint],
    used: &FxHashMap<Var, Vec<usize>>,
) -> Option<Branch> {
    let parent_of = |choice: Var| {
        choices
            .iter()
            .find(|other| other.choice == choice)
            .and_then(|other| other.parent)
    };

    // The member of a union that a branch is directly or indirectly under
    let ancestors = |mut branch: Option<Branch>| {
        let mut ancestors = Vec::new();

        while let Some(current) = branch {
            ancestors.push(current);
            branch = parent_of(current.0);
        }

        ancestors
    };

    let items: Vec<(Item, Vec<Branch>)> = choices
        .iter()
        .map(|choice| (Item::Choice(choice.clone()), ancestors(choice.parent)))
        .chain(constraints.iter().map(|constraint| {
            (
                Item::Constraint(constraint.clone()),
                ancestors(constraint.branch),
            )
        }))
        .collect();
    let (item_locals, total) = local_counts(items.iter().map(|(item, _)| item));

    for choice in choices {
        let Some(members) = used.get(&choice.choice) else {
            continue;
        };

        if members.len() < 2 {
            continue;
        }

        let index = |member: usize| member.min(choice.members.len());
        let result = |member: usize| {
            if member == FALLBACK {
                choice.fallback
            } else {
                choice.members[member]
            }
        };

        // Only members that gave the same result and collected bounds alike can be alike, which is
        // cheap to tell, so the shapes are only worked out for those
        let signature = |member: usize| (result(member), choice.bounded[index(member)]);
        let candidates: Vec<usize> = members
            .iter()
            .copied()
            .filter(|&member| {
                members
                    .iter()
                    .filter(|&&other| signature(other) == signature(member))
                    .count()
                    > 1
            })
            .collect();

        if candidates.is_empty() {
            continue;
        }

        // What's under each member, with the member itself numbered alike, and the local binders
        // that something outside of it refers to kept apart from the others
        let shapes: Vec<(usize, Vec<Item>)> = candidates
            .iter()
            .map(|&member| {
                let under: Vec<&Item> = items
                    .iter()
                    .filter(|(_, ancestors)| ancestors.contains(&(choice.choice, member)))
                    .map(|(item, _)| item)
                    .collect();
                let mut outside = outside_of(
                    &item_locals,
                    &total,
                    items
                        .iter()
                        .map(|(_, ancestors)| ancestors.contains(&(choice.choice, member))),
                );

                if let Var::Local(local) = choice.choice {
                    outside[local] = true;
                }
                let normalized: Vec<Item> = under
                    .into_iter()
                    .map(|item| normalize(item, (choice.choice, member), &outside))
                    .collect();
                (member, shape(&normalized).0)
            })
            .collect();

        for (position, (member, items)) in shapes.iter().enumerate() {
            let alike = shapes[..position].iter().any(|(other, other_items)| {
                result(*other) == result(*member)
                    && choice.bounded[index(*other)] == choice.bounded[index(*member)]
                    && other_items == items
            });

            if alike {
                return Some((choice.choice, *member));
            }
        }
    }

    None
}

/// A union in the summary that's the third copy of another under the same member (or of none):
/// what's under it, along with the union itself, is the same as what's under each of two others
/// up to renaming the local binders that only it refers to. Like a third copy of a component (see
/// `Summary::canonicalize`), it only adds pairs of bounds like those the two others give, so it
/// can be left out, which keeps a union that's reached again and again from piling up copies.
fn extra_copy(choices: &[ChoiceRecord], constraints: &[Constraint]) -> Option<Var> {
    let parent_of = |choice: Var| {
        choices
            .iter()
            .find(|other| other.choice == choice)
            .and_then(|other| other.parent)
    };
    let unions_above = |mut branch: Option<Branch>| {
        let mut unions = Vec::new();

        while let Some((choice, _)) = branch {
            unions.push(choice);
            branch = parent_of(choice);
        }

        unions
    };

    let items: Vec<(Item, Vec<Var>)> = choices
        .iter()
        .map(|choice| {
            let mut unions = unions_above(choice.parent);
            unions.push(choice.choice);
            (Item::Choice(choice.clone()), unions)
        })
        .chain(constraints.iter().map(|constraint| {
            (
                Item::Constraint(constraint.clone()),
                unions_above(constraint.branch),
            )
        }))
        .collect();

    let (item_locals, total) = local_counts(items.iter().map(|(item, _)| item));
    let mut seen: Vec<(Option<Branch>, Vec<Item>, usize)> = Vec::new();

    for choice in choices {
        // Only a union with two others alike under the same member can be a third copy, which is
        // cheap to tell, so the shape is only worked out then
        let siblings = choices
            .iter()
            .filter(|other| other.parent == choice.parent && other.members == choice.members)
            .count();

        if siblings < 3 {
            continue;
        }

        let inside = |unions: &[Var]| unions.contains(&choice.choice);
        let outside = outside_of(
            &item_locals,
            &total,
            items.iter().map(|(_, unions)| inside(unions)),
        );
        let normalized: Vec<Item> = items
            .iter()
            .filter(|(_, unions)| inside(unions))
            .map(|(item, _)| normalize(item, (choice.choice, usize::MAX), &outside))
            .collect();
        work(normalized.len());
        let (shape, _) = shape(&normalized);

        match seen
            .iter_mut()
            .find(|(parent, other, _)| *parent == choice.parent && *other == shape)
        {
            Some((_, _, count)) if *count >= 2 => return Some(choice.choice),
            Some((_, _, count)) => *count += 1,
            None => seen.push((choice.parent, shape, 1)),
        }
    }

    None
}

/// The local binders that each item refers to, and how many times each local binder is referred
/// to in all of them.
fn local_counts<'a>(items: impl Iterator<Item = &'a Item>) -> (Vec<Vec<usize>>, Vec<usize>) {
    let item_locals: Vec<Vec<usize>> = items.map(Item::locals).collect();
    let size = item_locals
        .iter()
        .flatten()
        .max()
        .map_or(0, |&local| local + 1);
    let mut total = vec![0; size];

    for &local in item_locals.iter().flatten() {
        total[local] += 1;
    }

    (item_locals, total)
}

/// Which local binders something other than the items that `inside` picks out refers to (see
/// `local_counts`).
fn outside_of(
    item_locals: &[Vec<usize>],
    total: &[usize],
    inside: impl Iterator<Item = bool>,
) -> Vec<bool> {
    let mut within = vec![0; total.len()];

    for (locals, inside) in item_locals.iter().zip(inside) {
        if inside {
            for &local in locals {
                within[local] += 1;
            }
        }
    }

    total
        .iter()
        .zip(within)
        .map(|(&total, within)| total > within)
        .collect()
}

/// The item with the member that it's directly under numbered 0, and each local binder in
/// `outside` made a key binder past any real one, so that `shape` only renumbers the others.
fn normalize(item: &Item, member: Branch, outside: &[bool]) -> Item {
    let fixed = |var: Var| match var {
        Var::Local(local) if outside[local] => Var::Key(usize::MAX / 2 + local),
        var => var,
    };
    let branch = |branch: Option<Branch>| {
        branch.map(|branch| {
            let (choice, index) = if branch == member {
                (branch.0, 0)
            } else {
                branch
            };
            (fixed(choice), index)
        })
    };

    match item {
        Item::Choice(choice) => Item::Choice(ChoiceRecord {
            choice: fixed(choice.choice),
            parent: branch(choice.parent),
            ..choice.clone()
        }),
        Item::Constraint(constraint) => Item::Constraint(Constraint {
            var: fixed(constraint.var),
            env: constraint
                .env
                .iter()
                .map(|&(generic, var)| (generic, fixed(var)))
                .collect(),
            branch: branch(constraint.branch),
            ..constraint.clone()
        }),
    }
}

/// How deeply a summary's unions are nested in each other at most, a deliberate bound on how much
/// of them a summary keeps (#362's step 13g).
///
/// A comparison that's reached again inside of a union member, such as a recursive type compared
/// through a union, nests its summary's unions one level deeper each time it's computed again, so
/// a summary that kept them all would have no fixed point to iterate to: each round has a union
/// whose members each hold a copy of the previous round's (seed 1532 of
/// `test_memo_differential_large` doubled to 458,752 items by round 18). Those levels aren't
/// redundant, since picking a different member at each level is a different set of bounds, and
/// while there are only finitely many such sets, keeping them as sets instead (one per way of
/// picking a member of each union) is exponential in the number of unions.
///
/// So a union nested deeper is left out, and what's under its members is put under the member
/// it's nested in, which then needs what's under every one of them to hold, rather than one.
/// That's what a summary did before it kept unions, so it can only reject more than computing the
/// comparison again would, and a summary that left any out is marked approximate and never
/// reused from the memo, which keeps reusing exact. It's 2 since that's the least that keeps the
/// slowest comparisons of `nested_baseline.txt` (11.3, and 31451.1, .2 and .5) within the steps
/// they took before #362's step 13f: with 1, more summaries are approximate and aren't reused,
/// and 31451.5 takes 20 times as many.
const MAX_UNION_DEPTH: usize = 2;

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
/// found by then. Its shape can then depend on how it was numbered, and canonicalizing it again can
/// give another one, which keeps more copies than needed but loses nothing.
fn shape(constraints: &[Item]) -> (Vec<Item>, usize) {
    let mut locals: Vec<usize> = constraints.iter().flat_map(Item::locals).collect();
    locals.sort_unstable();
    locals.dedup();

    // A constraint that's there twice would tell its binders apart from the same ones without it
    let mut constraints: Vec<Item> = constraints
        .iter()
        .map(|constraint| constraint.map_locals(|local| locals.binary_search(&local).unwrap()))
        .collect();
    constraints.sort();
    constraints.dedup();

    let mut uses: Vec<Vec<(usize, Vec<usize>)>> = vec![Vec::new(); locals.len()];

    for (index, constraint) in constraints.iter().enumerate() {
        for (position, var) in constraint.vars().into_iter().enumerate() {
            if let Var::Local(local) = var {
                match uses[local].last_mut() {
                    Some((last, positions)) if *last == index => positions.push(position),
                    _ => uses[local].push((index, vec![position])),
                }
            }
        }
    }

    let mut search = ShapeSearch {
        constraints: &constraints,
        uses: &uses,
        best: Vec::new(),
        numberings: 0,
    };
    search.search(vec![0; locals.len()]);
    (search.best, locals.len())
}

/// The most numberings of a component's local binders that `shape` tries.
const SHAPE_SEARCH_LIMIT: usize = 64;

/// The numberings of a component's local binders that `shape` tries.
struct ShapeSearch<'a> {
    /// The component's constraints and unions, with its local binders numbered from 0.
    constraints: &'a [Item],
    /// The constraints that each local binder is in, by index, and its positions in each: 0 for
    /// the binder that the constraint bounds, and 1 onward for its environment.
    uses: &'a [Vec<(usize, Vec<usize>)>],
    /// The least sorted constraints and unions found so far.
    best: Vec<Item>,
    /// The numberings tried so far.
    numberings: usize,
}

impl ShapeSearch<'_> {
    /// Tries each numbering that the colours of the local binders allow, after refining them.
    fn search(&mut self, colours: Vec<usize>) {
        work(self.constraints.len());
        let colours = self.refine(colours);

        // The least colour that more than one binder has, if any (the colours are numbered from 0)
        let mut counts = vec![0; colours.len()];

        for &colour in &colours {
            counts[colour] += 1;
        }

        let Some(tied) = counts.iter().position(|&count| count > 1) else {
            let mut constraints: Vec<Item> = self
                .constraints
                .iter()
                .map(|constraint| constraint.map_locals(|local| colours[local]))
                .collect();
            constraints.sort();

            if self.numberings == 0 || constraints < self.best {
                self.best = constraints;
            }

            self.numberings += 1;
            return;
        };

        // Each binder of that colour is tried first, before the others of the same colour
        for local in (0..colours.len()).filter(|&local| colours[local] == tied) {
            if self.numberings >= SHAPE_SEARCH_LIMIT {
                return;
            }

            let individualized = colours
                .iter()
                .enumerate()
                .map(|(other, &colour)| 2 * colour + usize::from(colour == tied && other != local))
                .collect();
            self.search(individualized);
        }
    }

    /// Tells the local binders apart by the constraints they're in (with each binder replaced by
    /// its colour), and their positions in them, until that stops telling more of them apart.
    /// Colours are numbered from 0 in the order of what tells them apart, which doesn't depend on
    /// how the binders are numbered, and a binder that had a lesser colour than another keeps one.
    fn refine(&self, mut colours: Vec<usize>) -> Vec<usize> {
        let mut count = {
            let mut distinct = colours.clone();
            distinct.sort_unstable();
            distinct.dedup();
            distinct.len()
        };

        loop {
            work(self.constraints.len());

            // Each constraint is replaced by its rank among the recoloured constraints
            let recoloured: Vec<Item> = self
                .constraints
                .iter()
                .map(|constraint| constraint.map_locals(|local| colours[local]))
                .collect();
            let mut sorted: Vec<&Item> = recoloured.iter().collect();
            sorted.sort();
            sorted.dedup();
            let ranks: Vec<usize> = recoloured
                .iter()
                .map(|constraint| sorted.binary_search(&constraint).unwrap())
                .collect();

            let signatures: Vec<Signature<'_>> = self
                .uses
                .iter()
                .enumerate()
                .map(|(local, uses)| {
                    let mut uses: Vec<(usize, &[usize])> = uses
                        .iter()
                        .map(|(index, positions)| (ranks[*index], positions.as_slice()))
                        .collect();
                    uses.sort_unstable();
                    (colours[local], uses)
                })
                .collect();

            let mut distinct: Vec<&Signature<'_>> = signatures.iter().collect();
            distinct.sort();
            distinct.dedup();

            colours = signatures
                .iter()
                .map(|signature| distinct.binary_search(&signature).unwrap())
                .collect();

            if distinct.len() == count {
                return colours;
            }

            count = distinct.len();
        }
    }
}

/// What tells a local binder apart in `ShapeSearch::refine`: its colour, and the ranks of the
/// recoloured constraints it's in along with its positions in them.
type Signature<'a> = (usize, Vec<(usize, &'a [usize])>);

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
            branch: None,
            unmatched: false,
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

        // `x` is bounded, so it's an instance, and its component keeps up to three copies
        let mut summary = Summary::default();

        for _ in 0..4 {
            summary.apply(&child, &[Var::Key(0)]);
        }

        summary.canonicalize();
        assert_eq!(summary.locals, 6);
        assert_eq!(summary.constraints().len(), 9);

        for constraint in summary.constraints() {
            assert!(
                Item::Constraint(constraint.clone())
                    .locals()
                    .iter()
                    .all(|&local| local < 6)
            );
        }

        // Without a bound on a local binder, a component keeps two
        let mut rigid = Summary::default();
        let (x, y) = (rigid.local(), rigid.local());
        rigid.add(bound(Var::Key(0), false, a, vec![(a, x)]));
        rigid.add(bound(Var::Key(0), true, b, vec![(b, y), (a, x)]));
        let mut summary = Summary::default();

        for _ in 0..4 {
            summary.apply(&rigid, &[Var::Key(0)]);
        }

        summary.canonicalize();
        assert_eq!(summary.locals, 4);
        assert_eq!(summary.constraints().len(), 4);
    }

    /// Local binders in a component that are only told apart by their positions in the
    /// constraints they share are numbered the same way whatever order the constraints are in, so
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

    /// A canonical summary with five local binders, where each one `l` has the upper bounds `ty`
    /// with `a` bound to `first[l]`, and with `b` bound to `second[l]`, numbered by `numbering`.
    fn permutations(
        [ty, a, b]: [TypeId; 3],
        first: [usize; 5],
        second: [usize; 5],
        numbering: [usize; 5],
    ) -> Summary {
        let mut summary = Summary::default();
        let locals: Vec<Var> = (0..5).map(|_| summary.local()).collect();
        let local = |index: usize| locals[numbering[index]];

        for index in 0..5 {
            summary.add(bound(
                local(index),
                true,
                ty,
                vec![(a, local(first[index]))],
            ));
            summary.add(bound(
                local(index),
                true,
                ty,
                vec![(b, local(second[index]))],
            ));
        }

        summary.canonicalize();
        summary
    }

    /// Local binders that refinement can't tell apart, but that aren't alike (since a pair of them
    /// that `a` swaps can't be mapped to three that it rotates), are numbered the same way however
    /// they were numbered before, which needs trying more than one order of them.
    #[test]
    fn test_canonicalize_numbers_locals_that_refinement_cant_tell_apart() {
        let first = [1, 0, 3, 4, 2];
        let second = [2, 3, 1, 4, 0];
        let identity = [0, 1, 2, 3, 4];

        let mut types = Types::new();
        let generics = [types.generic(), types.generic(), types.generic()];
        let expected = permutations(generics, first, second, identity);

        for numbering in [
            [1, 2, 3, 4, 0],
            [4, 3, 2, 1, 0],
            [2, 0, 4, 1, 3],
            [3, 4, 0, 2, 1],
        ] {
            let renumbered = permutations(generics, first, second, numbering);
            assert_eq!(renumbered, expected, "{numbering:?}");
        }
    }

    /// A chain of local binders is only told apart from one end by refining again and again, which
    /// would take more numberings than `SHAPE_SEARCH_LIMIT` to try otherwise.
    #[test]
    fn test_canonicalize_refines_until_stable() {
        let mut types = Types::new();
        let [ty, a] = [types.generic(), types.generic()];

        let chain = |numbering: &dyn Fn(usize) -> usize| {
            let mut summary = Summary::default();

            for _ in 0..20 {
                summary.local();
            }

            for index in 0..19 {
                let next = Var::Local(numbering(index + 1));
                summary.add(bound(
                    Var::Local(numbering(index)),
                    true,
                    ty,
                    vec![(a, next)],
                ));
            }

            summary.canonicalize();
            summary
        };

        let expected = chain(&|index| index);
        assert_eq!(chain(&|index| 19 - index), expected);
        assert_eq!(chain(&|index| (index * 7) % 20), expected);
        assert_eq!(chain(&|index| (index + 13) % 20), expected);
    }

    /// A component with more orders of alike local binders than `SHAPE_SEARCH_LIMIT` (each of the
    /// five bounds each of the others, so there are 120) still keeps all of its constraints.
    #[test]
    fn test_canonicalize_stops_searching() {
        let mut types = Types::new();
        let [ty, a] = [types.generic(), types.generic()];
        let mut summary = Summary::default();
        let locals: Vec<Var> = (0..5).map(|_| summary.local()).collect();

        for &lower in &locals {
            for &other in &locals {
                if lower != other {
                    summary.add(bound(lower, false, ty, vec![(a, other)]));
                }
            }
        }

        summary.canonicalize();
        assert_eq!(summary.locals, 5);
        assert_eq!(summary.constraints().len(), 20);
    }

    /// A constraint that's there twice in a component doesn't change how it's numbered, so it's
    /// the same as without the duplicate, and canonicalizing it again doesn't change it.
    #[test]
    fn test_canonicalize_ignores_duplicates() {
        let mut types = Types::new();
        let ty = types.generic();
        let generics = [types.generic(), types.generic()];

        let summary = |duplicate: bool| {
            let mut summary = Summary::default();
            let locals: Vec<Var> = (0..7).map(|_| summary.local()).collect();
            let env = |vars: &[(usize, Var)]| -> Env {
                vars.iter()
                    .map(|&(generic, var)| (generics[generic], var))
                    .collect()
            };

            let mut constraints = vec![
                bound(
                    locals[4],
                    true,
                    ty,
                    env(&[(0, locals[5]), (1, Var::Key(0))]),
                ),
                bound(locals[4], false, ty, vec![]),
                bound(
                    locals[6],
                    true,
                    ty,
                    env(&[(0, Var::Key(0)), (1, locals[0])]),
                ),
                bound(locals[5], false, ty, env(&[(0, locals[2])])),
                bound(locals[1], false, ty, env(&[(1, locals[4])])),
                bound(locals[3], false, ty, env(&[(0, locals[3]), (1, locals[4])])),
                bound(locals[2], false, ty, vec![]),
                bound(locals[0], true, ty, vec![]),
            ];

            if duplicate {
                constraints.insert(2, bound(locals[4], false, ty, vec![]));
            }

            for constraint in constraints {
                summary.add(constraint);
            }

            summary.canonicalize();
            summary
        };

        let mut with_duplicate = summary(true);
        assert_eq!(with_duplicate, summary(false));

        let canonical = with_duplicate.clone();
        with_duplicate.canonicalize();
        assert_eq!(with_duplicate, canonical);
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

    fn union(
        choice: Var,
        parent: Option<Branch>,
        members: &[Comparison],
        bounded: &[bool],
    ) -> ChoiceRecord {
        ChoiceRecord {
            choice,
            parent,
            members: members.iter().copied().map(Some).collect(),
            fallback: None,
            bounded: bounded.to_vec(),
        }
    }

    /// Members of a union that gave the same result and have the same bounds under them are
    /// alternatives that are the same to pick, so all but one are left out.
    #[test]
    fn test_canonicalize_merges_alike_members() {
        let mut types = Types::new();
        let (a, b) = (types.generic(), types.generic());
        let mut summary = Summary::default();
        let choice = summary.local();
        let assign = [Comparison::Assign, Comparison::Assign];
        summary.add_choice(union(choice, None, &assign, &[true, true, false]));

        for member in 0..2 {
            summary.add(Constraint {
                branch: Some((choice, member)),
                ..bound(Var::Key(0), true, a, vec![])
            });
        }

        // A different bound under a third member keeps the union
        let mut other = summary.clone();
        let mut members = assign.to_vec();
        members.push(Comparison::Assign);
        other.choices[0] = union(choice, None, &members, &[true, true, true, false]);
        other.add(Constraint {
            branch: Some((choice, 2)),
            ..bound(Var::Key(0), true, b, vec![])
        });

        other.canonicalize();
        assert_eq!(other.choices().len(), 1);
        assert_eq!(other.constraints().len(), 2);
        assert!(!other.is_approximate());

        summary.canonicalize();
        assert_eq!(summary.choices().len(), 1);
        assert_eq!(summary.constraints().len(), 1);
        assert!(!summary.is_approximate());

        // The merged member is left out of the union, so nothing that fails under the one kept
        // is hidden by it, a member with nothing under it to charge
        assert_eq!(
            summary.choices()[0].members,
            [Some(Comparison::Assign), None]
        );
    }

    /// A union with two copies of it under the same member is left out, like a third copy of a
    /// component.
    #[test]
    fn test_canonicalize_leaves_out_a_third_copy_of_a_union() {
        let mut types = Types::new();
        let (a, b, c) = (types.generic(), types.generic(), types.generic());
        let assign = [Comparison::Assign, Comparison::Assign];
        let mut summary = Summary::default();
        let outer = summary.local();
        summary.add_choice(union(outer, None, &assign, &[true, true, false]));
        summary.add(Constraint {
            branch: Some((outer, 1)),
            ..bound(Var::Key(0), true, c, vec![])
        });

        for _ in 0..3 {
            let inner = summary.local();
            summary.add_choice(union(
                inner,
                Some((outer, 0)),
                &assign,
                &[true, true, false],
            ));
            summary.add(Constraint {
                branch: Some((inner, 0)),
                ..bound(Var::Key(0), false, a, vec![])
            });
            summary.add(Constraint {
                branch: Some((inner, 1)),
                ..bound(Var::Key(0), true, b, vec![])
            });
        }

        summary.canonicalize();
        assert_eq!(summary.choices().len(), 3);
        assert_eq!(summary.constraints().len(), 5);
        assert!(!summary.is_approximate());
    }

    /// A union held by a member that gave `Assign` without collecting bounds is left out, with
    /// what's under its other members, which decides nothing. One held only by a member that gave
    /// `Cast` is kept.
    #[test]
    fn test_canonicalize_leaves_out_held_unions() {
        let mut types = Types::new();
        let a = types.generic();

        for (second, kept) in [(Comparison::Assign, 0), (Comparison::Cast, 1)] {
            let mut summary = Summary::default();
            let choice = summary.local();
            summary.add_choice(union(
                choice,
                None,
                &[Comparison::Assign, second],
                &[true, false, false],
            ));
            summary.add(Constraint {
                branch: Some((choice, 0)),
                ..bound(Var::Key(0), true, a, vec![])
            });

            summary.canonicalize();
            assert_eq!(summary.choices().len(), kept, "{second:?}");
            assert_eq!(summary.constraints().len(), kept, "{second:?}");
        }
    }

    /// A union held by a member that gave `Assign` and collected bounds, none of which are in the
    /// summary, is left out too, since nothing can be charged to that member where the summary is
    /// applied. One whose member with nothing under it gave `Cast`, or was left out as alike
    /// another (so it has no result), is kept.
    #[test]
    fn test_canonicalize_leaves_out_unions_held_by_a_member_with_nothing_under_it() {
        let mut types = Types::new();
        let a = types.generic();

        for (first, kept) in [
            (Some(Comparison::Assign), 0),
            (Some(Comparison::Cast), 1),
            (None, 1),
        ] {
            let mut summary = Summary::default();
            let choice = summary.local();
            summary.add_choice(ChoiceRecord {
                members: vec![first, Some(Comparison::Assign)],
                ..union(choice, None, &[], &[true, true, false])
            });
            summary.add(Constraint {
                branch: Some((choice, 1)),
                ..bound(Var::Key(0), true, a, vec![])
            });

            summary.canonicalize();
            assert_eq!(summary.choices().len(), kept, "{first:?}");
            assert_eq!(summary.constraints().len(), kept, "{first:?}");
        }
    }

    /// Adding to a canonical summary drops the components it cached, so joining it afterwards
    /// keeps what was added.
    #[test]
    fn test_join_after_adding_keeps_it() {
        let mut types = Types::new();
        let (a, b) = (types.generic(), types.generic());
        let mut summary = Summary::default();
        summary.add(bound(Var::Key(0), true, a, vec![]));
        summary.canonicalize();
        summary.add(bound(Var::Key(0), true, b, vec![]));

        let joined = summary.join(&Summary::default());
        assert_eq!(joined.constraints().len(), 2);
    }

    /// Unions nested more deeply than `MAX_UNION_DEPTH` are left out, with what's under them put
    /// under the member they're nested in, which then needs all of it to hold, and the summary is
    /// approximate (see `Summary::is_approximate`), so it's never reused from the memo. Each union
    /// here has a bound under both of its members, the second of which the next union is nested
    /// in, so the fourth, three deep, is past the cap.
    #[test]
    fn test_canonicalize_caps_union_depth() {
        let mut types = Types::new();
        let generics: Vec<TypeId> = (0..8).map(|_| types.generic()).collect();
        let mut summary = Summary::default();
        let unions: Vec<Var> = (0..4).map(|_| summary.local()).collect();

        for (depth, &union) in unions.iter().enumerate() {
            summary.add_choice(ChoiceRecord {
                choice: union,
                parent: depth.checked_sub(1).map(|outer| (unions[outer], 1)),
                members: vec![Some(Comparison::Assign), Some(Comparison::Assign)],
                fallback: None,
                bounded: vec![true, true, false],
            });

            for member in 0..2 {
                summary.add(Constraint {
                    branch: Some((union, member)),
                    ..bound(
                        Var::Key(0),
                        member == 1,
                        generics[2 * depth + member],
                        vec![],
                    )
                });
            }
        }

        summary.canonicalize();
        assert!(summary.is_approximate());
        assert_eq!(summary.choices().len(), 3);

        // The fourth union's bounds are both under the third union's second member now, along
        // with its own
        let parent = |union: Var| {
            summary
                .choices()
                .iter()
                .find(|choice| choice.choice == union)
                .and_then(|choice| choice.parent)
        };
        let deepest = summary
            .choices()
            .iter()
            .map(|choice| choice.choice)
            .find(|&union| parent(union).and_then(|(outer, _)| parent(outer)).is_some())
            .unwrap();
        let under_deepest = summary
            .constraints()
            .iter()
            .filter(|constraint| constraint.branch == Some((deepest, 1)))
            .count();
        assert_eq!(under_deepest, 3);

        // Three deep, nothing is left out
        let mut shallow = Summary::default();
        let unions: Vec<Var> = (0..3).map(|_| shallow.local()).collect();

        for (depth, &union) in unions.iter().enumerate() {
            shallow.add_choice(ChoiceRecord {
                choice: union,
                parent: depth.checked_sub(1).map(|outer| (unions[outer], 1)),
                members: vec![Some(Comparison::Assign), Some(Comparison::Assign)],
                fallback: None,
                bounded: vec![true, true, false],
            });

            for member in 0..2 {
                shallow.add(Constraint {
                    branch: Some((union, member)),
                    ..bound(
                        Var::Key(0),
                        member == 1,
                        generics[2 * depth + member],
                        vec![],
                    )
                });
            }
        }

        shallow.canonicalize();
        assert!(!shallow.is_approximate());
        assert_eq!(shallow.choices().len(), 3);
    }

    /// Joining a summary with itself gives it back, even when it has unions, two copies of which
    /// would have more bounds to check against each other than one.
    #[test]
    fn test_join_is_idempotent() {
        let mut types = Types::new();
        let [a, b] = [types.generic(), types.generic()];
        let mut summary = Summary::default();
        let choice = summary.local();
        summary.add_choice(ChoiceRecord {
            choice,
            parent: None,
            members: vec![Some(Comparison::Assign), Some(Comparison::Assign)],
            fallback: None,
            bounded: vec![true, true, false],
        });
        summary.add(Constraint {
            branch: Some((choice, 0)),
            ..bound(Var::Key(0), true, a, vec![])
        });
        summary.add(Constraint {
            branch: Some((choice, 1)),
            ..bound(Var::Key(0), false, b, vec![])
        });
        summary.canonicalize();

        assert_eq!(summary.join(&summary), summary);
        assert_eq!(summary.join(&Summary::default()), summary);

        // Applying it twice is two unions
        let mut twice = Summary::default();
        twice.apply(&summary, &[Var::Key(0)]);
        twice.apply(&summary, &[Var::Key(0)]);
        twice.canonicalize();
        assert_eq!(twice.choices().len(), 2);
        assert_eq!(twice.join(&summary), twice);
    }
}
