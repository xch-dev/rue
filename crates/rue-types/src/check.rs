use std::cmp::max;
use std::fmt;
use std::hash::{BuildHasher, Hash, Hasher};
use std::ops::ControlFlow;

use rustc_hash::FxBuildHasher;
use std::sync::Arc;

use id_arena::Arena;
use indexmap::IndexSet;
use log::{Level, log_enabled, trace};
use thiserror::Error;

use crate::{
    AtomRestriction, Atoms, BuiltinTypes, Comparison, ComparisonContext, FxHashMap, FxHashSet,
    FxIndexSet, Memo, Pair, Type, TypeId, Walk, compare_impl, for_each_leaf, grow_stack,
    reaches_non_contractive_cycle, step, stringify_substituted, substitute, walk_leaves,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[must_use]
pub enum Check {
    None,
    Impossible,
    /// Whether the value is an atom. If `can_be_truthy` is false, the only atom the value can be
    /// is nil, so the check is generated as whether the value is nil rather than with `l`. An
    /// atom other than nil is then treated as a pair, which is how Chialisp checks whether a list
    /// has ended.
    IsAtom {
        can_be_truthy: bool,
    },
    /// Whether the value is a pair, generated the same way as [`Check::IsAtom`].
    IsPair {
        can_be_truthy: bool,
    },
    Pair(Box<Check>, Box<Check>),
    Atom(AtomRestriction),
    And(Vec<Check>),
    Or(Vec<Check>),
    /// A check that the same part of a type needs wherever it's used, such as `T` in `(T, T)`,
    /// which is shared rather than copied, so that the check stays linear in the size of the
    /// type. [`Check::plan`] decides whether it's generated as a helper or copied in.
    Shared(SharedCheck),
    /// A call to the helper at this index in a [`CheckPlan`], with the value being checked.
    Helper(usize),
}

/// A part of a check that may be used in more than one place (see [`Check::Shared`]). It's
/// simplified before it's shared, and equal to another shared check with the same parts.
#[derive(Clone)]
pub struct SharedCheck {
    check: Arc<Check>,
    /// The hash of the check, so that hashing a check doesn't go through the shared parts it
    /// uses every time they're used.
    hash: u64,
}

impl SharedCheck {
    fn new(check: Check) -> Self {
        let hash = FxBuildHasher.hash_one(&check);

        Self {
            check: Arc::new(check),
            hash,
        }
    }

    pub fn check(&self) -> &Check {
        &self.check
    }

    fn key(&self) -> *const Check {
        Arc::as_ptr(&self.check)
    }
}

/// Prints the shared check where it's used, like any other part. A check that uses the same part
/// at every level prints it exponentially many times, so print its [`CheckPlan`] instead.
impl fmt::Debug for SharedCheck {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.check.fmt(f)
    }
}

impl PartialEq for SharedCheck {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.check, &other.check)
            || (self.hash == other.hash && self.check == other.check)
    }
}

impl Eq for SharedCheck {}

impl Hash for SharedCheck {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.hash.hash(state);
    }
}

/// The smallest check, in nodes (see [`Check::size`]), that's shared rather than copied in where
/// it's used. Calling a helper takes about as many bytes as a check of a few nodes, and a check
/// with helpers is run as a program of its own, so smaller checks are copied in. Sharing checks
/// of 5 nodes saves a few bytes for some types, such as `[(Bytes32, Int), (Bytes32, Int)]`, but
/// costs more for others whose checks are short comparisons, such as the struct tags in
/// `tests/guards/pair_parts.rue`. No program in the tests gets larger with this limit.
pub const SHARED_CHECK_MIN_SIZE: usize = 8;

/// A check that's ready to be generated, with each part that's used in more than one place as a
/// helper, which the check and the other helpers call with [`Check::Helper`]. If there are no
/// helpers, the check has no [`Check::Shared`] or [`Check::Helper`] in it, and is generated as is.
/// Helpers only call helpers after them, and the check calls at least one helper if there are any.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CheckPlan {
    pub check: Check,
    pub helpers: Vec<Check>,
    /// How many times each helper is called, by the check and the other helpers.
    pub calls: Vec<usize>,
}

impl Check {
    /// The number of nodes in the check, which is proportional to the size of the code it
    /// generates. A shared part is only counted once, since it's generated once if it's large
    /// (see [`Check::plan`]).
    pub fn size(&self) -> usize {
        self.size_impl(&mut FxHashSet::default())
    }

    fn size_impl(&self, seen: &mut FxHashSet<*const Check>) -> usize {
        let inner = match self {
            Check::Shared(shared) if seen.insert(shared.key()) => shared.check().size_impl(seen),
            check => check
                .children()
                .map(|check| check.size_impl(seen))
                .sum::<usize>(),
        };

        1 + inner
    }

    /// The checks directly inside of this one, in order. A shared check isn't included, since
    /// whether to go into it depends on what it's for.
    pub fn children(&self) -> impl Iterator<Item = &Check> {
        let (pair, list): ([Option<&Check>; 2], &[Check]) = match self {
            Check::Pair(first, rest) => ([Some(first), Some(rest)], &[]),
            Check::And(checks) | Check::Or(checks) => ([None, None], checks),
            Check::None
            | Check::Impossible
            | Check::IsAtom { .. }
            | Check::IsPair { .. }
            | Check::Atom(_)
            | Check::Shared(_)
            | Check::Helper(_) => ([None, None], &[]),
        };

        pair.into_iter().flatten().chain(list)
    }

    /// Decides how the check is generated. A shared part that ends up in more than one place in
    /// the generated code becomes a helper, and the rest are copied in where they're used.
    pub fn plan(&self) -> CheckPlan {
        // The shared parts, each once, with the parts they use before them
        let mut order = Vec::new();
        let mut index = FxHashMap::default();
        collect_shared(self, &mut order, &mut index);

        // How many times each shared part is used by the check and by each other shared part,
        // not counting the uses inside shared parts that it uses
        let mut uses = vec![0usize; order.len()];
        count_shared(self, &index, &mut uses);

        let mut inner_uses = Vec::with_capacity(order.len());
        for shared in &order {
            let mut counts = vec![0usize; order.len()];
            count_shared(shared.check(), &index, &mut counts);
            inner_uses.push(counts);
        }

        // How many times each part ends up in the generated code, going from the parts that use
        // others to the parts they use, so that each part's count is known before the parts it
        // uses. A part that's used more than once is a helper, generated once, and a part that's
        // used once is copied in once, so the parts it uses are used as many times as it uses
        // them.
        let mut helper = vec![None; order.len()];
        let mut calls = Vec::new();

        for i in (0..order.len()).rev() {
            let copies = if uses[i] > 1 {
                helper[i] = Some(calls.len());
                calls.push(uses[i]);
                1
            } else {
                uses[i]
            };

            for (j, &count) in inner_uses[i].iter().enumerate() {
                uses[j] += count * copies;
            }
        }

        let replace = |check: &Check| replace_shared(check, &index, &helper);

        let mut bodies = vec![None; calls.len()];

        for (i, shared) in order.iter().enumerate() {
            if let Some(helper) = helper[i] {
                bodies[helper] = Some(replace(shared.check()));
            }
        }

        CheckPlan {
            check: replace(self),
            helpers: bodies.into_iter().map(Option::unwrap).collect(),
            calls,
        }
    }

    pub fn simplify(self) -> Check {
        grow_stack(|| self.simplify_inner())
    }

    fn simplify_inner(self) -> Check {
        step();

        match self {
            Check::None => Check::None,
            Check::Impossible => Check::Impossible,
            Check::Pair(first, rest) => {
                let first = first.simplify();
                let rest = rest.simplify();

                // Pair checks are only ever evaluated against values that are known to be pairs
                if first == Check::Impossible || rest == Check::Impossible {
                    Check::Impossible
                } else if first == Check::None && rest == Check::None {
                    Check::None
                } else {
                    Check::Pair(Box::new(first), Box::new(rest))
                }
            }
            Check::IsAtom { can_be_truthy } => Check::IsAtom { can_be_truthy },
            Check::IsPair { can_be_truthy } => Check::IsPair { can_be_truthy },
            Check::Atom(restriction) => Check::Atom(restriction),
            // Shared checks are simplified before they're shared
            check @ (Check::Shared(_) | Check::Helper(_)) => check,
            Check::And(checks) => {
                let mut flattened = Vec::new();

                for check in checks {
                    match check.simplify() {
                        Check::None => {}
                        Check::Impossible => {
                            return Check::Impossible;
                        }
                        Check::And(inner) => {
                            flattened.extend(inner);
                        }
                        check => {
                            flattened.push(check);
                        }
                    }
                }

                let mut listp = None;
                let mut length = None;
                let mut value = None;
                let mut result = Vec::new();

                for check in flattened {
                    match check {
                        Check::None | Check::Impossible | Check::And(_) => unreachable!(),
                        Check::IsAtom { can_be_truthy } => {
                            if matches!(listp, Some(Check::IsPair { .. })) {
                                return Check::Impossible;
                            }
                            listp = Some(Check::IsAtom { can_be_truthy });
                        }
                        Check::IsPair { can_be_truthy } => {
                            if matches!(listp, Some(Check::IsAtom { .. })) {
                                return Check::Impossible;
                            }
                            listp = Some(Check::IsPair { can_be_truthy });
                        }
                        Check::Atom(AtomRestriction::Length(check)) => {
                            if length.is_some_and(|length| length != check) {
                                return Check::Impossible;
                            }
                            length = Some(check);
                        }
                        Check::Atom(AtomRestriction::Value(check)) => {
                            if value.is_some_and(|value| value != check) {
                                return Check::Impossible;
                            }
                            value = Some(check);
                        }
                        check @ (Check::Or(_)
                        | Check::Pair(..)
                        | Check::Shared(_)
                        | Check::Helper(_)) => {
                            result.push(check);
                        }
                    }
                }

                match (length, value) {
                    (Some(length), Some(value)) => {
                        if length != value.len() {
                            return Check::Impossible;
                        }
                        result.insert(0, Check::Atom(AtomRestriction::Value(value)));
                    }
                    (None, Some(value)) => {
                        result.insert(0, Check::Atom(AtomRestriction::Value(value)));
                    }
                    (Some(length), None) => {
                        result.insert(0, Check::Atom(AtomRestriction::Length(length)));
                    }
                    (None, None) => {}
                }

                if let Some(listp) = listp {
                    result.insert(0, listp);
                }

                if result.is_empty() {
                    Check::None
                } else if result.len() == 1 {
                    result[0].clone()
                } else {
                    Check::And(result)
                }
            }
            Check::Or(checks) => {
                let mut result = Vec::new();

                for check in checks {
                    match check.simplify() {
                        Check::None => {
                            return Check::None;
                        }
                        Check::Impossible => {}
                        Check::Or(inner) => {
                            for check in inner {
                                if !result.contains(&check) {
                                    result.push(check);
                                }
                            }
                        }
                        check => {
                            if !result.contains(&check) {
                                result.push(check);
                            }
                        }
                    }
                }

                if result.is_empty() {
                    Check::Impossible
                } else if result.len() == 1 {
                    result.remove(0)
                } else {
                    Check::Or(result)
                }
            }
        }
    }
}

/// Adds the shared parts that the check uses to the order, after the shared parts that they use.
fn collect_shared(
    check: &Check,
    order: &mut Vec<SharedCheck>,
    index: &mut FxHashMap<*const Check, usize>,
) {
    if let Check::Shared(shared) = check {
        if !index.contains_key(&shared.key()) {
            collect_shared(shared.check(), order, index);
            index.insert(shared.key(), order.len());
            order.push(shared.clone());
        }
        return;
    }

    for check in check.children() {
        collect_shared(check, order, index);
    }
}

/// Counts the uses of each shared part in the check, without looking inside them.
fn count_shared(check: &Check, index: &FxHashMap<*const Check, usize>, uses: &mut [usize]) {
    if let Check::Shared(shared) = check {
        uses[index[&shared.key()]] += 1;
        return;
    }

    for check in check.children() {
        count_shared(check, index, uses);
    }
}

/// Replaces each shared part with a call to its helper, or copies it in if it doesn't have one.
fn replace_shared(
    check: &Check,
    index: &FxHashMap<*const Check, usize>,
    helper: &[Option<usize>],
) -> Check {
    match check {
        Check::None
        | Check::Impossible
        | Check::IsAtom { .. }
        | Check::IsPair { .. }
        | Check::Atom(_)
        | Check::Helper(_) => check.clone(),
        Check::Pair(first, rest) => Check::Pair(
            Box::new(replace_shared(first, index, helper)),
            Box::new(replace_shared(rest, index, helper)),
        ),
        Check::And(checks) => Check::And(
            checks
                .iter()
                .map(|check| replace_shared(check, index, helper))
                .collect(),
        ),
        Check::Or(checks) => Check::Or(
            checks
                .iter()
                .map(|check| replace_shared(check, index, helper))
                .collect(),
        ),
        Check::Shared(shared) => match helper[index[&shared.key()]] {
            Some(helper) => Check::Helper(helper),
            None => replace_shared(shared.check(), index, helper),
        },
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Error)]
pub enum CheckError {
    #[error("Cannot check if value is of deeply nested type at runtime")]
    DepthExceeded,

    #[error("Cannot check if value is of recursive type at runtime")]
    RecursiveType,

    #[error(
        "Cannot check if value is a function at runtime, since how a function is represented differs between debug and release builds"
    )]
    FunctionType,

    #[error("Cannot check if value is of generic type at runtime")]
    GenericType,

    #[error("Comparing the types is too complex")]
    TooComplex,

    #[error("Cannot check if value is of this type at runtime, since the check would be too large")]
    TooLarge,
}

/// The largest check that can be generated, in nodes (see [`Check::size`]). Each node generates
/// around a dozen bytes of CLVM, and the checks for typical types are under a hundred nodes. The
/// parts that are repeated are shared, so this is a backstop for checks that are large without
/// repeating anything, such as of a union of thousands of values.
pub const CHECK_SIZE_LIMIT: usize = 10_000;

pub fn check(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    lhs: TypeId,
    rhs: TypeId,
) -> Result<Check, CheckError> {
    let lhs = substitute(arena, lhs);
    let rhs = substitute(arena, rhs);

    // These should be rejected before reaching this point, but they would recurse forever
    if reaches_non_contractive_cycle(arena, lhs) || reaches_non_contractive_cycle(arena, rhs) {
        return Err(CheckError::RecursiveType);
    }

    let names = log_enabled!(Level::Trace).then(|| {
        (
            stringify_substituted(arena, lhs),
            stringify_substituted(arena, rhs),
        )
    });

    if let Some((lhs_name, rhs_name)) = &names {
        trace!("Checking {lhs_name} to {rhs_name}");
    }

    let mut ctx = CheckContext::default();

    // The check itself is only used once
    let mut result = check_impl(arena, builtins, &mut ctx, 0, lhs, rhs).map(|check| match check {
        Check::Shared(shared) => shared.check().clone(),
        check => check,
    });

    // A comparison that gave up was `Invalid`, so the check can't be relied on
    if ctx.comparison.take_too_complex() {
        result = Err(CheckError::TooComplex);
    }

    if let Some((lhs_name, rhs_name)) = &names {
        // The plan has each shared part once, whereas the check would print it for every use
        let plan = result.as_ref().map(Check::plan);
        trace!("Check from {lhs_name} to {rhs_name} yielded {plan:?}");
    }

    result
}

#[derive(Debug, Default)]
struct CheckContext {
    /// Shared by every comparison, since they all start from an empty stack.
    comparison: ComparisonContext<'static>,
    /// The checks that are shared, so that the same check is shared wherever it's used, even
    /// when it's reached from different types, such as each reference to `T` in `(T, T)`.
    shared: FxHashSet<SharedCheck>,
    /// Completed checks, keyed by the arguments to `check_each`. The same checks are reached
    /// through every reference to a type, which is exponential in the depth of the type.
    memo: FxHashMap<(Vec<TypeId>, TypeId, usize), Result<Check, CheckError>>,
    /// The arguments to each `check_each` call on the current path, with the types sorted. The
    /// check only depends on these, so reaching the same ones again would repeat forever.
    path: FxIndexSet<(Vec<TypeId>, TypeId)>,
}

fn check_impl(
    arena: &Arena<Type>,
    builtins: &BuiltinTypes,
    ctx: &mut CheckContext,
    depth: usize,
    lhs: TypeId,
    rhs: TypeId,
) -> Result<Check, CheckError> {
    let variants = variants_of(arena, builtins, lhs);
    check_each(arena, builtins, ctx, depth, &variants, rhs)
}

fn check_each(
    arena: &Arena<Type>,
    builtins: &BuiltinTypes,
    ctx: &mut CheckContext,
    depth: usize,
    lhs: &[TypeId],
    rhs: TypeId,
) -> Result<Check, CheckError> {
    // Repeating a type doesn't change the result, and the nested checks would otherwise repeat
    // each type more times at every level
    let lhs: Vec<TypeId> = lhs
        .iter()
        .copied()
        .collect::<IndexSet<_>>()
        .into_iter()
        .collect();
    let key = (lhs, rhs, depth);

    if let Some(result) = ctx.memo.get(&key) {
        return result.clone();
    }

    let mut types = key.0.clone();
    types.sort_unstable();

    // No finite check exists, such as when checking `Any` against `List<Int>`, so report that
    // rather than continuing until the depth limit is reached
    if !ctx.path.insert((types, rhs)) {
        return Err(CheckError::RecursiveType);
    }

    let result = check_each_impl(arena, builtins, ctx, depth + 1, &key.0, rhs);

    // The memoized check is shared by every use of it, so a type that nests the same type twice
    // at each level, such as `(T, T)`, doesn't double the size of the check each time. The size
    // only counts each shared part once, and limiting every memoized check, not just the final
    // one, bounds the work of building it.
    let result = result.and_then(|check| {
        let size = check.size();

        if size > CHECK_SIZE_LIMIT {
            Err(CheckError::TooLarge)
        } else {
            Ok(share(&mut ctx.shared, check, size))
        }
    });

    ctx.path.pop();
    ctx.memo.insert(key, result.clone());
    result
}

fn check_each_impl(
    arena: &Arena<Type>,
    builtins: &BuiltinTypes,
    ctx: &mut CheckContext,
    depth: usize,
    lhs: &[TypeId],
    rhs: TypeId,
) -> Result<Check, CheckError> {
    grow_stack(|| check_each_impl_inner(arena, builtins, ctx, depth, lhs, rhs))
}

fn check_each_impl_inner(
    arena: &Arena<Type>,
    builtins: &BuiltinTypes,
    ctx: &mut CheckContext,
    depth: usize,
    lhs: &[TypeId],
    rhs: TypeId,
) -> Result<Check, CheckError> {
    step();

    if depth > 25 {
        return Err(CheckError::DepthExceeded);
    }

    let mut result = Comparison::Assign;

    for &id in lhs {
        let comparison = compare_impl(arena, builtins, &mut ctx.comparison, id, rhs, None, None);

        // A function can be cast to a function type that it isn't assignable to, but nothing can
        // tell them apart at runtime, so it isn't accepted without a check like other casts are
        if comparison == Comparison::Cast && matches!(arena[id], Type::Function(_)) {
            return Err(CheckError::FunctionType);
        }

        result = max(result, comparison);
    }

    if result <= Comparison::Cast {
        return Ok(Check::None);
    }

    let target_atoms = atoms_of(arena, rhs, true)?;

    let mut overlap = IndexSet::new();
    let mut exceeds_overlap = false;
    let mut lhs_has_atom = false;
    let mut can_be_truthy = false;

    for &id in lhs {
        let atoms = atoms_of(arena, id, false)?;

        if let Some(atoms) = &atoms {
            lhs_has_atom = true;

            match atoms {
                Atoms::Unrestricted => {
                    can_be_truthy = true;
                }
                Atoms::Restricted(restrictions) => {
                    for restriction in restrictions {
                        match restriction {
                            AtomRestriction::Value(value) => {
                                if !value.is_empty() {
                                    can_be_truthy = true;
                                }
                            }
                            AtomRestriction::Length(length) => {
                                if *length > 0 {
                                    can_be_truthy = true;
                                }
                            }
                        }
                    }
                }
            }
        }

        match (atoms, &target_atoms) {
            (Some(_), None) => {}
            (Some(_), Some(Atoms::Unrestricted)) | (None, _) => {}
            (Some(Atoms::Unrestricted), Some(Atoms::Restricted(restrictions))) => {
                exceeds_overlap = true;

                for restriction in restrictions {
                    if let AtomRestriction::Value(value) = restriction
                        && overlap.contains(&AtomRestriction::Length(value.len()))
                    {
                        continue;
                    }
                    overlap.insert(restriction.clone());
                }
            }
            (
                Some(Atoms::Restricted(restrictions)),
                Some(Atoms::Restricted(target_restrictions)),
            ) => {
                for restriction in restrictions {
                    if target_restrictions.contains(&restriction) {
                        overlap.insert(restriction);
                        continue;
                    }

                    match restriction {
                        AtomRestriction::Value(value) => {
                            let length = AtomRestriction::Length(value.len());
                            if target_restrictions.contains(&length) {
                                overlap.insert(length);
                                continue;
                            }
                        }
                        AtomRestriction::Length(length) => {
                            // Some values of this length may still be in the target
                            for target_restriction in target_restrictions {
                                if let AtomRestriction::Value(value) = target_restriction
                                    && value.len() == length
                                {
                                    overlap.insert(target_restriction.clone());
                                }
                            }
                        }
                    }

                    exceeds_overlap = true;
                }
            }
        }
    }

    let atom_result = lhs_has_atom.then(|| {
        if target_atoms.is_none() {
            Check::Impossible
        } else if !exceeds_overlap {
            Check::None
        } else if overlap.is_empty() {
            Check::Impossible
        } else if overlap.len() == 1 {
            overlap.into_iter().next().map(Check::Atom).unwrap()
        } else {
            Check::Or(overlap.into_iter().map(Check::Atom).collect())
        }
    });

    let target_pairs = pairs_of(arena, builtins, rhs, true)?;

    let mut pairs = Vec::new();

    for &lhs in lhs {
        pairs.extend(pairs_of(arena, builtins, lhs, false)?);
    }

    let pair_result = if pairs.is_empty() {
        None
    } else {
        let firsts: Vec<TypeId> = pairs.iter().map(|pair| pair.first).collect();

        let mut checks = Vec::new();

        // Whether a pair's check was left out because it was too large. The error is only
        // reported once the other pairs are known, since it doesn't matter if another pair's
        // check accepts every pair.
        let mut too_large = false;

        // For each target pair, the first check is exact for every possible first value. The rest
        // check only runs if the first check passes, so it must be exact for the rest of every pair
        // whose first could pass the first check (not just those whose first is assignable to the
        // target's first). Each check in the resulting union is therefore exact for every pair.
        for target_pair in &target_pairs {
            let first = unless_too_large(check_each(
                arena,
                builtins,
                ctx,
                depth,
                &firsts,
                target_pair.first,
            ))?;

            if first == Some(Check::Impossible) {
                continue;
            }

            let rests: Vec<TypeId> = pairs
                .iter()
                .filter(|pair| {
                    may_overlap(
                        arena,
                        builtins,
                        pair.first,
                        target_pair.first,
                        &mut FxIndexSet::default(),
                        &mut Memo::default(),
                    )
                })
                .map(|pair| pair.rest)
                .collect();

            if rests.is_empty() {
                continue;
            }

            let rest = unless_too_large(check_each(
                arena,
                builtins,
                ctx,
                depth,
                &rests,
                target_pair.rest,
            ))?;

            // The rest is checked even if the first check is too large, since the pair can't
            // match if the rest can't
            match (first, rest) {
                (_, Some(Check::Impossible)) => {}
                (Some(first), Some(rest)) => {
                    checks.push(Check::Pair(Box::new(first), Box::new(rest)));
                }
                (None, _) | (_, None) => too_large = true,
            }
        }

        // If any target pair accepts every pair, this simplifies to `Check::None`
        match Check::Or(checks).simplify() {
            Check::None => Some(Check::None),
            _ if too_large => return Err(CheckError::TooLarge),
            check => Some(check),
        }
    };

    let check = match (atom_result, pair_result) {
        (None, None) => Check::Impossible,
        (Some(atom), None) => atom,
        (None, Some(pair)) => pair,
        (Some(atom), Some(Check::Impossible)) => {
            Check::And(vec![Check::IsAtom { can_be_truthy }, atom])
        }
        (Some(Check::Impossible), Some(pair)) => {
            Check::And(vec![Check::IsPair { can_be_truthy }, pair])
        }
        (Some(atom), Some(Check::None)) => Check::Or(vec![Check::IsPair { can_be_truthy }, atom]),
        (Some(Check::None), Some(pair)) => Check::Or(vec![Check::IsAtom { can_be_truthy }, pair]),
        (Some(atom), Some(pair)) => Check::Or(vec![
            Check::And(vec![Check::IsAtom { can_be_truthy }, atom]),
            Check::And(vec![Check::IsPair { can_be_truthy }, pair]),
        ]),
    };

    Ok(check.simplify())
}

/// Shares the check if it's large enough to be worth calling as a helper (see
/// [`SHARED_CHECK_MIN_SIZE`]), given its size, with the same shared check as any equal one.
fn share(shared: &mut FxHashSet<SharedCheck>, check: Check, size: usize) -> Check {
    if size < SHARED_CHECK_MIN_SIZE || matches!(check, Check::Shared(_)) {
        return check;
    }

    let check = SharedCheck::new(check);

    let check = if let Some(existing) = shared.get(&check) {
        existing.clone()
    } else {
        shared.insert(check.clone());
        check
    };

    Check::Shared(check)
}

/// Turns a check that's too large into `None`, so that the caller can decide whether it's needed.
fn unless_too_large(result: Result<Check, CheckError>) -> Result<Option<Check>, CheckError> {
    match result {
        Ok(check) => Ok(Some(check)),
        Err(CheckError::TooLarge) => Ok(None),
        Err(error) => Err(error),
    }
}

/// The types that a type's values can be, with each type once. See [`walk_leaves`] for why a type
/// that's reached again is skipped.
fn variants_of(arena: &Arena<Type>, builtins: &BuiltinTypes, id: TypeId) -> Vec<TypeId> {
    let mut variants = Vec::new();

    for_each_leaf(arena, id, Walk::Leaves, |id, ty, _| {
        match ty {
            // A function is kept as is, rather than as the atom or pair that it's compiled to, so
            // that a check which has to look at it is an error. How a function is represented
            // depends on how it's compiled, so the result would differ between debug and release
            // builds. Subtracting a type still splits a function into an atom and a pair.
            Type::Unresolved
            | Type::Atom(_)
            | Type::Pair(_)
            | Type::Generic(_)
            | Type::Function(_) => variants.push(id),
            Type::Never => {}
            Type::Any => variants.extend([builtins.atom, builtins.recursive_any_pair]),
            Type::Ref(_) | Type::Alias(_) | Type::Struct(_) | Type::Union(_) | Type::Apply(_) => {
                unreachable!()
            }
        }
    });

    variants
}

/// Returns the atoms that a type can contain. If `target` is true, the type is the one being checked
/// against, so generic types are an error since their values can't be known at runtime. Generic
/// targets are still allowed if the value is already known to be assignable (see `check_each`).
fn atoms_of(arena: &Arena<Type>, id: TypeId, target: bool) -> Result<Option<Atoms>, CheckError> {
    let mut restrictions = IndexSet::new();

    let flow = walk_leaves(arena, id, Walk::Leaves, |_, ty, _| match ty {
        Type::Unresolved | Type::Never | Type::Pair(_) => ControlFlow::Continue(()),
        Type::Generic(_) if target => ControlFlow::Break(Err(CheckError::GenericType)),
        Type::Generic(_) | Type::Any => ControlFlow::Break(Ok(Atoms::Unrestricted)),
        Type::Atom(atom) => {
            let Some(restriction) = &atom.restriction else {
                return ControlFlow::Break(Ok(Atoms::Unrestricted));
            };
            add_restriction(&mut restrictions, restriction.clone());
            ControlFlow::Continue(())
        }
        Type::Function(_) => ControlFlow::Break(Err(CheckError::FunctionType)),
        Type::Ref(_) | Type::Alias(_) | Type::Struct(_) | Type::Union(_) | Type::Apply(_) => {
            unreachable!()
        }
    });

    match flow {
        ControlFlow::Break(atoms) => atoms.map(Some),
        ControlFlow::Continue(()) if restrictions.is_empty() => Ok(None),
        ControlFlow::Continue(()) => Ok(Some(Atoms::Restricted(restrictions))),
    }
}

/// Adds an atom's restriction to the ones a type can have, where a length includes every value of
/// that length.
fn add_restriction(restrictions: &mut IndexSet<AtomRestriction>, restriction: AtomRestriction) {
    match &restriction {
        AtomRestriction::Value(value) => {
            if restrictions.contains(&AtomRestriction::Length(value.len())) {
                return;
            }
        }
        AtomRestriction::Length(length) => {
            restrictions.retain(|item| match item {
                AtomRestriction::Value(value) => value.len() != *length,
                AtomRestriction::Length(_) => true,
            });
        }
    }

    restrictions.insert(restriction);
}

/// Returns the pairs that a type can contain. See [`atoms_of`] for the meaning of `target`.
fn pairs_of(
    arena: &Arena<Type>,
    builtins: &BuiltinTypes,
    id: TypeId,
    target: bool,
) -> Result<Vec<Pair>, CheckError> {
    let mut pairs = Vec::new();

    let flow = walk_leaves(arena, id, Walk::Leaves, |_, ty, _| {
        match ty {
            Type::Unresolved | Type::Never | Type::Atom(_) => {}
            &Type::Pair(pair) => pairs.push(pair),
            Type::Generic(_) if target => return ControlFlow::Break(CheckError::GenericType),
            Type::Generic(_) | Type::Any => {
                pairs.push(Pair::new(builtins.recursive_any, builtins.recursive_any));
            }
            Type::Function(_) => return ControlFlow::Break(CheckError::FunctionType),
            Type::Ref(_) | Type::Alias(_) | Type::Struct(_) | Type::Union(_) | Type::Apply(_) => {
                unreachable!()
            }
        }

        ControlFlow::Continue(())
    });

    match flow {
        ControlFlow::Break(error) => Err(error),
        ControlFlow::Continue(()) => Ok(pairs),
    }
}

/// Completed overlap checks, which can be shared by checks that start from an empty stack.
pub(crate) type OverlapMemo = Memo<(TypeId, TypeId), (TypeId, TypeId, usize), bool>;

/// Conservatively determines whether two substituted types may have any values in common. This
/// must return true if there is any overlap, but may also return true if there isn't.
pub(crate) fn types_may_overlap(
    arena: &Arena<Type>,
    builtins: &BuiltinTypes,
    lhs: TypeId,
    rhs: TypeId,
    memo: &mut OverlapMemo,
) -> bool {
    may_overlap(arena, builtins, lhs, rhs, &mut FxIndexSet::default(), memo)
}

fn may_overlap(
    arena: &Arena<Type>,
    builtins: &BuiltinTypes,
    lhs: TypeId,
    rhs: TypeId,
    stack: &mut FxIndexSet<(TypeId, TypeId)>,
    memo: &mut OverlapMemo,
) -> bool {
    grow_stack(|| may_overlap_inner(arena, builtins, lhs, rhs, stack, memo))
}

fn may_overlap_inner(
    arena: &Arena<Type>,
    builtins: &BuiltinTypes,
    lhs: TypeId,
    rhs: TypeId,
    stack: &mut FxIndexSet<(TypeId, TypeId)>,
    memo: &mut OverlapMemo,
) -> bool {
    step();

    if stack.len() > 25 {
        return true;
    }

    if let Some(index) = stack.get_index_of(&(lhs, rhs)) {
        memo.reached(index);
        return true;
    }

    let item = (lhs, rhs);
    stack.insert(item);

    let rhs_variants = variants_of(arena, builtins, rhs);

    let result = variants_of(arena, builtins, lhs).into_iter().any(|lhs| {
        rhs_variants
            .iter()
            .any(|&rhs| match (&arena[lhs], &arena[rhs]) {
                (Type::Atom(lhs), Type::Atom(rhs)) => match (&lhs.restriction, &rhs.restriction) {
                    (None, _) | (_, None) => true,
                    (Some(AtomRestriction::Value(lhs)), Some(AtomRestriction::Value(rhs))) => {
                        lhs == rhs
                    }
                    (Some(AtomRestriction::Length(lhs)), Some(AtomRestriction::Length(rhs))) => {
                        lhs == rhs
                    }
                    (
                        Some(AtomRestriction::Value(value)),
                        Some(AtomRestriction::Length(length)),
                    )
                    | (
                        Some(AtomRestriction::Length(length)),
                        Some(AtomRestriction::Value(value)),
                    ) => value.len() == *length,
                },
                (Type::Pair(lhs_pair), Type::Pair(rhs_pair)) => {
                    // The pairs are cached rather than the types they came from, since each
                    // reference to a type (such as `T` in `(T, T)`) is a distinct alias of it.
                    // This depends on the depth, since overlap is assumed past a certain depth.
                    let key = (lhs, rhs, stack.len());

                    if let Some(result) = memo.get(&key, stack.iter().copied()) {
                        return result;
                    }

                    let frame = memo.enter(stack.len());
                    let result =
                        may_overlap(arena, builtins, lhs_pair.first, rhs_pair.first, stack, memo)
                            && may_overlap(
                                arena,
                                builtins,
                                lhs_pair.rest,
                                rhs_pair.rest,
                                stack,
                                memo,
                            );
                    memo.exit(frame, item, Some(key), result);

                    result
                }
                (Type::Atom(_), Type::Pair(_)) | (Type::Pair(_), Type::Atom(_)) => false,
                _ => true,
            })
    });

    stack.pop().unwrap();

    result
}
