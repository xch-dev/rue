use std::collections::{HashMap, HashSet};

use rue_diagnostic::{Lint, Note};
use rue_hir::{Database, Guard, Hir, HirId};
use rue_types::{Check, Type, TypeId};

use crate::{HirLintPass, LintContext, LintInput, MatchArmGuard};

/// A function value is a program whose representation isn't specified. A release build compiles a
/// closure with no captures to its bare body, while a debug build always builds a closure, so the
/// same function can be an atom in one and a pair in the other. Code that looks at the value as
/// data, rather than calling it, can therefore behave differently between the two.
///
/// Two things look at it:
///
/// - A type guard or `match` arm whose runtime check inspects a value that can be a function,
///   such as `f is nil` when `f` is `(fn() -> nil) | nil`.
/// - A cast to a type that forgets that the value is a function, such as `f as Any`, after which
///   it can be inspected like any other value. A function can only be cast to a type that holds
///   every value, so this is always `Any` (or a union that covers it) where the function is.
///
/// A function is also assignable to `Any` without a cast, which is how it's passed to a function
/// that treats it as an opaque tree, such as `tree_hash`. That isn't reported, and neither is a
/// cast written directly as an argument of a call, which is the same thing.
#[derive(Debug)]
pub(crate) struct FunctionValueObservation<'a> {
    arms: &'a HashMap<HirId, MatchArmGuard>,
}

impl<'a> FunctionValueObservation<'a> {
    pub(crate) fn new(input: &'a LintInput<'a>) -> Self {
        Self {
            arms: &input.sites.match_arm_guards,
        }
    }
}

impl HirLintPass for FunctionValueObservation<'_> {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::FunctionValueObservation]
    }

    fn check_hir(&mut self, cx: &mut LintContext<'_>, hir: HirId, node: &Hir) {
        match node {
            Hir::Guard(guard) => self.check_guard(cx, hir, guard),
            Hir::Cast(value) => check_cast(cx, hir, *value),
            _ => {}
        }
    }
}

impl FunctionValueObservation<'_> {
    fn check_guard(&self, cx: &mut LintContext<'_>, hir: HirId, guard: &Guard) {
        let db = cx.db();

        // The guard of a `match` arm is generated, so its type and location are recorded instead
        let (what, from, srcloc) = if let Some(arm) = self.arms.get(&hir) {
            ("match arm", arm.value, arm.pattern.clone())
        } else if let (Some(from), Some(srcloc)) =
            (db.hir_type(guard.value), db.hir_srcloc(hir).cloned())
        {
            ("type guard", from, srcloc)
        } else {
            return;
        };

        // An error has already been reported, and the check isn't meaningful
        if cx.contains_unresolved(from) || cx.contains_unresolved(guard.ty) {
            return;
        }

        let from = cx.substitute(from);

        if !inspects_function(cx.db(), &guard.runtime_check(), &[from]) {
            return;
        }

        if let Some(diagnostic) = cx.report(
            Lint::FunctionValueObservation,
            srcloc,
            format!(
                "This {what} inspects a function value, whose representation depends on how the function is compiled, so the result can differ between debug and release builds"
            ),
        ) {
            diagnostic.notes.push(Note::new(
                "Wrap the function in a pair or a struct, such as `(function, nil)`, so that the check doesn't look at the function itself",
            ));
        }
    }
}

fn check_cast(cx: &mut LintContext<'_>, hir: HirId, value: HirId) {
    let db = cx.db();

    let (Some(from), Some(to), Some(srcloc), Some(scope)) = (
        db.hir_type(value),
        db.hir_type(hir),
        db.hir_srcloc(hir).cloned(),
        db.hir_scope(hir),
    ) else {
        return;
    };

    // Passing a function to a parameter that accepts any value doesn't need a cast, so one that's
    // written there doesn't make the function any more observable than the call already does
    if let Some(parent) = cx.parent()
        && let Hir::FunctionCall(call) = db.hir(parent)
        && call.args.contains(&hir)
    {
        return;
    }

    // An error has already been reported, and the type is compatible with everything
    if cx.contains_unresolved(from) || cx.contains_unresolved(to) {
        return;
    }

    let substituted_from = cx.substitute(from);
    let substituted_to = cx.substitute(to);

    if !erases_function(
        cx.db(),
        substituted_from,
        substituted_to,
        &mut HashSet::new(),
    ) {
        return;
    }

    let from = cx.type_name(scope, from);
    let to = cx.type_name(scope, to);

    if let Some(diagnostic) = cx.report(
        Lint::FunctionValueObservation,
        srcloc,
        format!(
            "Casting `{from}` to `{to}` lets a function value be inspected as data, but its representation depends on how the function is compiled, which differs between debug and release builds"
        ),
    ) {
        diagnostic.notes.push(Note::new(
            "A function can be passed where `Any` is expected without a cast, such as to `tree_hash`",
        ));
    }
}

/// What the values of a set of types can be at one position of a value, as far as functions are
/// concerned.
#[derive(Debug, Default)]
struct Shape {
    /// Whether the value itself can be a function.
    function: bool,
    /// The types of the first and rest of the value, when it's a pair.
    firsts: Vec<TypeId>,
    rests: Vec<TypeId>,
}

/// The types must have been substituted.
fn shape(db: &Database, types: &[TypeId]) -> Shape {
    let mut shape = Shape::default();
    let mut visited = HashSet::new();
    let mut stack = types.to_vec();

    while let Some(id) = stack.pop() {
        if !visited.insert(id) {
            continue;
        }

        match db.ty(id) {
            Type::Function(_) => shape.function = true,
            Type::Pair(pair) => {
                shape.firsts.push(pair.first);
                shape.rests.push(pair.rest);
            }
            Type::Ref(id) => stack.push(*id),
            Type::Alias(alias) => stack.push(alias.inner),
            Type::Struct(ty) => stack.push(ty.inner),
            Type::Union(union) => stack.extend(union.types.iter().copied()),
            // An `Any` or generic value isn't known to be a function, and `Apply` is substituted
            Type::Unresolved
            | Type::Generic(_)
            | Type::Any
            | Type::Never
            | Type::Atom(_)
            | Type::Apply(_) => {}
        }
    }

    shape
}

/// Whether the check looks at a value that can be a function, when it's run on a value of one of
/// the types. Each part of the check is followed to the part of the value that it's run on, so a
/// function that's somewhere the check doesn't look (such as an item of a list that's only checked
/// for being empty) doesn't count.
fn inspects_function(db: &Database, check: &Check, types: &[TypeId]) -> bool {
    match check {
        Check::None | Check::Impossible => false,
        Check::IsAtom { .. } | Check::IsPair { .. } | Check::Atom(_) => shape(db, types).function,
        Check::Pair(first, rest) => {
            let shape = shape(db, types);

            shape.function
                || inspects_function(db, first, &shape.firsts)
                || inspects_function(db, rest, &shape.rests)
        }
        Check::And(checks) | Check::Or(checks) => checks
            .iter()
            .any(|check| inspects_function(db, check, types)),
    }
}

/// Whether casting a value from one type to the other leaves a function somewhere that the new
/// type doesn't say is a function. Both types must have been substituted.
fn erases_function(
    db: &Database,
    from: TypeId,
    to: TypeId,
    visited: &mut HashSet<(TypeId, TypeId)>,
) -> bool {
    // A type that's reached again is recursive, and has already been checked against the target
    if !visited.insert((from, to)) {
        return false;
    }

    match db.ty(from) {
        Type::Ref(id) => erases_function(db, *id, to, visited),
        Type::Alias(alias) => erases_function(db, alias.inner, to, visited),
        Type::Struct(ty) => erases_function(db, ty.inner, to, visited),
        Type::Union(union) => union
            .types
            .iter()
            .any(|&id| erases_function(db, id, to, visited)),
        Type::Function(_) => !targets(db, to).iter().any(|&id| {
            matches!(
                db.ty(id),
                Type::Function(_) | Type::Generic(_) | Type::Unresolved
            )
        }),
        Type::Pair(pair) => {
            // The pairs that the target could hold this one as, where `Any` holds any pair
            let pairs: Vec<(TypeId, TypeId)> = targets(db, to)
                .into_iter()
                .filter_map(|id| match db.ty(id) {
                    Type::Pair(pair) => Some((pair.first, pair.rest)),
                    Type::Any => Some((id, id)),
                    _ => None,
                })
                .collect();

            // The function is only erased if no member of the target keeps it as one
            !pairs.is_empty()
                && pairs.into_iter().all(|(first, rest)| {
                    erases_function(db, pair.first, first, visited)
                        || erases_function(db, pair.rest, rest, visited)
                })
        }
        Type::Unresolved
        | Type::Generic(_)
        | Type::Any
        | Type::Never
        | Type::Atom(_)
        | Type::Apply(_) => false,
    }
}

/// The types that a value of the target type can be, which are the members of a union.
fn targets(db: &Database, to: TypeId) -> Vec<TypeId> {
    let mut targets = Vec::new();
    let mut visited = HashSet::new();
    let mut stack = vec![to];

    while let Some(id) = stack.pop() {
        if !visited.insert(id) {
            continue;
        }

        match db.ty(id) {
            Type::Ref(id) => stack.push(*id),
            Type::Alias(alias) => stack.push(alias.inner),
            Type::Struct(ty) => stack.push(ty.inner),
            Type::Union(union) => stack.extend(union.types.iter().copied()),
            Type::Unresolved
            | Type::Generic(_)
            | Type::Any
            | Type::Never
            | Type::Atom(_)
            | Type::Pair(_)
            | Type::Function(_)
            | Type::Apply(_) => targets.push(id),
        }
    }

    targets
}
