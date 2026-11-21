use std::collections::{HashMap, HashSet};

use rue_diagnostic::{Lint, Note};
use rue_hir::{Database, Guard, Hir, HirId, Symbol};
use rue_types::{Check, Type, TypeId};

use crate::{HirLintPass, LintContext, LintInput, MatchArmGuard};

/// A function value is a program whose representation isn't specified. A release build compiles a
/// closure with no captures to its bare body, while a debug build always builds a closure, so the
/// same function can be an atom in one and a pair in the other. Code that looks at the value as
/// data, rather than calling it, can therefore behave differently between the two.
///
/// Two things look at it:
///
/// - A type guard or `match` arm that checks a value that can be a function, such as `f is nil`
///   when `f` is `(fn() -> nil) | nil`. Only the value itself counts, since the type checker
///   rejects a guard that would have to check a function inside of a pair, and a function that
///   the check doesn't reach (such as an item of a list that's checked for being empty) isn't
///   looked at.
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
        // A guard that checks nothing at runtime doesn't look at the value
        if matches!(guard.runtime_check(), Check::None | Check::Impossible) {
            return;
        }

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

        // An earlier guard can narrow a function to the atoms and pairs it could be, which no
        // longer says that it's a function, so the type that the value was declared with counts too
        let declared = match db.hir(guard.value) {
            Hir::Reference(symbol) => declared_type(db.symbol(*symbol)),
            _ => None,
        };

        // An error has already been reported, and the check isn't meaningful
        if cx.contains_unresolved(from) || cx.contains_unresolved(guard.ty) {
            return;
        }

        if ![Some(from), declared]
            .into_iter()
            .flatten()
            .any(|ty| can_be_function(cx, ty))
        {
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

/// The type that a symbol's value was declared with, before any narrowing.
fn declared_type(symbol: &Symbol) -> Option<TypeId> {
    match symbol {
        Symbol::Unresolved | Symbol::Module(_) | Symbol::Builtin(_) => None,
        Symbol::Function(function) => Some(function.ty),
        Symbol::Parameter(parameter) => Some(parameter.ty),
        Symbol::Constant(constant) => Some(constant.value.ty),
        Symbol::Binding(binding) => Some(binding.value.ty),
    }
}

/// Whether a value of the type can be a function itself, rather than only contain one.
fn can_be_function(cx: &mut LintContext<'_>, ty: TypeId) -> bool {
    let ty = cx.substitute(ty);

    members(cx.db(), ty)
        .into_iter()
        .any(|id| matches!(cx.db().ty(id), Type::Function(_)))
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
        &mut HashMap::new(),
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

/// Whether casting a value from one type to the other leaves a function somewhere that the new
/// type doesn't say is a function. Both types must have been substituted.
fn erases_function(
    db: &Database,
    from: TypeId,
    to: TypeId,
    results: &mut HashMap<(TypeId, TypeId), Option<bool>>,
) -> bool {
    match results.get(&(from, to)) {
        Some(Some(result)) => return *result,
        // The type is recursive, and is already being checked against the target
        Some(None) => return false,
        None => {}
    }

    results.insert((from, to), None);

    let result = match db.ty(from) {
        Type::Ref(id) => erases_function(db, *id, to, results),
        Type::Alias(alias) => erases_function(db, alias.inner, to, results),
        Type::Struct(ty) => erases_function(db, ty.inner, to, results),
        Type::Union(union) => union
            .types
            .iter()
            .any(|&id| erases_function(db, id, to, results)),
        Type::Function(_) => !members(db, to).into_iter().any(|id| {
            matches!(
                db.ty(id),
                Type::Function(_) | Type::Generic(_) | Type::Unresolved
            )
        }),
        Type::Pair(pair) => {
            // The pairs that the target could hold this one as, where `Any` holds any pair
            let pairs: Vec<(TypeId, TypeId)> = members(db, to)
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
                    erases_function(db, pair.first, first, results)
                        || erases_function(db, pair.rest, rest, results)
                })
        }
        Type::Unresolved
        | Type::Generic(_)
        | Type::Any
        | Type::Never
        | Type::Atom(_)
        | Type::Apply(_) => false,
    };

    results.insert((from, to), Some(result));
    result
}

/// The types that a value of the type can be, which are the members of its unions, behind any
/// aliases and structs. The type must have been substituted.
fn members(db: &Database, ty: TypeId) -> Vec<TypeId> {
    let mut members = Vec::new();
    let mut visited = HashSet::new();
    let mut stack = vec![ty];

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
            | Type::Apply(_) => members.push(id),
        }
    }

    members
}
