use rue_diagnostic::{Lint, Note, SrcLoc};
use rue_hir::{Block, Hir, HirId, Statement, SymbolId};
use std::collections::HashSet;

use rue_types::{Check, Type, TypeId};

use crate::{HirLintPass, LintContext};

/// Reports chains of type guards on the same value (such as `if x is A { .. } else if x is B
/// { .. }`) that end in a `raise`, when the value can still be another type there. Handling some
/// of the types in a union one at a time suggests that each of them was meant to be handled, so
/// the rest raising is likely a mistake. A chain with only one guard is more likely to be a check
/// that rejects everything else, so it's left alone.
#[derive(Debug, Default)]
pub(crate) struct NonExhaustiveUnionGuard;

/// The fewest guards in a chain for it to be reported.
const MIN_GUARDS: usize = 2;

impl HirLintPass for NonExhaustiveUnionGuard {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::NonExhaustiveUnionGuard]
    }

    fn check_hir(&mut self, cx: &mut LintContext<'_>, hir: HirId, node: &Hir) {
        let Hir::If(..) = node else {
            return;
        };

        let db = cx.db();

        // An `else if` is checked as part of the chain it's in
        if let Some(parent) = cx.parent()
            && let Hir::If(_, _, else_, _) = db.hir(parent)
            && *else_ == hir
        {
            return;
        }

        let mut guards = Vec::new();
        let mut current = hir;

        let end = loop {
            let Hir::If(condition, _, else_, _) = db.hir(current) else {
                break current;
            };

            let Some(guard) = guarded(cx, *condition) else {
                break current;
            };

            guards.push(guard);
            current = *else_;
        };

        let Some(raise) = raise_srcloc(cx, end) else {
            return;
        };

        check_chain(cx, &guards, raise);
    }

    fn check_statement(&mut self, cx: &mut LintContext<'_>, block: &Block, index: usize) {
        // A chain starts at an `if` statement that doesn't follow another guard of the chain
        if index > 0 && guarded_statement(cx, &block.statements[index - 1]).is_some() {
            return;
        }

        let mut guards = Vec::new();

        for statement in &block.statements[index..] {
            if let Some(guard) = guarded_statement(cx, statement) {
                guards.push(guard);
                continue;
            }

            // The chain must be followed directly by the `raise`
            if let Statement::Raise(_, srcloc) = statement {
                check_chain(cx, &guards, srcloc.clone());
            }

            return;
        }
    }
}

/// A guard in a chain: the value that's guarded, the node of the guard, and the type it's
/// checked against.
#[derive(Debug, Clone, Copy)]
struct Guarded {
    symbol: SymbolId,
    value: HirId,
    guard: HirId,
    ty: TypeId,
    /// Whether the guard can never be true, which is an error.
    impossible: bool,
}

fn check_chain(cx: &mut LintContext<'_>, guards: &[Guarded], raise: SrcLoc) {
    let Some(first) = guards.first() else {
        return;
    };

    // Every guard must be on the same value
    let guards = guards
        .iter()
        .take_while(|guard| guard.symbol == first.symbol)
        .collect::<Vec<_>>();

    if guards.len() < MIN_GUARDS {
        return;
    }

    let db = cx.db();

    let (Some(ty), Some(scope)) = (db.hir_type(first.value), db.hir_scope(first.guard)) else {
        return;
    };

    // A guard that failed to compile has already been reported
    if cx.contains_unresolved(ty)
        || guards
            .iter()
            .any(|guard| guard.impossible || cx.contains_unresolved(guard.ty))
    {
        return;
    }

    // Only a union is handled one type at a time, rather than checked for a few shapes (such as
    // a value that can be anything)
    let members = union_members(cx, ty);

    if members.len() < 2 {
        return;
    }

    let remaining = members
        .into_iter()
        .filter(|&member| {
            !guards
                .iter()
                .any(|guard| cx.is_assignable(member, guard.ty))
        })
        .collect::<Vec<_>>();

    if remaining.is_empty() {
        return;
    }

    let Some(name) = cx.db().symbol(first.symbol).name() else {
        return;
    };

    let remaining = remaining
        .into_iter()
        .map(|member| cx.type_name(scope, member))
        .collect::<Vec<_>>()
        .join(" | ");

    if let Some(diagnostic) = cx.report(
        Lint::NonExhaustiveUnionGuard,
        raise,
        format!(
            "`{}` can still be `{remaining}` here, after being checked against the other types, so this raises for it",
            name.text()
        ),
    ) {
        diagnostic.notes.push(Note::new(format!(
            "Handle `{remaining}` with another guard, or check for it explicitly if it should raise"
        )));
    }
}

/// The guard that a condition is, if it checks the type of a variable.
fn guarded(cx: &LintContext<'_>, condition: HirId) -> Option<Guarded> {
    let db = cx.db();
    let guard = db.without_casts(condition);

    let Hir::Guard(node) = db.hir(guard) else {
        return None;
    };

    let value = db.without_casts(node.value);

    let Hir::Reference(symbol) = db.hir(value) else {
        return None;
    };

    Some(Guarded {
        symbol: *symbol,
        value,
        guard,
        ty: node.ty,
        impossible: node.check == Check::Impossible,
    })
}

/// The guard of an `if` statement, if its condition checks the type of a variable. The `then`
/// block of an `if` statement always exits, so the statements after it are the `else` branch.
fn guarded_statement(cx: &LintContext<'_>, statement: &Statement) -> Option<Guarded> {
    let Statement::If(statement) = statement else {
        return None;
    };

    guarded(cx, statement.condition)
}

/// The location of the `raise` that a branch consists of, if that's all it does.
fn raise_srcloc(cx: &LintContext<'_>, hir: HirId) -> Option<SrcLoc> {
    let Hir::Block(block) = cx.db().hir(hir) else {
        return None;
    };

    match block.statements.as_slice() {
        [Statement::Raise(_, srcloc)] if block.body.is_none() => Some(srcloc.clone()),
        _ => None,
    }
}

/// The types in a union, including the ones in unions inside of it, or just the type itself if it
/// isn't a union.
fn union_members(cx: &mut LintContext<'_>, ty: TypeId) -> Vec<TypeId> {
    let mut members = Vec::new();
    let mut stack = vec![ty];
    let mut visited = HashSet::new();

    while let Some(ty) = stack.pop() {
        if !visited.insert(ty) {
            continue;
        }

        let semantic = cx.without_aliases(ty);

        match cx.db().ty(semantic) {
            Type::Union(union) => stack.extend(union.types.iter().rev()),
            _ => members.push(ty),
        }
    }

    members
}
