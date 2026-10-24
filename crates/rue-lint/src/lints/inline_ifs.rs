use rue_diagnostic::Lint;
use rue_hir::{Hir, Statement, may_raise, statements_may_raise};

use crate::{InlineIf, LintContext, LintInput};

/// Both branches of an `inline if` are evaluated eagerly, so a branch which can raise will do so
/// regardless of the condition. This runs after every function has been compiled, so that calls
/// to functions which may raise can be detected.
pub(crate) fn check(cx: &mut LintContext<'_>, input: &LintInput<'_>) {
    if !cx.is_enabled(Lint::InlineIfMayRaise) {
        return;
    }

    for site in &input.sites.inline_ifs {
        let db = cx.db();

        let raises = match site.inline_if {
            InlineIf::Expr(hir) => match db.hir(hir) {
                Hir::If(_, then, else_, _) => may_raise(db, *then) || may_raise(db, *else_),
                _ => false,
            },
            InlineIf::Statement(hir, index) => match db.hir(hir) {
                Hir::Block(block) => match &block.statements[index] {
                    Statement::If(stmt) => {
                        may_raise(db, stmt.then)
                            || statements_may_raise(db, &block.statements[index + 1..], block.body)
                    }
                    _ => false,
                },
                _ => false,
            },
        };

        if raises {
            cx.report(
                Lint::InlineIfMayRaise,
                site.srcloc.clone(),
                "A branch of this `inline if` can raise, but both branches are always evaluated regardless of the condition; use a normal `if` instead",
            );
        }
    }
}
