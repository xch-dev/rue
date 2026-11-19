use rue_diagnostic::{Lint, SrcLoc};
use rue_hir::{BinaryOp, Block, Hir, HirId, Statement, may_raise, statements_may_raise};

use crate::{HirLintPass, LintContext};

/// Both branches of an `inline if` are evaluated eagerly, so a branch which can raise will do so
/// regardless of the condition. This runs after every function has been compiled, so that calls
/// to functions which may raise can be detected.
#[derive(Debug, Default)]
pub(crate) struct InlineIfMayRaise;

impl HirLintPass for InlineIfMayRaise {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::InlineIfMayRaise]
    }

    fn check_hir(&mut self, cx: &mut LintContext<'_>, hir: HirId, node: &Hir) {
        let Hir::If(_, then, else_, true) = *node else {
            return;
        };

        let Some(srcloc) = cx.db().hir_srcloc(hir) else {
            return;
        };

        let srcloc = srcloc.clone();

        if may_raise(cx.db(), then) || may_raise(cx.db(), else_) {
            report(cx, srcloc);
        }
    }

    fn check_statement(&mut self, cx: &mut LintContext<'_>, block: &Block, index: usize) {
        // The rest of the block is the else branch of an `inline if` statement
        if let Statement::If(statement) = &block.statements[index]
            && statement.inline
            && (may_raise(cx.db(), statement.then)
                || statements_may_raise(cx.db(), &block.statements[index + 1..], block.body))
        {
            report(cx, statement.srcloc.clone());
        }
    }
}

fn report(cx: &mut LintContext<'_>, srcloc: SrcLoc) {
    cx.report(
        Lint::InlineIfMayRaise,
        srcloc,
        "A branch of this `inline if` can raise, but both branches are always evaluated regardless of the condition; use a normal `if` instead",
    );
}

/// Both operands of `&` and `|` on `Bool` are evaluated eagerly, so a right operand which can
/// raise will do so even when the left operand already decides the result. The left operand isn't
/// checked, since `&&` and `||` evaluate it too. Like [`InlineIfMayRaise`], this runs after every
/// function has been compiled, so that calls to functions which may raise can be detected.
#[derive(Debug, Default)]
pub(crate) struct BoolOperatorMayRaise;

impl HirLintPass for BoolOperatorMayRaise {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::BoolOperatorMayRaise]
    }

    fn check_hir(&mut self, cx: &mut LintContext<'_>, hir: HirId, node: &Hir) {
        let (operator, short_circuit, deciding_value, right) = match *node {
            Hir::Binary(BinaryOp::All, _, right) => ("&", "&&", "false", right),
            Hir::Binary(BinaryOp::Any, _, right) => ("|", "||", "true", right),
            _ => return,
        };

        if !may_raise(cx.db(), right) {
            return;
        }

        let Some(srcloc) = cx.db().hir_srcloc(hir).cloned() else {
            return;
        };

        cx.report(
            Lint::BoolOperatorMayRaise,
            srcloc,
            format!(
                "The right side of this `{operator}` can raise, but it's evaluated even when the left side is `{deciding_value}`; use `{short_circuit}` to skip it"
            ),
        );
    }
}
