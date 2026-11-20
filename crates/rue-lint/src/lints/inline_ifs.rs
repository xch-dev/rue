use std::collections::HashMap;

use rue_diagnostic::{Lint, SrcLoc};
use rue_hir::{
    BinaryOp, Block, ControlStatement, Hir, HirId, Statement, may_raise, statements_may_raise,
};

use crate::{HirLintPass, LintContext, LintInput};

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
        match &block.statements[index] {
            // The rest of the block is the else branch of an `inline if` statement
            Statement::If(statement)
                if statement.inline
                    && (may_raise(cx.db(), statement.then)
                        || statements_may_raise(
                            cx.db(),
                            &block.statements[index + 1..],
                            block.body,
                        )) =>
            {
                report(cx, statement.srcloc.clone());
            }
            // The branches of an `inline if` chain with a location are checked as an expression,
            // but an `if` statement without an `else` whose block falls through doesn't have one
            Statement::Control(statement)
                if statement.inline
                    && matches!(cx.db().hir(statement.hir), Hir::If(..))
                    && ((cx.db().hir_srcloc(statement.hir).is_none()
                        && may_raise(cx.db(), statement.hir))
                        || rest_may_raise(cx, statement, block, index)) =>
            {
                report(cx, statement.srcloc.clone());
            }
            _ => {}
        }
    }
}

/// Whether the rest of the block after an `inline` statement can raise when a branch of the
/// statement returns, since the branch that falls through to it is evaluated too.
fn rest_may_raise(
    cx: &LintContext<'_>,
    statement: &ControlStatement,
    block: &Block,
    index: usize,
) -> bool {
    statement.may_return
        && statement.fallthroughs > 0
        && statements_may_raise(cx.db(), &block.statements[index + 1..], block.body)
}

/// Every arm of an `inline match` is evaluated eagerly, like the branches of an `inline if`, so an
/// arm which can raise will do so whichever arm is taken. The arms are generated as an `inline if`
/// chain with no location of its own, so they're recorded by the compiler.
pub(crate) struct InlineMatchMayRaise<'a> {
    arms: &'a HashMap<HirId, Vec<HirId>>,
}

impl<'a> InlineMatchMayRaise<'a> {
    pub(crate) fn new(input: &'a LintInput<'a>) -> Self {
        Self {
            arms: &input.sites.inline_match_arms,
        }
    }
}

impl HirLintPass for InlineMatchMayRaise<'_> {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::InlineMatchMayRaise]
    }

    fn check_hir(&mut self, cx: &mut LintContext<'_>, hir: HirId, _node: &Hir) {
        let Some(arms) = self.arms.get(&hir) else {
            return;
        };

        if arms.len() < 2 || !arms.iter().any(|&arm| may_raise(cx.db(), arm)) {
            return;
        }

        let Some(srcloc) = cx.db().hir_srcloc(hir).cloned() else {
            return;
        };

        report_match(cx, srcloc);
    }

    fn check_statement(&mut self, cx: &mut LintContext<'_>, block: &Block, index: usize) {
        // A `match` statement's arms are checked as an expression, but when one returns, the rest
        // of the block is evaluated too, by the arm that falls through to it
        if let Statement::Control(statement) = &block.statements[index]
            && statement.inline
            && matches!(cx.db().hir(statement.hir), Hir::Block(_))
            && rest_may_raise(cx, statement, block, index)
        {
            report_match(cx, statement.srcloc.clone());
        }
    }
}

fn report_match(cx: &mut LintContext<'_>, srcloc: SrcLoc) {
    cx.report(
        Lint::InlineMatchMayRaise,
        srcloc,
        "An arm of this `inline match` can raise, but every arm is always evaluated regardless of which one matches; use a normal `match` instead",
    );
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

        // The right operand is reported rather than the whole expression, since in a chain like
        // `a & b & c` every operator's expression starts at `a`
        let Some(srcloc) = cx
            .db()
            .hir_srcloc(right)
            .or_else(|| cx.db().hir_srcloc(hir))
            .cloned()
        else {
            return;
        };

        cx.report(
            Lint::BoolOperatorMayRaise,
            srcloc,
            format!(
                "This operand can raise, but it's evaluated even when the left side of `{operator}` is `{deciding_value}`; use `{short_circuit}` to skip it"
            ),
        );
    }
}
