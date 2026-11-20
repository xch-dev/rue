use std::collections::{HashMap, HashSet};

use rue_diagnostic::{Lint, SrcLoc};
use rue_hir::{
    BinaryOp, Block, ControlStatement, Database, Hir, HirId, Statement, may_raise,
    statements_may_raise, statements_terminate,
};

use crate::{HirLintPass, LintContext, LintInput};

/// Both branches of an `inline if` are evaluated eagerly, so a branch which can raise will do so
/// regardless of the condition. This runs after every function has been compiled, so that calls
/// to functions which may raise can be detected.
#[derive(Debug, Default)]
pub(crate) struct InlineIfMayRaise {
    /// The statements that have been reported, by their node, since one inside the branch of
    /// another statement can be found from both.
    reported: HashSet<HirId>,
}

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
        let rest_may_raise =
            statements_may_raise(cx.db(), &block.statements[index + 1..], block.body);

        match &block.statements[index] {
            // The rest of the block is the else branch of an `inline if` statement
            Statement::If(statement)
                if statement.inline
                    && (may_raise(cx.db(), statement.then) || rest_may_raise)
                    && self.reported.insert(statement.condition) =>
            {
                report(cx, statement.srcloc.clone());
            }
            Statement::Control(statement) => {
                // The branches of an `inline if` chain with a location are checked as an
                // expression, but an `if` statement without an `else` whose block falls through
                // doesn't have one
                if statement.inline
                    && let Hir::If(_, then, else_, _) = *cx.db().hir(statement.hir)
                    && ((cx.db().hir_srcloc(statement.hir).is_none()
                        && (may_raise(cx.db(), then) || may_raise(cx.db(), else_)))
                        || (evaluates_rest(statement) && rest_may_raise))
                    && self.reported.insert(statement.hir)
                {
                    report(cx, statement.srcloc.clone());
                }

                if rest_may_raise {
                    for (hir, srcloc, is_match) in eager_statements(cx.db(), statement.hir) {
                        if !is_match && self.reported.insert(hir) {
                            report(cx, srcloc);
                        }
                    }
                }
            }
            _ => {}
        }
    }
}

/// Whether the branch of an `inline` statement that falls through evaluates the rest of the block
/// when another branch returns, since every branch is evaluated.
fn evaluates_rest(statement: &ControlStatement) -> bool {
    statement.inline && statement.may_return && statement.fallthroughs > 0
}

/// The `inline` statements in the branches of a statement that can reach the end of their branch,
/// and so continue with the rest of the block after the statement. Each one that evaluates the rest
/// of its block when a branch returns (an `inline if` statement without an `else`, or see
/// [`evaluates_rest`]) evaluates that too. Each is given with its node, its location and whether
/// it's an `inline match`.
fn eager_statements(db: &Database, hir: HirId) -> Vec<(HirId, SrcLoc, bool)> {
    let mut found = Vec::new();
    collect_eager_statements(db, hir, &mut found);
    found
}

fn collect_eager_statements(db: &Database, hir: HirId, found: &mut Vec<(HirId, SrcLoc, bool)>) {
    match db.hir(hir) {
        Hir::If(_, then, else_, _) => {
            collect_eager_statements(db, *then, found);
            collect_eager_statements(db, *else_, found);
        }
        // The block that binds a `match` statement's value has its `if` chain as its body
        Hir::Block(Block {
            body: Some(body), ..
        }) => collect_eager_statements(db, *body, found),
        Hir::Block(Block {
            statements,
            body: None,
        }) => {
            for (index, statement) in statements.iter().enumerate() {
                // A statement that's followed by a `return` or `raise` in its branch doesn't
                // continue with what comes after the branch
                if statements_terminate(&statements[index + 1..]) {
                    break;
                }

                match statement {
                    Statement::If(statement) if statement.inline => {
                        found.push((statement.condition, statement.srcloc.clone(), false));
                    }
                    Statement::Control(statement) => {
                        if evaluates_rest(statement) {
                            let is_match = matches!(db.hir(statement.hir), Hir::Block(_));
                            found.push((statement.hir, statement.srcloc.clone(), is_match));
                        }

                        collect_eager_statements(db, statement.hir, found);
                    }
                    Statement::If(_)
                    | Statement::Expr(_)
                    | Statement::Let(..)
                    | Statement::Return(..)
                    | Statement::Assert(..)
                    | Statement::Raise(..)
                    | Statement::Debug(..) => {}
                }
            }
        }
        _ => {}
    }
}

/// Every arm of an `inline match` is evaluated eagerly, like the branches of an `inline if`, so an
/// arm which can raise will do so whichever arm is taken. The arms are generated as an `inline if`
/// chain with no location of its own, so they're recorded by the compiler.
pub(crate) struct InlineMatchMayRaise<'a> {
    arms: &'a HashMap<HirId, Vec<HirId>>,
    /// The statements that have been reported, by their node, since one inside the branch of
    /// another statement can be found from both.
    reported: HashSet<HirId>,
}

impl<'a> InlineMatchMayRaise<'a> {
    pub(crate) fn new(input: &'a LintInput<'a>) -> Self {
        Self {
            arms: &input.sites.inline_match_arms,
            reported: HashSet::new(),
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
        let Statement::Control(statement) = &block.statements[index] else {
            return;
        };

        if !statements_may_raise(cx.db(), &block.statements[index + 1..], block.body) {
            return;
        }

        // A `match` statement's arms are checked as an expression, but when one returns, the rest
        // of the block is evaluated too, by the arm that falls through to it
        if evaluates_rest(statement)
            && matches!(cx.db().hir(statement.hir), Hir::Block(_))
            && self.reported.insert(statement.hir)
        {
            report_match(cx, statement.srcloc.clone());
        }

        for (hir, srcloc, is_match) in eager_statements(cx.db(), statement.hir) {
            if is_match && self.reported.insert(hir) {
                report_match(cx, srcloc);
            }
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
