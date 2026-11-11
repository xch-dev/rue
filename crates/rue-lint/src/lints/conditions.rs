use rue_diagnostic::Lint;
use rue_hir::{Block, Hir, HirId, Statement};

use crate::{HirLintPass, LintContext};

/// Conditions whose type is a single boolean value always take the same branch.
#[derive(Debug, Default)]
pub(crate) struct ConstantCondition;

impl HirLintPass for ConstantCondition {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::ConstantCondition]
    }

    fn check_hir(&mut self, cx: &mut LintContext<'_>, _hir: HirId, node: &Hir) {
        if let Hir::If(condition, ..) = *node {
            check_condition(cx, condition);
        }
    }

    fn check_statement(&mut self, cx: &mut LintContext<'_>, block: &Block, index: usize) {
        match &block.statements[index] {
            Statement::If(statement) => check_condition(cx, statement.condition),
            Statement::Assert(condition, _) => check_condition(cx, *condition),
            _ => {}
        }
    }
}

fn check_condition(cx: &mut LintContext<'_>, condition: HirId) {
    let (Some(ty), Some(srcloc)) = (cx.db().hir_type(condition), cx.db().hir_srcloc(condition))
    else {
        return;
    };

    let srcloc = srcloc.clone();

    // An error has already been reported, and the type is compatible with everything
    if cx.is_unresolved(ty) {
        return;
    }

    let value = if cx.is_castable(ty, cx.builtin_types().bool_true) {
        "true"
    } else if cx.is_castable(ty, cx.builtin_types().bool_false) {
        "false"
    } else {
        return;
    };

    cx.report(
        Lint::ConstantCondition,
        srcloc,
        format!("Condition always evaluates to `{value}`"),
    );
}
