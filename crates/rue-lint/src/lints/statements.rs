use rue_diagnostic::Lint;
use rue_hir::{Block, Statement};

use crate::{HirLintPass, LintContext};

/// Expression statements discard their value, so it should always be nil.
#[derive(Debug, Default)]
pub(crate) struct UnusedValue;

impl HirLintPass for UnusedValue {
    const LINTS: &'static [Lint] = &[Lint::UnusedValue];

    fn check_statement(&mut self, cx: &mut LintContext<'_>, block: &Block, index: usize) {
        if let Statement::Expr(statement) = &block.statements[index]
            && !statement.always_nil
        {
            cx.report(
                Lint::UnusedValue,
                statement.srcloc.clone(),
                "Value returned by statement is unused",
            );
        }
    }
}
