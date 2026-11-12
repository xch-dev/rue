use rue_diagnostic::Lint;
use rue_hir::{Hir, HirId, Statement, SymbolId};

use crate::{HirLintPass, LintContext};

/// A `return` statement directly in a function body can be replaced with an expression at the
/// end of the body. Anything after it is never evaluated.
#[derive(Debug, Default)]
pub(crate) struct UnnecessaryReturn;

impl HirLintPass for UnnecessaryReturn {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::UnnecessaryReturn]
    }

    fn check_function(&mut self, cx: &mut LintContext<'_>, _symbol: SymbolId, body: HirId) {
        let Hir::Block(block) = cx.db().hir(body) else {
            return;
        };

        let srclocs = block
            .statements
            .iter()
            .filter_map(|statement| match statement {
                Statement::Return(_, srcloc) => Some(srcloc.clone()),
                _ => None,
            })
            .collect::<Vec<_>>();

        for srcloc in srclocs {
            cx.report(
                Lint::UnnecessaryReturn,
                srcloc,
                "Unnecessary `return` statement, since the function body can end in an expression instead",
            );
        }
    }
}
