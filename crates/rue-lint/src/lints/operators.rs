use rue_diagnostic::Lint;
use rue_hir::{Hir, HirId, UnaryOp};

use crate::{HirLintPass, LintContext};

/// A unary `+` has no effect on an `Int`. On other types it's a type error, which the compiler
/// reports instead.
#[derive(Debug, Default)]
pub(crate) struct UnnecessaryPlus;

impl HirLintPass for UnnecessaryPlus {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::UnnecessaryPlus]
    }

    fn check_hir(&mut self, cx: &mut LintContext<'_>, hir: HirId, node: &Hir) {
        let Hir::Unary(UnaryOp::Plus, operand) = *node else {
            return;
        };

        let (Some(srcloc), Some(operand)) = (cx.db().hir_srcloc(hir), cx.db().hir_type(operand))
        else {
            return;
        };

        let srcloc = srcloc.clone();

        if cx.is_assignable(operand, cx.builtin_types().int) {
            cx.report(
                Lint::UnnecessaryPlus,
                srcloc,
                "Unnecessary `+` operator, since it has no effect",
            );
        }
    }
}
