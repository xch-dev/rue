use rue_diagnostic::{Applicability, Lint, Suggestion};
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

        let db = cx.db();

        let (Some(srcloc), Some(ty), Some(operand)) = (
            db.hir_srcloc(hir).cloned(),
            db.hir_type(operand),
            db.hir_srcloc(operand).cloned(),
        ) else {
            return;
        };

        if !cx.is_assignable(ty, cx.builtin_types().int) {
            return;
        }

        if let Some(diagnostic) = cx.report(
            Lint::UnnecessaryPlus,
            srcloc.clone(),
            "Unnecessary `+` operator, since it has no effect",
        ) {
            diagnostic.suggestions.push(Suggestion::replace(
                "Remove the `+`",
                srcloc,
                operand.text(),
                Applicability::MachineApplicable,
            ));
        }
    }
}
