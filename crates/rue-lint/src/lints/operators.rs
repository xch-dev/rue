use rue_diagnostic::{Applicability, Lint, Suggestion};
use rue_hir::{Hir, HirId, UnaryOp};

use crate::{
    HirLintPass, LintContext,
    text::{Position, position, text_at},
};

/// A unary `+` has no effect on an `Int`. On other types it's a type error, which the compiler
/// reports instead.
#[derive(Debug, Default)]
pub(crate) struct UnnecessaryPlus;

impl HirLintPass for UnnecessaryPlus {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::UnnecessaryPlus]
    }

    fn check_hir(&mut self, cx: &mut LintContext<'_>, hir: HirId, node: &Hir) {
        let Hir::Unary(UnaryOp::Plus, operand_hir) = *node else {
            return;
        };

        let db = cx.db();

        let (Some(srcloc), Some(ty), Some(operand)) = (
            db.hir_srcloc(hir).cloned(),
            db.hir_type(operand_hir),
            db.hir_srcloc(operand_hir).cloned(),
        ) else {
            return;
        };

        if !cx.is_assignable(ty, cx.builtin_types().int) {
            return;
        }

        // The location includes any parentheses around the `+`, which the operand still needs
        // before something that binds more tightly than a prefix operator does, such as `**`:
        // `(+-a) ** 2` is `(-a) ** 2`, not `-a ** 2`
        let replacement = match position(cx, hir) {
            Position::Postfix => text_at(cx.db(), operand_hir, Position::Postfix),
            _ => None,
        }
        .unwrap_or_else(|| operand.text().to_string());

        if let Some(diagnostic) = cx.report(
            Lint::UnnecessaryPlus,
            srcloc.clone(),
            "Unnecessary `+` operator, since it has no effect",
        ) {
            diagnostic.suggestions.push(Suggestion::replace(
                "Remove the `+`",
                srcloc,
                replacement,
                Applicability::MachineApplicable,
            ));
        }
    }
}
