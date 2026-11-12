use rue_diagnostic::{Applicability, Lint, Suggestion};
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

        let block = block.clone();

        for (index, statement) in block.statements.iter().enumerate() {
            let Statement::Return(value, srcloc) = statement else {
                continue;
            };

            // The `return` can only be replaced with its value if it's the last thing in the body.
            // A `return` without a value returns nil. Attributes can't be on an expression, so a
            // `return` with attributes is left alone, rather than removing them. A statement's
            // location starts at its attributes (and comments before them are trimmed), so it
            // starts with `#` exactly when it has attributes.
            let is_last = index == block.statements.len() - 1 && block.body.is_none();
            let has_attributes = srcloc.text().starts_with('#');

            let replacement = (is_last && !has_attributes).then(|| {
                cx.db()
                    .hir_srcloc(*value)
                    .map_or("nil".to_string(), |value| value.text().to_string())
            });

            let Some(diagnostic) = cx.report(
                Lint::UnnecessaryReturn,
                srcloc.clone(),
                "Unnecessary `return` statement, since the function body can end in an expression instead",
            ) else {
                continue;
            };

            if let Some(replacement) = replacement {
                diagnostic.suggestions.push(Suggestion::replace(
                    "Replace the `return` statement with its value",
                    srcloc.clone(),
                    replacement,
                    Applicability::MachineApplicable,
                ));
            }
        }
    }
}
