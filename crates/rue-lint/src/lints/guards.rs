use rue_diagnostic::Lint;
use rue_hir::{Hir, HirId};
use rue_types::Check;

use crate::{HirLintPass, LintContext};

/// Type guards that don't emit a runtime check are always true.
#[derive(Debug, Default)]
pub(crate) struct UnnecessaryGuard;

impl HirLintPass for UnnecessaryGuard {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::UnnecessaryGuard, Lint::IndistinguishableGuard]
    }

    fn check_hir(&mut self, cx: &mut LintContext<'_>, hir: HirId, node: &Hir) {
        let Hir::Guard(guard) = node else {
            return;
        };

        // Guards that can never be true are reported as errors by the compiler instead
        if guard.check != Check::None {
            return;
        }

        let db = cx.db();

        let (Some(from), Some(srcloc), Some(scope)) = (
            db.hir_type(guard.value),
            db.hir_srcloc(hir).cloned(),
            db.hir_scope(hir),
        ) else {
            return;
        };

        let to = guard.ty;

        // An error has already been reported, and the type is compatible with everything
        if cx.is_unresolved(from) || cx.is_unresolved(to) {
            return;
        }

        let lint = if cx.is_assignable(from, to) {
            Lint::UnnecessaryGuard
        } else {
            // The types are different at compile time, but there's nothing to check at runtime
            Lint::IndistinguishableGuard
        };

        if !cx.is_enabled(lint) {
            return;
        }

        let from = cx.type_name(scope, from);
        let to = cx.type_name(scope, to);

        let message = match lint {
            Lint::UnnecessaryGuard => format!(
                "Unnecessary type guard from `{from}` to `{to}`, since they are already compatible types"
            ),
            _ => format!(
                "Type guard from `{from}` to `{to}` is always true, since every `{from}` value is also a valid `{to}` at runtime; use a cast, or guard against a narrower type"
            ),
        };

        cx.report(lint, srcloc, message);
    }
}
