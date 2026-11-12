use rue_diagnostic::Lint;
use rue_hir::{Hir, HirId};
use rue_types::Comparison;

use crate::{HirLintPass, LintContext};

/// Casts between types that are assignable in both directions have no effect.
#[derive(Debug, Default)]
pub(crate) struct UnnecessaryCast;

impl HirLintPass for UnnecessaryCast {
    const LINTS: &'static [Lint] = &[Lint::UnnecessaryCast];

    fn check_hir(&mut self, cx: &mut LintContext<'_>, hir: HirId, node: &Hir) {
        let Hir::Cast(value) = *node else {
            return;
        };

        let db = cx.db();

        let (Some(from), Some(to), Some(srcloc), Some(scope)) = (
            db.hir_type(value),
            db.hir_type(hir),
            db.hir_srcloc(hir).cloned(),
            db.hir_scope(hir),
        ) else {
            return;
        };

        if cx.compare(from, to) != Comparison::Assign || !cx.is_assignable(to, from) {
            return;
        }

        let from = cx.type_name(scope, from);
        let to = cx.type_name(scope, to);

        cx.report(
            Lint::UnnecessaryCast,
            srcloc,
            format!("Unnecessary cast from `{from}` to `{to}`, since they are compatible with each other"),
        );
    }
}
