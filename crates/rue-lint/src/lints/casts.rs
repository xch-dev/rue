use num_bigint::{BigInt, Sign};
use rue_diagnostic::{Applicability, Lint, Suggestion};
use rue_hir::{Hir, HirId};
use rue_types::Comparison;

use crate::{HirLintPass, LintContext};

/// Casts between types that are assignable in both directions have no effect.
#[derive(Debug, Default)]
pub(crate) struct UnnecessaryCast;

impl HirLintPass for UnnecessaryCast {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::UnnecessaryCast]
    }

    fn check_hir(&mut self, cx: &mut LintContext<'_>, hir: HirId, node: &Hir) {
        let Hir::Cast(value) = *node else {
            return;
        };

        let db = cx.db();

        let (Some(from), Some(to), Some(srcloc), Some(scope), Some(value)) = (
            db.hir_type(value),
            db.hir_type(hir),
            db.hir_srcloc(hir).cloned(),
            db.hir_scope(hir),
            db.hir_srcloc(value).cloned(),
        ) else {
            return;
        };

        // An error has already been reported, and the type is compatible with everything
        if cx.is_unresolved(from) || cx.is_unresolved(to) {
            return;
        }

        if cx.compare(from, to) != Comparison::Assign || !cx.is_assignable(to, from) {
            return;
        }

        let from = cx.type_name(scope, from);
        let to = cx.type_name(scope, to);

        if let Some(diagnostic) = cx.report(
            Lint::UnnecessaryCast,
            srcloc.clone(),
            format!("Unnecessary cast from `{from}` to `{to}`, since they are compatible with each other"),
        ) {
            diagnostic.suggestions.push(Suggestion::replace(
                "Remove the cast",
                srcloc,
                value.text(),
                Applicability::MachineApplicable,
            ));
        }
    }
}

/// Integers are signed and compared by their bytes, so casting a byte literal to `Int` can give a
/// negative number, or one that isn't equal to the same number written as an `Int`.
#[derive(Debug, Default)]
pub(crate) struct BytesIntConfusion;

impl HirLintPass for BytesIntConfusion {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::BytesIntConfusion]
    }

    fn check_hir(&mut self, cx: &mut LintContext<'_>, hir: HirId, node: &Hir) {
        let Hir::Cast(_) = node else {
            return;
        };

        let db = cx.db();
        let value = db.without_casts(hir);

        let (Hir::Bytes(bytes), Some(ty), Some(srcloc), Some(value_srcloc)) = (
            db.hir(value),
            db.hir_type(hir),
            db.hir_srcloc(hir),
            db.hir_srcloc(value),
        ) else {
            return;
        };

        // Only hex literals are checked, rather than strings, which are rarely meant as numbers
        if !value_srcloc.text().starts_with("0x") {
            return;
        }

        let bytes = bytes.clone();
        let srcloc = srcloc.clone();
        let literal = value_srcloc.text().to_string();

        let int = cx.builtin_types().int;

        if cx.is_unresolved(ty) || !cx.is_assignable(ty, int) || !cx.is_assignable(int, ty) {
            return;
        }

        let value = BigInt::from_signed_bytes_be(&bytes);
        let unsigned = BigInt::from_bytes_be(Sign::Plus, &bytes);

        let message = if bytes.first().is_some_and(|byte| byte & 0x80 != 0) {
            format!(
                "`{literal}` is the negative number `{value}` as an `Int`, since integers are signed"
            )
        } else if bytes.first() == Some(&0) && bytes.get(1).is_none_or(|byte| byte & 0x80 == 0) {
            format!(
                "`{literal}` has a leading zero byte, so it isn't equal to the `Int` `{value}`, since integers are compared by their bytes"
            )
        } else {
            return;
        };

        if let Some(diagnostic) = cx.report(Lint::BytesIntConfusion, srcloc.clone(), message) {
            diagnostic.suggestions.push(Suggestion::replace(
                format!("Write the number `{unsigned}` instead"),
                srcloc,
                unsigned.to_string(),
                Applicability::MaybeIncorrect,
            ));
        }
    }
}
