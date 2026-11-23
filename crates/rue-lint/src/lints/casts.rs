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

        // An unresolved type is compatible with everything, but an error has already been
        // reported for it, so it's checked last since it walks the whole type
        if cx.compare(from, to) != Comparison::Assign
            || !cx.is_assignable(to, from)
            || cx.contains_unresolved(from)
            || cx.contains_unresolved(to)
        {
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

/// How bytes read as an `Int`, which is signed and compared by its bytes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BytesAsInt {
    /// The first byte's high bit is set, so the `Int` is negative: `0xff` is `-1`.
    Negative,
    /// The bytes start with a zero byte that the number doesn't need, so they aren't equal to the
    /// same number written as an `Int`: `0x007f` isn't equal to `127`, which is `0x7f`.
    LeadingZero,
    /// The bytes are the same as the number they're read as, such as `0x7f` and `127`, or `0x00ff`
    /// and `255`, whose zero byte keeps it positive. Empty bytes are `0`.
    Same,
}

pub fn bytes_as_int(bytes: &[u8]) -> BytesAsInt {
    match bytes {
        [first, ..] if first & 0x80 != 0 => BytesAsInt::Negative,
        [0] => BytesAsInt::LeadingZero,
        [0, second, ..] if second & 0x80 == 0 => BytesAsInt::LeadingZero,
        _ => BytesAsInt::Same,
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

        if !cx.is_assignable(ty, int) || !cx.is_assignable(int, ty) || cx.contains_unresolved(ty) {
            return;
        }

        let value = BigInt::from_signed_bytes_be(&bytes);
        let unsigned = BigInt::from_bytes_be(Sign::Plus, &bytes);

        let message = match bytes_as_int(&bytes) {
            BytesAsInt::Negative => format!(
                "`{literal}` is the negative number `{value}` as an `Int`, since integers are signed"
            ),
            BytesAsInt::LeadingZero => format!(
                "`{literal}` has a leading zero byte, so it isn't equal to the `Int` `{value}`, since integers are compared by their bytes"
            ),
            BytesAsInt::Same => return,
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
