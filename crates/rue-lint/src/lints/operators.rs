use rue_diagnostic::Lint;

use crate::{LintContext, LintInput};

/// A unary `+` has no effect on an `Int`. On other types it's a type error, which the compiler
/// reports instead.
pub(crate) fn check(cx: &mut LintContext<'_>, input: &LintInput<'_>) {
    if !cx.is_enabled(Lint::UnnecessaryPlus) {
        return;
    }

    let int = cx.builtin_types().int;

    for site in &input.sites.unary_plus {
        if cx.is_assignable(site.operand, int) {
            cx.report(
                Lint::UnnecessaryPlus,
                site.srcloc.clone(),
                "Unnecessary `+` operator, since it has no effect",
            );
        }
    }
}
