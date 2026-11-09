use rue_diagnostic::Lint;

use crate::{LintContext, LintInput};

/// Expression statements discard their value, so it should always be nil.
pub(crate) fn check(cx: &mut LintContext<'_>, input: &LintInput<'_>) {
    if !cx.is_enabled(Lint::UnusedValue) {
        return;
    }

    for site in &input.sites.expr_statements {
        if !site.always_nil {
            cx.report(
                Lint::UnusedValue,
                site.srcloc.clone(),
                "Value returned by statement is unused",
            );
        }
    }
}
