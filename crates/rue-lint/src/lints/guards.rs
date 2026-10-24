use rue_diagnostic::Lint;

use crate::{LintContext, LintInput};

/// Type guards that don't emit a runtime check are always true.
pub(crate) fn check(cx: &mut LintContext<'_>, input: &LintInput<'_>) {
    if !cx.is_enabled(Lint::UnnecessaryGuard) && !cx.is_enabled(Lint::IndistinguishableGuard) {
        return;
    }

    for site in &input.sites.guards {
        if site.has_runtime_check {
            continue;
        }

        let from = cx.type_name(site.scope, site.from);
        let to = cx.type_name(site.scope, site.to);

        if cx.is_assignable(site.from, site.to) {
            cx.report(
                Lint::UnnecessaryGuard,
                site.srcloc.clone(),
                format!(
                    "Unnecessary type guard from `{from}` to `{to}`, since they are already compatible types"
                ),
            );
        } else {
            // The types are different at compile time, but there's nothing to check at runtime
            cx.report(
                Lint::IndistinguishableGuard,
                site.srcloc.clone(),
                format!(
                    "Type guard from `{from}` to `{to}` is always true, since every `{from}` value is also a valid `{to}` at runtime; use a cast, or guard against a narrower type"
                ),
            );
        }
    }
}
