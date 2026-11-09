use rue_diagnostic::Lint;
use rue_types::Comparison;

use crate::{LintContext, LintInput};

/// Casts between types that are assignable in both directions have no effect.
pub(crate) fn check(cx: &mut LintContext<'_>, input: &LintInput<'_>) {
    if !cx.is_enabled(Lint::UnnecessaryCast) {
        return;
    }

    for site in &input.sites.casts {
        if site.comparison != Comparison::Assign || !cx.is_assignable(site.to, site.from) {
            continue;
        }

        let from = cx.type_name(site.scope, site.from);
        let to = cx.type_name(site.scope, site.to);

        cx.report(
            Lint::UnnecessaryCast,
            site.srcloc.clone(),
            format!("Unnecessary cast from `{from}` to `{to}`, since they are compatible with each other"),
        );
    }
}
