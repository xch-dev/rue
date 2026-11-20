use rue_diagnostic::Lint;

use crate::{LintContext, LintInput};

pub(crate) const LINTS: &[Lint] = &[Lint::ImpreciseNarrowing];

/// Reports the type guards and `match` arms that leave too many combinations of a struct's fields
/// to narrow it by. The types are subtracted while compiling, so these are recorded as sites then.
pub(crate) fn check(cx: &mut LintContext<'_>, input: &LintInput<'_>) {
    for site in &input.sites.imprecise_narrowings {
        cx.report(
            Lint::ImpreciseNarrowing,
            site.srcloc.clone(),
            format!(
                "Too many combinations of the fields of `{}` are left to narrow it by, so they're merged, and it's wider than it could be afterward; check fewer combinations at once",
                site.struct_name
            ),
        );
    }
}
