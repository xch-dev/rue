use rue_diagnostic::{Lint, Note};

use crate::{LintContext, LintInput};

pub(crate) const LINTS: &[Lint] = &[Lint::UnreachableMatchArm];

/// Reports the arms of `match` expressions that are never taken, since the arms before them handle
/// every value they match. The compiler leaves them out of the HIR, so they're recorded as sites.
pub(crate) fn check(cx: &mut LintContext<'_>, input: &LintInput<'_>) {
    for arm in &input.sites.unreachable_match_arms {
        let message = if arm.catch_all.is_some() {
            "Unreachable match arm, since an earlier arm handles every value that's left"
        } else {
            "Unreachable match arm, since the earlier arms handle every value it matches"
        };

        let Some(diagnostic) = cx.report(Lint::UnreachableMatchArm, arm.pattern.clone(), message)
        else {
            continue;
        };

        if let Some(catch_all) = arm.catch_all.clone() {
            diagnostic.notes.push(Note::at(
                catch_all,
                "This arm handles every value that's left",
            ));
        }
    }
}
