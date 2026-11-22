use rue_diagnostic::{Lint, Note};

use crate::{LintContext, LintInput, UnreachableReason};

pub(crate) const LINTS: &[Lint] = &[Lint::UnreachableMatchArm, Lint::IndistinguishableMatchArm];

/// Reports the arms of `match` expressions that are never taken, since the arms before them handle
/// every value they match, and the arms that also handle members of a union that they can't tell
/// apart from their own type. Unreachable arms are left out of the HIR, so both are recorded as
/// sites while compiling.
pub(crate) fn check(cx: &mut LintContext<'_>, input: &LintInput<'_>) {
    for arm in &input.sites.unreachable_match_arms {
        let message = match &arm.reason {
            UnreachableReason::Handled => {
                "Unreachable match arm, since the earlier arms handle every value it matches"
                    .to_string()
            }
            UnreachableReason::CatchAll(_) => {
                "Unreachable match arm, since an earlier arm handles every value that's left"
                    .to_string()
            }
            UnreachableReason::NoValues(ty) => {
                format!("Unreachable match arm, since the matched value's type `{ty}` has no values")
            }
            UnreachableReason::Indistinguishable { members, .. } => format!(
                "Unreachable match arm, since an earlier arm also handles {members}, which it can't tell apart from its own type at runtime"
            ),
        };

        let Some(diagnostic) = cx.report(Lint::UnreachableMatchArm, arm.pattern.clone(), message)
        else {
            continue;
        };

        match &arm.reason {
            UnreachableReason::Handled | UnreachableReason::NoValues(_) => {}
            UnreachableReason::CatchAll(catch_all) => diagnostic.notes.push(Note::at(
                catch_all.clone(),
                "This arm handles every value that's left",
            )),
            UnreachableReason::Indistinguishable { arm, members } => {
                diagnostic.notes.push(Note::at(
                    arm.clone(),
                    format!("This arm also handles {members}"),
                ));
            }
        }
    }

    for arm in &input.sites.indistinguishable_match_arms {
        let (ty, members) = (&arm.ty, &arm.members);
        cx.report(
            Lint::IndistinguishableMatchArm,
            arm.pattern.clone(),
            format!(
                "Match arm `{ty}` also handles {members}, since they can't be told apart from it at runtime; add them to this pattern if that's intended"
            ),
        );
    }
}
