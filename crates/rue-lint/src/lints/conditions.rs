use rue_diagnostic::Lint;

use crate::{LintContext, LintInput};

/// Conditions whose type is a single boolean value always take the same branch.
pub(crate) fn check(cx: &mut LintContext<'_>, input: &LintInput<'_>) {
    if !cx.is_enabled(Lint::ConstantCondition) {
        return;
    }

    let bool_true = cx.builtin_types().bool_true;
    let bool_false = cx.builtin_types().bool_false;

    for site in &input.sites.conditions {
        let value = if cx.is_castable(site.ty, bool_true) {
            "true"
        } else if cx.is_castable(site.ty, bool_false) {
            "false"
        } else {
            continue;
        };

        cx.report(
            Lint::ConstantCondition,
            site.srcloc.clone(),
            format!("Condition always evaluates to `{value}`"),
        );
    }
}
