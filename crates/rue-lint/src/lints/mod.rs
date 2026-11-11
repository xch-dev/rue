use rue_diagnostic::Lint;

use crate::{HirLintPass, LintContext, LintInput};

mod casts;
mod conditions;
mod guards;
mod inline_ifs;
mod operators;
mod returns;
mod statements;
mod syntax;
mod unreachable;
mod unused;

/// A lint pass that runs once over the [`LintInput`], such as the syntax trees or the declaration
/// graph, rather than over each node of the HIR.
pub(crate) struct InputPass {
    /// The lints the pass can report, so that it can be skipped if they're all allowed.
    pub(crate) lints: &'static [Lint],
    pub(crate) run: fn(&mut LintContext<'_>, &LintInput<'_>),
}

pub(crate) const INPUT_PASSES: &[InputPass] = &[
    InputPass {
        lints: syntax::LINTS,
        run: syntax::check,
    },
    InputPass {
        lints: unused::LINTS,
        run: unused::check,
    },
];

/// Every [`HirLintPass`], which all run during the same walk over the HIR.
pub(crate) fn hir_passes() -> Vec<Box<dyn HirLintPass>> {
    vec![
        Box::new(operators::UnnecessaryPlus),
        Box::new(casts::UnnecessaryCast),
        Box::new(guards::UnnecessaryGuard),
        Box::new(conditions::ConstantCondition),
        Box::new(statements::UnusedValue),
        Box::new(inline_ifs::InlineIfMayRaise),
        Box::new(returns::UnnecessaryReturn),
        Box::new(unreachable::UnreachableCode),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The lints that are reported while applying lint levels, rather than by a pass.
    const LEVEL_LINTS: &[Lint] = &[Lint::UnknownLint, Lint::UnfulfilledLintExpectation];

    /// Every lint must be reported by exactly one pass, so that a pass isn't skipped while one of
    /// its lints is enabled, and so that a new lint can't be added without being reported.
    #[test]
    fn every_lint_is_reported_by_one_pass() {
        let mut lints = LEVEL_LINTS.to_vec();

        for pass in INPUT_PASSES {
            lints.extend(pass.lints);
        }

        for pass in hir_passes() {
            lints.extend(pass.lints());
        }

        lints.sort();

        let mut expected = Lint::ALL.to_vec();
        expected.sort();

        assert_eq!(lints, expected);
    }
}
