use rue_diagnostic::Lint;

use crate::{HirLintPass, LintContext, LintInput};

mod bitwise;
mod branches;
mod casts;
mod comparisons;
mod conditions;
mod entrypoints;
mod exports;
mod guards;
mod inline_ifs;
mod inlining;
mod naming;
mod operators;
mod recursion;
mod repeated;
mod returns;
mod shadowing;
mod statements;
mod syntax;
mod unreachable;
mod unused;
mod unused_fields;

pub(crate) use unused::live_declarations;

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
    InputPass {
        lints: exports::LINTS,
        run: exports::check,
    },
    InputPass {
        lints: naming::LINTS,
        run: naming::check,
    },
    InputPass {
        lints: shadowing::LINTS,
        run: shadowing::check,
    },
    InputPass {
        lints: entrypoints::LINTS,
        run: entrypoints::check,
    },
];

/// Every [`HirLintPass`], which all run during the same walk over the HIR.
pub(crate) fn hir_passes<'a>(input: &'a LintInput<'_>) -> Vec<Box<dyn HirLintPass + 'a>> {
    vec![
        Box::new(operators::UnnecessaryPlus),
        Box::new(casts::UnnecessaryCast),
        Box::new(guards::UnnecessaryGuard),
        Box::new(conditions::ConstantCondition),
        Box::new(statements::UnusedValue),
        Box::new(inline_ifs::InlineIfMayRaise),
        Box::new(returns::UnnecessaryReturn),
        Box::new(unreachable::UnreachableCode::default()),
        Box::new(comparisons::SelfComparison),
        Box::new(comparisons::BoolComparison),
        Box::new(branches::IdenticalBranches),
        Box::new(branches::DuplicateCondition),
        Box::new(branches::NeedlessBool),
        Box::new(branches::NegatedCondition),
        Box::new(branches::ManualAssert),
        Box::new(statements::DebugStatement),
        Box::new(statements::NeedlessLetReturn),
        Box::new(casts::BytesIntConfusion),
        Box::new(recursion::UnconditionalRecursion),
        Box::new(repeated::RepeatedExpensiveCall::new(input)),
        Box::new(bitwise::BoolBitwiseOperator::default()),
        Box::new(inlining::Inlining::new(input)),
        Box::new(unused_fields::UnusedStructField::new(input)),
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

        let entrypoints = std::collections::HashSet::new();
        let input = LintInput::new(
            crate::LintSites::default(),
            Vec::new(),
            &entrypoints,
            rue_diagnostic::LintOptions::default(),
            false,
        );

        for pass in hir_passes(&input) {
            lints.extend(pass.lints());
        }

        lints.sort();

        let mut expected = Lint::ALL.to_vec();
        expected.sort();

        assert_eq!(lints, expected);
    }
}
