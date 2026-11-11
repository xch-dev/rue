use crate::{LintContext, LintInput, run_hir_pass};

mod casts;
mod conditions;
mod guards;
mod inline_ifs;
mod operators;
mod statements;
mod syntax;
mod unused;

/// A lint pass, which reports any number of lints through the [`LintContext`].
pub(crate) type LintPass = fn(&mut LintContext<'_>, &LintInput<'_>);

/// Every lint pass, in the order they run.
pub(crate) const PASSES: &[LintPass] = &[
    syntax::check,
    run_hir_pass::<operators::UnnecessaryPlus>,
    casts::check,
    guards::check,
    run_hir_pass::<conditions::ConstantCondition>,
    run_hir_pass::<statements::UnusedValue>,
    run_hir_pass::<inline_ifs::InlineIfMayRaise>,
    unused::check,
];
