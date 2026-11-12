use crate::{LintContext, LintInput, run_hir_pass};

mod casts;
mod conditions;
mod guards;
mod inline_ifs;
mod operators;
mod returns;
mod statements;
mod syntax;
mod unused;

/// A lint pass, which reports any number of lints through the [`LintContext`].
pub(crate) type LintPass = fn(&mut LintContext<'_>, &LintInput<'_>);

/// Every lint pass, in the order they run.
pub(crate) const PASSES: &[LintPass] = &[
    syntax::check,
    run_hir_pass::<operators::UnnecessaryPlus>,
    run_hir_pass::<casts::UnnecessaryCast>,
    run_hir_pass::<guards::UnnecessaryGuard>,
    run_hir_pass::<conditions::ConstantCondition>,
    run_hir_pass::<statements::UnusedValue>,
    run_hir_pass::<inline_ifs::InlineIfMayRaise>,
    run_hir_pass::<returns::UnnecessaryReturn>,
    unused::check,
];
