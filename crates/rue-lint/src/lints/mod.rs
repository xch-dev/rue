use crate::{LintContext, LintInput};

mod casts;
mod conditions;
mod guards;
mod inline_ifs;
mod statements;
mod syntax;
mod unused;

/// A lint pass, which reports any number of lints through the [`LintContext`].
pub(crate) type LintPass = fn(&mut LintContext<'_>, &LintInput<'_>);

/// Every lint pass, in the order they run.
pub(crate) const PASSES: &[LintPass] = &[
    syntax::check,
    casts::check,
    guards::check,
    conditions::check,
    statements::check,
    inline_ifs::check,
    unused::check,
];
