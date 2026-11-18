mod builtins;
mod check;
mod compare;
mod contractive;
mod extractors;
mod hash;
mod infer;
mod intersect;
mod memo;
mod regular;
mod stack;
mod steps;
mod stringify;
mod substitute;
mod subtract;
mod types;
mod unwrap;

pub use builtins::*;
pub use check::*;
pub use compare::*;
pub use contractive::*;
pub use extractors::*;
pub(crate) use hash::*;
pub use infer::*;
pub use intersect::*;
pub(crate) use memo::*;
pub use regular::*;
pub use stack::grow_stack;
pub(crate) use steps::{
    abandoned_pass, compare_memo_enabled, comparison_step_limit, fixpoint_budget, step,
};
#[cfg(feature = "step-counter")]
pub use steps::{
    count_abandoned_passes, count_steps, with_comparison_step_limit, with_fixpoint_budget,
    with_step_limit, without_compare_memo,
};
pub use stringify::*;
pub use substitute::*;
pub use subtract::*;
pub use types::*;
pub use unwrap::*;

#[cfg(test)]
mod tests;
