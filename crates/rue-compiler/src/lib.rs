mod check_recursion;
mod compile;
mod compiler;
mod const_eval;
pub mod debug_metadata;
mod entrypoint;
mod file;
mod syntax_map;

pub use check_recursion::*;
pub use compile::*;
pub use compiler::*;
pub use debug_metadata::EncodeError;
pub(crate) use debug_metadata::build_debug_metadata;
pub use file::*;
pub use syntax_map::*;

#[cfg(test)]
mod assert_message_tests;
#[cfg(test)]
mod cancel_tests;
#[cfg(test)]
mod comparison_limit_tests;
#[cfg(test)]
mod const_eval_tests;
#[cfg(test)]
mod entrypoint_tests;
#[cfg(test)]
mod enum_tests;
#[cfg(test)]
mod hir_type_tests;
#[cfg(test)]
mod import_pass_limit_tests;
#[cfg(test)]
mod import_tests;
#[cfg(test)]
mod index_tests;
#[cfg(test)]
mod inference_order_tests;
#[cfg(test)]
mod lint_tests;
#[cfg(test)]
mod list_spread_tests;
#[cfg(test)]
mod match_tests;
#[cfg(test)]
mod narrowing_tests;
#[cfg(test)]
mod nesting_tests;
#[cfg(test)]
mod placeholder_tests;
#[cfg(test)]
mod resolve_tests;
#[cfg(test)]
mod solver_stress_tests;
#[cfg(test)]
mod test_utils;
