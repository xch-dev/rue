mod check_recursion;
mod compile;
mod compiler;
mod const_eval;
mod file;
mod syntax_map;

pub use check_recursion::*;
pub use compile::*;
pub use compiler::*;
pub use file::*;
pub use syntax_map::*;

#[cfg(test)]
mod cancel_tests;
#[cfg(test)]
mod const_eval_tests;
#[cfg(test)]
mod hir_type_tests;
#[cfg(test)]
mod import_tests;
#[cfg(test)]
mod lint_tests;
#[cfg(test)]
mod narrowing_tests;
#[cfg(test)]
mod nesting_tests;
#[cfg(test)]
mod resolve_tests;
#[cfg(test)]
mod test_utils;
