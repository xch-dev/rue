//! Measures the programs that the compiler generates, for lints whose advice depends on the size
//! of the code, which is hard to estimate from the HIR alone (since it depends on constant folding,
//! and how values are passed between functions).

use std::path::PathBuf;

use clvmr::{Allocator, serde::node_to_bytes};
use rue_hir::{Database, DependencyGraph, SymbolId};
use rue_options::CompilerOptions;

/// The options that programs are measured with, which are the ones used to build them for
/// release, since that's what the cost applies to.
fn options() -> CompilerOptions {
    CompilerOptions::default()
}

/// The symbols that the program depends on, and how many times each of them is referenced.
pub(crate) fn program_graph(db: &Database, program: SymbolId) -> DependencyGraph {
    DependencyGraph::build(db, program, options())
}

/// The number of bytes that the program is serialized to, or `None` if it can't be generated.
/// This must only be called if compilation didn't report any errors.
pub(crate) fn program_size(db: &mut Database, program: SymbolId) -> Option<u64> {
    let mut allocator = Allocator::new();
    let program = rue_hir::codegen(db, &mut allocator, program, options(), PathBuf::new()).ok()?;
    Some(node_to_bytes(&allocator, program).ok()?.len() as u64)
}
