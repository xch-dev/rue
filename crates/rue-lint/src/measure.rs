//! Measures the programs that the compiler generates, for lints whose advice depends on the size
//! of the code, which is hard to estimate from the HIR alone (since it depends on constant folding,
//! and how values are passed between functions).

use std::path::PathBuf;

use clvmr::{Allocator, serde::node_to_bytes};
use id_arena::Arena;
use rue_hir::SymbolId;
use rue_hir::{Database, DependencyGraph, Environment, Lowerer};
use rue_lir::CodegenOptions;
use rue_options::CompilerOptions;

/// The options that programs are measured with, which are the ones used to build them for
/// release, since that's what the cost applies to.
fn options() -> CompilerOptions {
    CompilerOptions::default()
}

/// Whether the program uses the symbol.
pub(crate) fn program_uses(db: &Database, program: SymbolId, symbol: SymbolId) -> bool {
    DependencyGraph::build(db, program, options()).references(symbol) > 0
}

/// The number of bytes that the program is serialized to, or `None` if it can't be generated.
/// This must only be called if compilation didn't report any errors.
pub(crate) fn program_size(db: &mut Database, program: SymbolId) -> Option<u64> {
    let options = options();
    let graph = DependencyGraph::build(db, program, options);

    let mut arena = Arena::new();
    let mut lowerer = Lowerer::new(db, &mut arena, &graph, options, program, PathBuf::new());
    let mut lir = lowerer.lower_symbol_value(&Environment::default(), program);

    if options.optimize_lir {
        lir = rue_lir::optimize(&mut arena, lir);
    }

    let mut allocator = Allocator::new();

    let program = rue_lir::codegen(
        &arena,
        &mut allocator,
        lir,
        CodegenOptions {
            optimize_static_pairs: options.optimize_static_pairs,
        },
    )
    .ok()?;

    Some(node_to_bytes(&allocator, program).ok()?.len() as u64)
}
