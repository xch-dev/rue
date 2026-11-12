use std::path::PathBuf;

use clvmr::{Allocator, NodePtr};
use id_arena::Arena;
use rue_lir::CodegenOptions;
use rue_options::CompilerOptions;

use crate::{Database, DependencyGraph, Environment, Lowerer, SymbolId};

/// Lowers a symbol and its dependencies, and generates the CLVM program for it, which is how the
/// compiler builds each program. Debug messages refer to source files relative to `base_path`.
pub fn codegen(
    db: &mut Database,
    allocator: &mut Allocator,
    symbol: SymbolId,
    options: CompilerOptions,
    base_path: PathBuf,
) -> Result<NodePtr, rue_lir::Error> {
    let graph = DependencyGraph::build(db, symbol, options);

    let mut arena = Arena::new();
    let mut lowerer = Lowerer::new(db, &mut arena, &graph, options, symbol, base_path);
    let mut lir = lowerer.lower_symbol_value(&Environment::default(), symbol);

    if options.optimize_lir {
        lir = rue_lir::optimize(&mut arena, lir);
    }

    rue_lir::codegen(
        &arena,
        allocator,
        lir,
        CodegenOptions {
            optimize_static_pairs: options.optimize_static_pairs,
        },
    )
}
