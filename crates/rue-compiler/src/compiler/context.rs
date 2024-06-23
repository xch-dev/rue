use clvmr::{Allocator, NodePtr};
use indexmap::IndexMap;
use rue_parser::Root;

use crate::{
    codegen::Codegen,
    optimizer::{DependencyGraph, Optimizer},
    symbol::{Module, Symbol},
    Database, SymbolId,
};

use super::{builtins::builtins, item::Declarations, symbol_table::SymbolTable, Compiler};

pub struct CompilerContext<'a> {
    compiler: Compiler<'a>,
    roots: IndexMap<SymbolId, (Root, Declarations)>,
}

pub fn setup_compiler(db: &mut Database) -> CompilerContext<'_> {
    let builtins = builtins(db);
    let compiler = Compiler::new(db, builtins);
    CompilerContext {
        compiler,
        roots: IndexMap::new(),
    }
}

pub fn load_module(ctx: &mut CompilerContext<'_>, root: &Root) -> SymbolId {
    let (module_id, declarations) = ctx.compiler.declare_root(root);
    ctx.roots.insert(module_id, (root.clone(), declarations));
    module_id
}

pub fn compile_modules(mut ctx: CompilerContext<'_>) -> SymbolTable {
    for (module_id, (root, declarations)) in ctx.roots {
        ctx.compiler.compile_root(&root, module_id, declarations);
    }
    ctx.compiler.finish()
}

pub fn build_graph(
    db: &mut Database,
    symbol_table: &SymbolTable,
    entrypoint: SymbolId,
) -> DependencyGraph {
    let Symbol::Module(module) = db.symbol_mut(entrypoint).clone() else {
        unreachable!();
    };
    let graph = DependencyGraph::build(db, &module);
    symbol_table.calculate_unused(db, &graph, &module);
    graph
}

pub fn codegen(
    allocator: &mut Allocator,
    db: &mut Database,
    graph: DependencyGraph,
    entrypoint: SymbolId,
) -> NodePtr {
    let mut optimizer = Optimizer::new(db, graph);
    let lir_id = optimizer.opt_main(entrypoint);
    let mut codegen = Codegen::new(db, allocator);
    codegen.gen_lir(lir_id)
}

pub fn try_export_main(db: &mut Database, main_module_id: SymbolId) -> Option<SymbolId> {
    let Symbol::Module(Module { scope_id, .. }) = db.symbol_mut(main_module_id).clone() else {
        unreachable!();
    };

    if let Some(main_symbol_id) = db.scope_mut(scope_id).symbol("main") {
        let Symbol::Module(module) = db.symbol_mut(main_module_id) else {
            unreachable!();
        };
        module.exported_symbols.insert(main_symbol_id);
        Some(main_symbol_id)
    } else {
        None
    }
}
