mod codegen;
mod compiler;
mod database;
mod error;
mod hir;
mod lir;
mod optimizer;
mod scope;
mod symbol;
mod ty;

use clvmr::{Allocator, NodePtr};
use codegen::Codegen;
use compiler::Compiler;
use optimizer::{DependencyGraph, Optimizer};
use rue_parser::Root;
use symbol::{Module, Symbol};

pub use database::*;
pub use error::*;

#[derive(Debug)]
pub struct Output {
    diagnostics: Vec<Diagnostic>,
    node_ptr: NodePtr,
}

impl Output {
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    pub fn node_ptr(&self) -> NodePtr {
        self.node_ptr
    }
}

pub fn analyze(root: &Root) -> Vec<Diagnostic> {
    let mut db = Database::default();
    precompile(&mut db, root);
    db.diagnostics().to_vec()
}

fn precompile(db: &mut Database, root: &Root) -> Option<LirId> {
    let mut compiler = Compiler::new(db);

    let (module_id, declarations) = compiler.declare_root(root);
    compiler.compile_root(root, module_id, declarations);

    let symbol_table = compiler.finish();

    let Symbol::Module(Module { scope_id, .. }) = db.symbol_mut(module_id).clone() else {
        unreachable!();
    };

    let main_symbol_id = db.scope_mut(scope_id).symbol("main")?;

    let Symbol::Module(module) = db.symbol_mut(module_id) else {
        unreachable!();
    };

    module.exported_symbols.insert(main_symbol_id);
    let module_clone = module.clone();

    let graph = DependencyGraph::build(db, &module_clone);
    symbol_table.calculate_unused(db, &graph, &module_clone);

    let mut optimizer = Optimizer::new(db, graph);
    Some(optimizer.opt_main(main_symbol_id))
}

pub fn compile(allocator: &mut Allocator, root: &Root, parsing_succeeded: bool) -> Output {
    let mut db = Database::default();
    let lir_id = precompile(&mut db, root);
    let diagnostics = db.diagnostics().to_vec();

    let node_ptr = if !diagnostics.iter().any(Diagnostic::is_error) && parsing_succeeded {
        let mut codegen = Codegen::new(&mut db, allocator);
        codegen.gen_lir(lir_id.unwrap())
    } else {
        NodePtr::NIL
    };

    Output {
        diagnostics,
        node_ptr,
    }
}
