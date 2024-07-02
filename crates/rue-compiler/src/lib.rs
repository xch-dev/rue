mod codegen;
mod compiler;
mod database;
mod dependency_graph;
mod environment;
mod error;
mod hir;
mod lir;
mod lowerer;
mod mir;
mod optimizer;
mod scope;
mod symbol;
mod value;

use clvmr::{Allocator, NodePtr};
use compiler::{
    build_graph, codegen, compile_modules, load_module, load_standard_library, setup_compiler,
    try_export_main,
};
use rue_parser::Root;

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
    let mut ctx = setup_compiler(&mut db);

    let stdlib = load_standard_library(&mut ctx);
    let main_module_id = load_module(&mut ctx, root);
    let symbol_table = compile_modules(ctx);

    try_export_main(&mut db, main_module_id);
    build_graph(
        &mut db,
        &symbol_table,
        main_module_id,
        &[main_module_id, stdlib],
    );

    db.diagnostics().to_vec()
}

pub fn compile(allocator: &mut Allocator, root: &Root, mut should_codegen: bool) -> Output {
    let mut db = Database::default();
    let mut ctx = setup_compiler(&mut db);

    let stdlib = load_standard_library(&mut ctx);
    let main_module_id = load_module(&mut ctx, root);
    let symbol_table = compile_modules(ctx);

    let main = try_export_main(&mut db, main_module_id).expect("missing main function");
    let graph = build_graph(
        &mut db,
        &symbol_table,
        main_module_id,
        &[main_module_id, stdlib],
    );

    should_codegen &= !db.diagnostics().iter().any(Diagnostic::is_error);

    Output {
        diagnostics: db.diagnostics().to_vec(),
        node_ptr: if should_codegen {
            codegen(allocator, &mut db, &graph, main)
        } else {
            NodePtr::default()
        },
    }
}
