mod compiler;
mod database;
mod dependency_graph;
mod environment;
mod error;
mod hir;
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
use rue_typing::TypeSystem;

#[derive(Debug)]
pub struct Output {
    pub diagnostics: Vec<Diagnostic>,
    pub node_ptr: NodePtr,
}

pub fn compile(allocator: &mut Allocator, root: &Root, should_codegen: bool) -> Output {
    compile_raw(allocator, root, should_codegen, true)
}

pub fn compile_raw(
    allocator: &mut Allocator,
    root: &Root,
    mut should_codegen: bool,
    should_stdlib: bool,
) -> Output {
    let mut db = Database::new();
    let mut ty = TypeSystem::new();
    let mut ctx = setup_compiler(&mut db, &mut ty);

    let stdlib = if should_stdlib {
        Some(load_standard_library(&mut ctx))
    } else {
        None
    };

    let main_module_id = load_module(&mut ctx, root);
    let symbol_table = compile_modules(ctx);

    let main = try_export_main(&mut db, main_module_id);
    let graph = build_graph(
        &mut db,
        &ty,
        &symbol_table,
        main_module_id,
        &if let Some(stdlib) = stdlib {
            [main_module_id, stdlib].to_vec()
        } else {
            [main_module_id].to_vec()
        },
    );

    should_codegen &= !db.diagnostics().iter().any(Diagnostic::is_error);

    Output {
        diagnostics: db.diagnostics().to_vec(),
        node_ptr: if should_codegen {
            codegen(
                allocator,
                &mut db,
                &graph,
                main.expect("missing main function"),
            )
        } else {
            NodePtr::default()
        },
    }
}
