use clvmr::{Allocator, NodePtr};
use codegen::Codegen;
use lowerer::Lowerer;
use optimizer::Optimizer;
use rue_parser::Root;

mod codegen;
mod database;
mod error;
mod hir;
mod lir;
mod lowerer;
mod optimizer;
mod scope;
mod symbol;
mod ty;

pub use database::*;
pub use error::*;

use scope::Scope;

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

pub fn analyze(root: Root) -> Vec<Diagnostic> {
    let mut database = Database::default();
    precompile(&mut database, root).0
}

fn precompile(database: &mut Database, root: Root) -> (Vec<Diagnostic>, Option<LirId>) {
    let scope_id = database.alloc_scope(Scope::default());

    let mut lowerer = Lowerer::new(database);
    lowerer.compile_root(root, scope_id);
    let mut diagnostics = lowerer.finish();

    let Some(main_id) = database.scope_mut(scope_id).symbol("main") else {
        diagnostics.push(Diagnostic::new(
            DiagnosticKind::Error(ErrorKind::MissingMain),
            0..0,
        ));

        return (diagnostics, None);
    };

    let mut optimizer = Optimizer::new(database);
    let lir_id = optimizer.opt_main(scope_id, main_id);
    diagnostics.extend(optimizer.finish());

    (diagnostics, Some(lir_id))
}

pub fn compile(allocator: &mut Allocator, root: Root, parsing_succeeded: bool) -> Output {
    let mut database = Database::default();
    let (diagnostics, lir_id) = precompile(&mut database, root);

    let node_ptr = if !diagnostics.iter().any(Diagnostic::is_error) && parsing_succeeded {
        let mut codegen = Codegen::new(&mut database, allocator);
        codegen.gen_lir(lir_id.unwrap())
    } else {
        NodePtr::NIL
    };

    Output {
        diagnostics,
        node_ptr,
    }
}
