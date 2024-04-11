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
    let mut db = Database::default();
    let scope_id = db.alloc_scope(Scope::default());
    let mut lowerer = Lowerer::new(&mut db);
    lowerer.compile_root(root, scope_id);
    lowerer.finish()
}

pub fn compile(allocator: &mut Allocator, root: Root) -> Output {
    let mut db = Database::default();
    let scope_id = db.alloc_scope(Scope::default());

    let mut lowerer = Lowerer::new(&mut db);
    lowerer.compile_root(root, scope_id);
    let mut diagnostics = lowerer.finish();

    let Some(main_id) = db.scope_mut(scope_id).symbol("main") else {
        diagnostics.push(Diagnostic::new(
            DiagnosticKind::Error,
            DiagnosticInfo::MissingMain,
            0..0,
        ));

        return Output {
            diagnostics,
            node_ptr: NodePtr::NIL,
        };
    };

    let node_ptr = if !diagnostics
        .iter()
        .any(|diagnostic| diagnostic.kind() == DiagnosticKind::Error)
    {
        let mut optimizer = Optimizer::new(&mut db);
        let lir_id = optimizer.opt_main(main_id);

        let mut codegen = Codegen::new(&mut db, allocator);
        codegen.gen_lir(lir_id)
    } else {
        NodePtr::NIL
    };

    Output {
        diagnostics,
        node_ptr,
    }
}
