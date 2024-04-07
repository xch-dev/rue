use clvmr::{Allocator, NodePtr};
use codegen::Codegen;
use database::{Database, SymbolId};
use lowerer::Lowerer;
use rue_parser::Root;

mod codegen;
mod database;
mod error;
mod hir;
mod lowerer;
mod scope;
mod symbol;
mod ty;

#[cfg(test)]
mod tests;

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
        let mut codegen = Codegen::new(&mut db, allocator);
        codegen.gen_main(main_id)
    } else {
        NodePtr::NIL
    };

    Output {
        diagnostics,
        node_ptr,
    }
}
