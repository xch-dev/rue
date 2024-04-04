#![forbid(clippy::unwrap_used)]

use clvmr::{Allocator, NodePtr};
use codegen::codegen;
use database::{Database, SymbolId};
use lowerer::lower;
use rue_parser::Root;
use value::Value;

mod codegen;
mod database;
mod error;
mod lowerer;
mod scope;
mod symbol;
mod ty;
mod value;

pub use error::*;

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
    let mut output = lower(&mut db, root);

    let Some(main) = db.scope_mut(output.main_scope_id).symbol("main") else {
        output.diagnostics.push(Diagnostic::new(
            DiagnosticKind::Error,
            DiagnosticInfo::MissingMain,
            0..0,
        ));

        return Output {
            diagnostics: output.diagnostics,
            node_ptr: NodePtr::NIL,
        };
    };

    let node_ptr = codegen(allocator, &mut db, main);

    Output {
        diagnostics: output.diagnostics,
        node_ptr,
    }
}
