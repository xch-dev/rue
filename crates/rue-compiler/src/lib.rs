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
    errors: Vec<CompilerError>,
    node_ptr: NodePtr,
}

impl Output {
    pub fn errors(&self) -> &[CompilerError] {
        &self.errors
    }

    pub fn node_ptr(&self) -> NodePtr {
        self.node_ptr
    }
}

pub fn compile(allocator: &mut Allocator, root: Root) -> Output {
    let mut db = Database::default();
    let mut output = lower(&mut db, root);

    let Some(main) = db.scope_mut(output.main_scope_id).symbol("main") else {
        output.errors.push(CompilerError::MissingMain);

        return Output {
            errors: output.errors,
            node_ptr: NodePtr::NIL,
        };
    };

    db.scope_mut(output.main_scope_id).use_symbol(main);
    let node_ptr = codegen(allocator, &mut db, main);

    Output {
        errors: output.errors,
        node_ptr,
    }
}
