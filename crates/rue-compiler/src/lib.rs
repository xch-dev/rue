#![forbid(clippy::unwrap_used)]

use clvmr::{Allocator, NodePtr};
use codegen::Codegen;
use database::{ScopeId, SymbolId};
use lowerer::Lowerer;
use rue_parser::Root;
use value::Value;

mod codegen;
mod database;
mod lowerer;
mod scope;
mod value;

pub fn compile(allocator: &mut Allocator, root: Root) -> Output {
    let compiler = Lowerer::new();
    let mut output = compiler.compile_root(root);

    let Some(main) = output.db.scope_mut(output.main_scope_id).get_symbol("main") else {
        output.errors.push("no main function".to_string());

        return Output {
            errors: output.errors,
            node_ptr: NodePtr::NIL,
        };
    };

    output.db.scope_mut(output.main_scope_id).use_symbol(main);

    let node_ptr = Codegen::new(output.db, allocator).gen_main(main);

    Output {
        errors: output.errors,
        node_ptr,
    }
}

#[derive(Debug, Clone)]
enum Symbol {
    Function { scope_id: ScopeId, value: Value },
    Parameter,
}

pub struct Output {
    errors: Vec<String>,
    node_ptr: NodePtr,
}

impl Output {
    pub fn errors(&self) -> &[String] {
        &self.errors
    }

    pub fn node_ptr(&self) -> NodePtr {
        self.node_ptr
    }
}
