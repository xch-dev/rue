use rue_parser::{AstNode, Block};

use crate::{
    compiler::{block::BlockTerminator, Compiler},
    scope::Scope,
    value::Value,
    ErrorKind, TypeId,
};

impl Compiler<'_> {
    pub fn compile_block_expr(&mut self, block: &Block, expected_type: Option<TypeId>) -> Value {
        let scope_id = self.db.alloc_scope(Scope::default());

        self.scope_stack.push(scope_id);
        let summary = self.compile_block(block, expected_type);
        self.scope_stack.pop().unwrap();

        if summary.terminator == BlockTerminator::Return {
            self.db
                .error(ErrorKind::ExplicitReturnInExpr, block.syntax().text_range());
        }

        summary.value
    }
}
