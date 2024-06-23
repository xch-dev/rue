use rue_parser::{AstNode, Block};

use crate::{compiler::Compiler, scope::Scope, ty::Value, ErrorKind, TypeId};

impl Compiler<'_> {
    pub fn compile_block_expr(&mut self, block: &Block, expected_type: Option<TypeId>) -> Value {
        let scope_id = self.db.alloc_scope(Scope::default());

        self.scope_stack.push(scope_id);
        let (value, explicit_return) = self.compile_block(block, expected_type);
        self.scope_stack.pop().unwrap();

        if explicit_return {
            self.db
                .error(ErrorKind::ExplicitReturnInExpr, block.syntax().text_range());
        }

        value
    }
}
