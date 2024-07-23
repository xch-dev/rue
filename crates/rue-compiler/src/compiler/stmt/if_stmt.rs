use std::collections::HashMap;

use rue_parser::{AstNode, IfStmt};
use rue_typing::TypeId;

use crate::{
    compiler::{block::BlockTerminator, Compiler},
    scope::Scope,
    value::{GuardPath, TypeOverride},
    ErrorKind, HirId,
};

impl Compiler<'_> {
    /// Compiles an if statement, returning the condition HIR, then block HIR, and else block guards.
    pub fn compile_if_stmt(
        &mut self,
        if_stmt: &IfStmt,
        expected_type: Option<TypeId>,
    ) -> (HirId, HirId, HashMap<GuardPath, TypeOverride>) {
        // Compile the condition expression.
        let condition = if_stmt
            .condition()
            .map(|condition| self.compile_expr(&condition, Some(self.ty.std().bool)))
            .unwrap_or_else(|| self.unknown());

        // Check that the condition is a boolean.
        self.type_check(
            condition.type_id,
            self.ty.std().bool,
            if_stmt.syntax().text_range(),
        );

        let then_block = if let Some(then_block) = if_stmt.then_block() {
            // We create a new scope for the then block.
            let scope_id = self.db.alloc_scope(Scope::default());

            // We can apply any type guards from the condition.
            self.type_guard_stack.push(condition.then_guards());

            // Compile the then block.
            self.scope_stack.push(scope_id);
            let summary = self.compile_block(&then_block, expected_type);
            self.scope_stack.pop().unwrap();

            // Pop the type guards, since we've left the scope.
            self.type_guard_stack.pop().unwrap();

            // If there's an implicit return, we want to raise an error.
            // This could technically work but makes the intent of the code unclear.
            if summary.terminator == BlockTerminator::Implicit {
                self.db.error(
                    ErrorKind::ImplicitReturnInIf,
                    then_block.syntax().text_range(),
                );
            }

            summary.value
        } else {
            self.unknown()
        };

        // Check that the output matches the expected type.
        self.type_check(
            then_block.type_id,
            expected_type.unwrap_or(self.ty.std().unknown),
            if_stmt.syntax().text_range(),
        );

        (condition.hir_id, then_block.hir_id, condition.else_guards())
    }
}
