use rue_parser::{AstNode, Block, Stmt};

use crate::{
    hir::{Hir, Op},
    value::Value,
    ErrorKind, TypeId,
};

use super::{stmt::Statement, Compiler};

impl Compiler<'_> {
    /// Compile a block expression into the current scope, returning the HIR and whether there was an explicit return.
    pub fn compile_block(&mut self, block: &Block, expected_type: Option<TypeId>) -> (Value, bool) {
        // Compile all of the items in the block first.
        // This means that statements can use item symbols in any order,
        // but items cannot use statement symbols.
        let items = block.items();
        let declarations = self.declare_items(&items);
        self.compile_items(&items, declarations);

        let mut statements = Vec::new();
        let mut explicit_return = false;
        let mut is_terminated = block.expr().is_some();

        for stmt in block.stmts() {
            match stmt {
                Stmt::LetStmt(let_stmt) => {
                    let Some(scope_id) = self.compile_let_stmt(&let_stmt) else {
                        continue;
                    };
                    statements.push(Statement::Let(scope_id));
                }
                Stmt::IfStmt(if_stmt) => {
                    let (condition_hir, then_hir, else_guards) =
                        self.compile_if_stmt(&if_stmt, expected_type);

                    // Push the type guards onto the stack.
                    // This will be popped in reverse order later after all statements have been lowered.
                    self.type_guard_stack.push(else_guards);

                    statements.push(Statement::If(condition_hir, then_hir));
                }
                Stmt::ReturnStmt(return_stmt) => {
                    let value = return_stmt
                        .expr()
                        .map(|expr| self.compile_expr(&expr, expected_type))
                        .unwrap_or_else(|| self.unknown());

                    // Make sure that the return value matches the expected type.
                    self.type_check(
                        value.type_id,
                        expected_type.unwrap_or(self.builtins.unknown),
                        return_stmt.syntax().text_range(),
                    );

                    explicit_return = true;
                    is_terminated = true;

                    statements.push(Statement::Return(value));
                }
                Stmt::RaiseStmt(raise_stmt) => {
                    // You can raise any value as an error, so we don't need to check the type.
                    // The value is also optional.
                    let value = raise_stmt
                        .expr()
                        .map(|expr| self.compile_expr(&expr, None).hir_id);

                    let hir_id = self.db.alloc_hir(Hir::Raise(value));

                    is_terminated = true;

                    statements.push(Statement::Return(Value::new(hir_id, self.builtins.unknown)));
                }
                Stmt::AssertStmt(assert_stmt) => {
                    // Compile the condition expression.
                    let condition = assert_stmt
                        .expr()
                        .map(|condition| self.compile_expr(&condition, Some(self.builtins.bool)))
                        .unwrap_or_else(|| self.unknown());

                    // Make sure that the condition is a boolean.
                    self.type_check(
                        condition.type_id,
                        self.builtins.bool,
                        assert_stmt.syntax().text_range(),
                    );

                    // If the condition is false, we raise an error.
                    // So we can assume that the condition is true from this point on.
                    // This will be popped in reverse order later after all statements have been lowered.
                    self.type_guard_stack.push(condition.then_guards());

                    let not_condition = self.db.alloc_hir(Hir::Op(Op::Not, condition.hir_id));
                    let raise = self.db.alloc_hir(Hir::Raise(None));

                    // We lower this down to an inverted if statement.
                    statements.push(Statement::If(not_condition, raise));
                }
                Stmt::AssumeStmt(assume_stmt) => {
                    // Compile the expression.
                    let expr = assume_stmt
                        .expr()
                        .map(|expr| self.compile_expr(&expr, Some(self.builtins.bool)))
                        .unwrap_or_else(|| self.unknown());

                    // Make sure that the condition is a boolean.
                    self.type_check(
                        expr.type_id,
                        self.builtins.bool,
                        assume_stmt.syntax().text_range(),
                    );

                    self.type_guard_stack.push(expr.then_guards());
                    statements.push(Statement::Assume);
                }
            }
        }

        // Compile the expression of the block, if present.
        let mut body = block
            .expr()
            .map(|expr| self.compile_expr(&expr, expected_type))
            .unwrap_or(self.unknown());

        // Ensure that the block terminates.
        if !is_terminated {
            self.db
                .error(ErrorKind::EmptyBlock, block.syntax().text_range());
        }

        // Pop each statement in reverse order and mutate the body.
        for statement in statements.into_iter().rev() {
            match statement {
                Statement::Let(scope_id) => {
                    body = Value::new(
                        self.db.alloc_hir(Hir::Definition(scope_id, body.hir_id)),
                        body.type_id,
                    );
                    self.scope_stack.pop().unwrap();
                }
                Statement::Return(value) => {
                    body = value;
                }
                Statement::If(condition, then_block) => {
                    self.type_guard_stack.pop().unwrap();

                    body = Value::new(
                        self.db
                            .alloc_hir(Hir::If(condition, then_block, body.hir_id)),
                        body.type_id,
                    );
                }
                Statement::Assume => {
                    self.type_guard_stack.pop().unwrap();
                }
            }
        }

        (body, explicit_return)
    }
}
