use rue_parser::{AstNode, LetStmt};

use crate::{
    compiler::Compiler,
    scope::Scope,
    symbol::{Let, Symbol},
    ScopeId,
};

impl Compiler<'_> {
    /// Compiles a let statement and returns its new scope id.
    pub fn compile_let_stmt(&mut self, let_stmt: &LetStmt) -> Option<ScopeId> {
        // Add the symbol to the stack early so you can track type references.
        let symbol_id = self.db.alloc_symbol(Symbol::Unknown);
        self.symbol_stack.push(symbol_id);

        // This doesn't default to unknown, since we want to infer the type if possible.
        let expected_type = let_stmt.ty().map(|ty| self.compile_type(ty));

        // Compile the expression.
        let value = let_stmt
            .expr()
            .map(|expr| self.compile_expr(&expr, expected_type))
            .unwrap_or(self.unknown());

        // Check that the expression's type matches the type annotation, if present.
        if let Some(expected_type) = expected_type {
            self.type_check(value.type_id, expected_type, let_stmt.syntax().text_range());
        }

        // If the name can't be resolved, there's no reason to continue compiling it.
        // We only do the above steps first to catch any other errors that may occur.
        let Some(name) = let_stmt.name() else {
            self.symbol_stack.pop().unwrap();
            return None;
        };

        *self.db.symbol_mut(symbol_id) = Symbol::Let(Let {
            type_id: expected_type.unwrap_or(value.type_id),
            hir_id: value.hir_id,
        });

        // Every let binding is a new scope for now, to ensure references are resolved in the proper order.
        let mut let_scope = Scope::default();
        let_scope.define_symbol(name.to_string(), symbol_id);
        self.db.insert_symbol_token(symbol_id, name.clone());

        let scope_id = self.db.alloc_scope(let_scope);
        self.db.insert_scope_token(scope_id, name);
        self.scope_stack.push(scope_id);
        self.symbol_stack.pop().unwrap();

        Some(scope_id)
    }
}
