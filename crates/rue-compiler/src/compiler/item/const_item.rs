use rue_parser::{AstNode, ConstItem};

use crate::{
    compiler::Compiler,
    hir::Hir,
    symbol::{Const, Symbol},
    SymbolId,
};

impl Compiler<'_> {
    /// Define a constant in the current scope, but don't lower its body.
    pub fn declare_const_item(&mut self, const_item: &ConstItem) -> SymbolId {
        // Add the symbol to the stack early so you can track type references.
        let symbol_id = self.db.alloc_symbol(Symbol::Unknown);
        self.symbol_stack.push(symbol_id);

        let type_id = const_item
            .ty()
            .map_or(self.builtins.unknown, |ty| self.compile_type(ty));

        let hir_id = self.db.alloc_hir(Hir::Unknown);

        if const_item.inline().is_some() {
            *self.db.symbol_mut(symbol_id) = Symbol::InlineConst(Const { type_id, hir_id });
        } else {
            *self.db.symbol_mut(symbol_id) = Symbol::Const(Const { type_id, hir_id });
        }

        if let Some(name) = const_item.name() {
            self.scope_mut().define_symbol(name.to_string(), symbol_id);
            self.db.insert_symbol_token(symbol_id, name);
        }

        self.symbol_stack.pop().unwrap()
    }

    /// Compiles a constant's value.
    pub fn compile_const_item(&mut self, const_item: &ConstItem, symbol_id: SymbolId) {
        let Some(expr) = const_item.expr() else {
            return;
        };

        let (Symbol::Const(Const { type_id, .. }) | Symbol::InlineConst(Const { type_id, .. })) =
            self.db.symbol(symbol_id).clone()
        else {
            unreachable!();
        };

        let output = self.compile_expr(&expr, Some(type_id));

        // Ensure that the expression is assignable to the constant's type.
        self.type_check(output.type_id, type_id, const_item.syntax().text_range());

        // We ignore type guards here for now.
        // Just set the constant HIR.
        let (Symbol::Const(Const { hir_id, .. }) | Symbol::InlineConst(Const { hir_id, .. })) =
            self.db.symbol_mut(symbol_id)
        else {
            unreachable!();
        };
        *hir_id = output.hir_id;
    }
}
