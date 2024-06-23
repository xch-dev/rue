use rue_parser::{AstNode, IfExpr};

use crate::{compiler::Compiler, hir::Hir, ty::Value, TypeId};

impl Compiler<'_> {
    pub fn compile_if_expr(&mut self, if_expr: &IfExpr, expected_type: Option<TypeId>) -> Value {
        let condition = if_expr
            .condition()
            .map(|condition| self.compile_expr(&condition, Some(self.builtins.bool)));

        if let Some(condition) = condition.as_ref() {
            self.type_guard_stack.push(condition.then_guards());
        }

        let then_block = if_expr
            .then_block()
            .map(|then_block| self.compile_block_expr(&then_block, expected_type));

        if condition.is_some() {
            self.type_guard_stack.pop().unwrap();
        }

        if let Some(condition) = condition.as_ref() {
            self.type_guard_stack.push(condition.else_guards());
        }

        let expected_type =
            expected_type.or_else(|| then_block.as_ref().map(|then_block| then_block.type_id));

        let else_block = if_expr
            .else_block()
            .map(|else_block| self.compile_block_expr(&else_block, expected_type));

        if condition.is_some() {
            self.type_guard_stack.pop().unwrap();
        }

        if let Some(condition_type) = condition.as_ref().map(|condition| condition.type_id) {
            self.type_check(
                condition_type,
                self.builtins.bool,
                if_expr.condition().unwrap().syntax().text_range(),
            );
        }

        if let (Some(then_block), Some(else_block)) = (&then_block, &else_block) {
            self.type_check(
                else_block.type_id,
                then_block.type_id,
                if_expr.else_block().unwrap().syntax().text_range(),
            );
        }

        let ty = then_block
            .as_ref()
            .or(else_block.as_ref())
            .map_or(self.builtins.unknown, |block| block.type_id);

        let value = condition.and_then(|condition| {
            then_block.and_then(|then_block| {
                else_block.map(|else_block| {
                    self.db.alloc_hir(Hir::If {
                        condition: condition.hir_id,
                        then_block: then_block.hir_id,
                        else_block: else_block.hir_id,
                    })
                })
            })
        });

        Value::new(value.unwrap_or(self.builtins.unknown_hir), ty)
    }
}
