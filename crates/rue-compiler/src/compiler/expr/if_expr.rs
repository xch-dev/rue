use rue_parser::{AstNode, IfExpr};
use rue_typing::TypeId;

use crate::{compiler::Compiler, hir::Hir, value::Value};

impl Compiler<'_> {
    pub fn compile_if_expr(&mut self, if_expr: &IfExpr, expected_type: Option<TypeId>) -> Value {
        let condition = if_expr
            .condition()
            .map(|condition| self.compile_expr(&condition, Some(self.ty.std().bool)));

        if let Some(condition) = condition.as_ref() {
            let overrides = self.build_overrides(condition.then_guards());
            self.type_overrides.push(overrides);
        }

        let then_block = if_expr
            .then_block()
            .map(|then_block| self.compile_block_expr(&then_block, expected_type));

        if condition.is_some() {
            self.type_overrides.pop().unwrap();
        }

        if let Some(condition) = condition.as_ref() {
            let overrides = self.build_overrides(condition.else_guards());
            self.type_overrides.push(overrides);
        }

        let expected_type =
            expected_type.or_else(|| then_block.as_ref().map(|then_block| then_block.type_id));

        let else_block = if_expr
            .else_block()
            .map(|else_block| self.compile_block_expr(&else_block, expected_type));

        if condition.is_some() {
            self.type_overrides.pop().unwrap();
        }

        if let Some(condition_type) = condition.as_ref().map(|condition| condition.type_id) {
            self.type_check(
                condition_type,
                self.ty.std().bool,
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
            .map_or(self.ty.std().unknown, |block| block.type_id);

        let value = condition.and_then(|condition| {
            then_block.and_then(|then_block| {
                else_block.map(|else_block| {
                    self.db.alloc_hir(Hir::If(
                        condition.hir_id,
                        then_block.hir_id,
                        else_block.hir_id,
                    ))
                })
            })
        });

        Value::new(value.unwrap_or(self.builtins.unknown), ty)
    }
}
