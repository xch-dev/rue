use rue_parser::{AstNode, CastExpr};

use crate::{compiler::Compiler, ty::Value, TypeId};

impl Compiler<'_> {
    pub fn compile_cast_expr(&mut self, cast: &CastExpr, expected_type: Option<TypeId>) -> Value {
        let Some(expr) = cast
            .expr()
            .map(|expr| self.compile_expr(&expr, expected_type))
        else {
            return self.unknown();
        };

        let ty = cast
            .ty()
            .map_or(self.builtins.unknown, |ty| self.compile_type(ty));

        self.cast_check(expr.type_id, ty, cast.expr().unwrap().syntax().text_range());

        Value::new(expr.hir_id, ty)
    }
}
