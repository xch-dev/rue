use rue_parser::{AstNode, CastExpr};

use crate::{compiler::Compiler, ty::Value};

impl Compiler<'_> {
    pub fn compile_cast_expr(&mut self, cast: &CastExpr) -> Value {
        let Some(expr) = cast.expr().map(|expr| self.compile_expr(&expr, None)) else {
            return self.unknown();
        };

        let type_id = cast
            .ty()
            .map_or(self.builtins.unknown, |ty| self.compile_type(ty));

        self.cast_check(
            expr.type_id,
            type_id,
            cast.expr().unwrap().syntax().text_range(),
        );

        Value::new(expr.hir_id, type_id)
    }
}
