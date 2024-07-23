use rue_parser::{AstNode, CastExpr};

use crate::{compiler::Compiler, value::Value};

impl Compiler<'_> {
    pub fn compile_cast_expr(&mut self, cast: &CastExpr) -> Value {
        // It's fine to default to unknown, since the cast check will succeed anyways.
        let type_id = cast
            .ty()
            .map_or(self.ty.std().unknown, |ty| self.compile_type(ty));

        // Let's used the cast type as the expected type.
        let Some(expr) = cast
            .expr()
            .map(|expr| self.compile_expr(&expr, Some(type_id)))
        else {
            return self.unknown();
        };

        // Make sure that the cast is valid, but do it even if not to make the rest of the type checking succeed.
        self.cast_check(
            expr.type_id,
            type_id,
            cast.expr().unwrap().syntax().text_range(),
        );

        // Change the type of the value. We throw away type guards here.
        Value::new(expr.hir_id, type_id)
    }
}
