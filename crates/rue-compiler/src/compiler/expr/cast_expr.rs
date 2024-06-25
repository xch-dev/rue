use rue_parser::{AstNode, CastExpr};

use crate::{compiler::Compiler, value::Value};

impl Compiler<'_> {
    pub fn compile_cast_expr(&mut self, cast: &CastExpr) -> Value {
        // There is no expected type for the left hand side, since it's changed with a cast.
        let Some(expr) = cast.expr().map(|expr| self.compile_expr(&expr, None)) else {
            return self.unknown();
        };

        // It's fine to default to unknown, since the cast check will succeed anyways.
        let type_id = cast
            .ty()
            .map_or(self.builtins.unknown, |ty| self.compile_type(ty));

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
