use rue_parser::GroupExpr;
use rue_typing::TypeId;

use crate::{Compiler, Value};

impl Compiler<'_> {
    pub fn compile_group_expr(
        &mut self,
        group_expr: &GroupExpr,
        expected_type: Option<TypeId>,
    ) -> Value {
        // Compile the inner expression, or return unknown if it's missing.
        // This would be a parser error, so no diagnostic is needed.
        group_expr
            .expr()
            .map(|expr| self.compile_expr(&expr, expected_type))
            .unwrap_or_else(|| self.unknown())
    }
}
