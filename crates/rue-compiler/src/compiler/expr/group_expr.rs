use rue_parser::GroupExpr;

use crate::{compiler::Compiler, ty::Value, TypeId};

impl Compiler<'_> {
    pub fn compile_group_expr(
        &mut self,
        group_expr: &GroupExpr,
        expected_type: Option<TypeId>,
    ) -> Value {
        let Some(expr) = group_expr
            .expr()
            .map(|expr| self.compile_expr(&expr, expected_type))
        else {
            return self.unknown();
        };

        expr
    }
}
