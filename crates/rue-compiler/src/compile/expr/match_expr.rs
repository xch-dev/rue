use rue_ast::AstMatchExpr;
use rue_hir::Value;
use rue_types::TypeId;

use crate::Compiler;

pub fn compile_match_expr(
    _ctx: &mut Compiler,
    _expr: &AstMatchExpr,
    _expected_type: Option<TypeId>,
) -> Value {
    todo!()
}
