use rue_ast::AstReturnStmt;
use rue_hir::Statement;
use rue_types::TypeId;

use crate::{Compiler, compile_expr};

pub fn compile_return_stmt(
    ctx: &mut Compiler,
    stmt: &AstReturnStmt,
    expected_type: Option<TypeId>,
) -> Statement {
    let value = if let Some(expr) = stmt.expr() {
        compile_expr(ctx, &expr, expected_type)
    } else {
        ctx.builtins().unresolved.clone()
    };

    Statement::Return(value.hir)
}
