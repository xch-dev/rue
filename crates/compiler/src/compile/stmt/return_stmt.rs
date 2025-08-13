use rue_ast::AstReturnStmt;
use rue_hir::Statement;

use crate::{Compiler, compile_expr};

pub fn compile_return_stmt(ctx: &mut Compiler, stmt: &AstReturnStmt) -> Statement {
    let value = if let Some(expr) = stmt.expr() {
        compile_expr(ctx, &expr)
    } else {
        ctx.builtins().unresolved.clone()
    };

    Statement::Return(value.hir)
}
