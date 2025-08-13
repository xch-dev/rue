use rue_ast::AstAssertStmt;
use rue_hir::Statement;

use crate::{Compiler, compile_expr};

pub fn compile_assert_stmt(ctx: &mut Compiler, stmt: &AstAssertStmt) -> Statement {
    let value = if let Some(expr) = stmt.expr() {
        compile_expr(ctx, &expr)
    } else {
        ctx.builtins().unresolved.clone()
    };

    Statement::Assert(value.hir)
}
