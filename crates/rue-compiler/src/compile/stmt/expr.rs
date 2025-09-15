use log::debug;
use rue_ast::AstExprStmt;
use rue_hir::Statement;

use crate::{Compiler, compile_expr};

pub fn compile_expr_stmt(ctx: &mut Compiler, stmt: &AstExprStmt) -> Statement {
    let value = if let Some(expr) = stmt.expr() {
        compile_expr(ctx, &expr, None)
    } else {
        debug!("Unresolved expr stmt expr");
        ctx.builtins().unresolved.clone()
    };

    Statement::Expr(value.hir)
}
