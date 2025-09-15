use log::debug;
use rue_ast::AstRaiseStmt;
use rue_hir::Statement;

use crate::{Compiler, compile_expr};

pub fn compile_raise_stmt(ctx: &mut Compiler, stmt: &AstRaiseStmt) -> Statement {
    let value = if let Some(expr) = stmt.expr() {
        compile_expr(ctx, &expr, None)
    } else {
        debug!("Unresolved raise stmt expr");
        ctx.builtins().unresolved.clone()
    };

    Statement::Raise(value.hir)
}
