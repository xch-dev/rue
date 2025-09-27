use log::debug;
use rue_ast::{AstAssertStmt, AstNode};
use rue_hir::Statement;

use crate::{Compiler, compile_expr};

pub fn compile_assert_stmt(ctx: &mut Compiler, stmt: &AstAssertStmt) -> Statement {
    let value = if let Some(expr) = stmt.expr() {
        let value = compile_expr(ctx, &expr, None);
        ctx.check_condition(expr.syntax(), value.ty);
        value
    } else {
        debug!("Unresolved assert expr");
        ctx.builtins().unresolved.clone()
    };

    ctx.push_mappings(value.then_map);

    Statement::Assert(value.hir)
}
