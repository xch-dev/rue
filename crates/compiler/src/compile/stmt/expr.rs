use rue_ast::{AstExprStmt, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::Statement;

use crate::{Compiler, compile_expr};

pub fn compile_expr_stmt(ctx: &mut Compiler, stmt: &AstExprStmt) -> Statement {
    let value = if let Some(expr) = stmt.expr() {
        compile_expr(ctx, &expr)
    } else {
        ctx.builtins().unresolved.clone()
    };

    ctx.diagnostic(stmt.syntax(), DiagnosticKind::InvalidExpressionStatement);

    Statement::Expr(value.hir)
}
