mod expr;
mod let_binding;

pub use expr::*;
pub use let_binding::*;

use rue_ast::AstStmt;
use rue_hir::Statement;

use crate::Compiler;

pub fn compile_stmt(ctx: &mut Compiler, stmt: &AstStmt) -> Statement {
    match stmt {
        AstStmt::ExprStmt(expr) => compile_expr_stmt(ctx, expr),
        AstStmt::LetStmt(let_stmt) => compile_let_binding(ctx, let_stmt),
    }
}
