mod assert;
mod expr;
mod if_stmt;
mod let_binding;
mod raise;
mod return_stmt;

pub use assert::*;
pub use expr::*;
pub use if_stmt::*;
pub use let_binding::*;
pub use raise::*;
pub use return_stmt::*;

use rue_ast::AstStmt;
use rue_hir::Statement;
use rue_types::TypeId;

use crate::Compiler;

pub fn compile_stmt(
    ctx: &mut Compiler,
    stmt: &AstStmt,
    expected_type: Option<TypeId>,
) -> Statement {
    match stmt {
        AstStmt::ExprStmt(expr) => compile_expr_stmt(ctx, expr),
        AstStmt::LetStmt(let_stmt) => compile_let_binding(ctx, let_stmt),
        AstStmt::IfStmt(if_stmt) => compile_if_stmt(ctx, if_stmt, expected_type),
        AstStmt::ReturnStmt(return_stmt) => compile_return_stmt(ctx, return_stmt, expected_type),
        AstStmt::AssertStmt(assert_stmt) => compile_assert_stmt(ctx, assert_stmt),
        AstStmt::RaiseStmt(raise_stmt) => compile_raise_stmt(ctx, raise_stmt),
    }
}
