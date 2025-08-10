mod binary;
mod function_call;
mod group;
mod literal;
mod path;
mod prefix;

pub use binary::*;
pub use function_call::*;
pub use group::*;
pub use literal::*;
pub use path::*;
pub use prefix::*;

use rue_ast::AstExpr;
use rue_hir::Value;

use crate::Context;

pub fn compile_expr(ctx: &mut Context, expr: &AstExpr) -> Value {
    match expr {
        AstExpr::PathExpr(expr) => compile_path_expr(ctx, expr),
        AstExpr::LiteralExpr(expr) => compile_literal_expr(ctx, expr),
        AstExpr::GroupExpr(expr) => compile_group_expr(ctx, expr),
        AstExpr::PrefixExpr(expr) => compile_prefix_expr(ctx, expr),
        AstExpr::BinaryExpr(expr) => compile_binary_expr(ctx, expr),
        AstExpr::FunctionCallExpr(expr) => compile_function_call_expr(ctx, expr),
    }
}
