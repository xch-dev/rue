mod binary;
mod function_call;
mod group;
mod literal;
mod prefix;

pub use binary::*;
pub use function_call::*;
pub use group::*;
pub use literal::*;
pub use prefix::*;

use crate::{AstExpr, Context, Value};

pub fn compile_expr(ctx: &mut Context, expr: &AstExpr) -> Value {
    match expr {
        AstExpr::LiteralExpr(expr) => compile_literal_expr(ctx, expr),
        AstExpr::GroupExpr(expr) => compile_group_expr(ctx, expr),
        AstExpr::PrefixExpr(expr) => compile_prefix_expr(ctx, expr),
        AstExpr::BinaryExpr(expr) => compile_binary_expr(ctx, expr),
        AstExpr::FunctionCallExpr(expr) => compile_function_call_expr(ctx, expr),
    }
}
