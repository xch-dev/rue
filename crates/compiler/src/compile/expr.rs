mod binary;
mod cast;
mod field_access;
mod function_call;
mod group;
mod guard;
mod if_expr;
mod literal;
mod pair;
mod path;
mod prefix;

pub use binary::*;
pub use cast::*;
pub use field_access::*;
pub use function_call::*;
pub use group::*;
pub use guard::*;
pub use if_expr::*;
pub use literal::*;
pub use pair::*;
pub use path::*;
pub use prefix::*;

use rue_ast::AstExpr;
use rue_hir::Value;

use crate::{Compiler, compile_block};

pub fn compile_expr(ctx: &mut Compiler, expr: &AstExpr) -> Value {
    match expr {
        AstExpr::PathExpr(expr) => compile_path_expr(ctx, expr),
        AstExpr::LiteralExpr(expr) => compile_literal_expr(ctx, expr),
        AstExpr::GroupExpr(expr) => compile_group_expr(ctx, expr),
        AstExpr::PairExpr(expr) => compile_pair_expr(ctx, expr),
        AstExpr::PrefixExpr(expr) => compile_prefix_expr(ctx, expr),
        AstExpr::BinaryExpr(expr) => compile_binary_expr(ctx, expr),
        AstExpr::FunctionCallExpr(expr) => compile_function_call_expr(ctx, expr),
        AstExpr::IfExpr(expr) => compile_if_expr(ctx, expr),
        AstExpr::GuardExpr(expr) => compile_guard_expr(ctx, expr),
        AstExpr::CastExpr(expr) => compile_cast_expr(ctx, expr),
        AstExpr::FieldAccessExpr(expr) => compile_field_access_expr(ctx, expr),
        AstExpr::Block(block) => compile_block(ctx, block, true),
    }
}
