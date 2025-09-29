mod binary;
mod cast;
mod field_access;
mod function_call;
mod group;
mod guard;
mod if_expr;
mod lambda;
mod list;
mod literal;
mod pair;
mod path;
mod prefix;
mod struct_initializer;

pub use binary::*;
pub use cast::*;
pub use field_access::*;
pub use function_call::*;
pub use group::*;
pub use guard::*;
pub use if_expr::*;
pub use lambda::*;
pub use list::*;
pub use literal::*;
pub use pair::*;
pub use path::*;
pub use prefix::*;
pub use struct_initializer::*;

use rue_ast::AstExpr;
use rue_hir::Value;
use rue_types::TypeId;

use crate::{Compiler, compile_block};

pub fn compile_expr(ctx: &mut Compiler, expr: &AstExpr, expected_type: Option<TypeId>) -> Value {
    match expr {
        AstExpr::PathExpr(expr) => compile_path_expr(ctx, expr),
        AstExpr::StructInitializerExpr(expr) => compile_struct_initializer_expr(ctx, expr),
        AstExpr::LiteralExpr(expr) => compile_literal_expr(ctx, expr),
        AstExpr::GroupExpr(expr) => compile_group_expr(ctx, expr, expected_type),
        AstExpr::PairExpr(expr) => compile_pair_expr(ctx, expr, expected_type),
        AstExpr::ListExpr(expr) => compile_list_expr(ctx, expr, expected_type),
        AstExpr::PrefixExpr(expr) => compile_prefix_expr(ctx, expr),
        AstExpr::BinaryExpr(expr) => compile_binary_expr(ctx, expr),
        AstExpr::FunctionCallExpr(expr) => compile_function_call_expr(ctx, expr),
        AstExpr::IfExpr(expr) => compile_if_expr(ctx, expr, expected_type),
        AstExpr::GuardExpr(expr) => compile_guard_expr(ctx, expr),
        AstExpr::CastExpr(expr) => compile_cast_expr(ctx, expr),
        AstExpr::FieldAccessExpr(expr) => compile_field_access_expr(ctx, expr),
        AstExpr::LambdaExpr(expr) => compile_lambda_expr(ctx, expr, expected_type),
        AstExpr::Block(block) => compile_block(ctx, block, true, expected_type, true),
    }
}
