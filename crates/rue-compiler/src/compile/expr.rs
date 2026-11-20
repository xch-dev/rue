mod binary;
mod builtin;
mod call_inference;
mod cast;
mod const_expr;
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
pub(crate) use call_inference::{DeferredLambda, ElementLambda};
pub use cast::*;
pub use const_expr::*;
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

use rue_ast::{AstExpr, AstFunctionCallExpr, AstLambdaExpr, AstNode};
use rue_hir::Value;
use rue_types::TypeId;

use crate::{BlockKind, Compiler, CompletionContext, SyntaxItemKind, compile_block};

pub fn compile_expr(ctx: &mut Compiler, expr: &AstExpr, expected_type: Option<TypeId>) -> Value {
    ctx.add_syntax(
        SyntaxItemKind::CompletionContext(CompletionContext::Expression),
        expr.syntax().text_range(),
    );

    let value = match expr {
        AstExpr::PathExpr(expr) => compile_path_expr(ctx, expr),
        AstExpr::StructInitializerExpr(expr) => compile_struct_initializer_expr(ctx, expr),
        AstExpr::LiteralExpr(expr) => compile_literal_expr(ctx, expr),
        AstExpr::ConstExpr(expr) => compile_const_expr(ctx, expr, expected_type),
        AstExpr::GroupExpr(expr) => compile_group_expr(ctx, expr, expected_type),
        AstExpr::PairExpr(expr) => compile_pair_expr(ctx, expr, expected_type),
        AstExpr::ListExpr(expr) => compile_list_expr(ctx, expr, expected_type),
        AstExpr::PrefixExpr(expr) => compile_prefix_expr(ctx, expr),
        AstExpr::BinaryExpr(expr) => compile_binary_expr(ctx, expr),
        AstExpr::FunctionCallExpr(expr) => {
            compile_function_call_expr(ctx, expr, expected_type, None)
        }
        AstExpr::IfExpr(expr) => compile_if_expr(ctx, expr, expected_type),
        AstExpr::GuardExpr(expr) => compile_guard_expr(ctx, expr),
        AstExpr::CastExpr(expr) => compile_cast_expr(ctx, expr),
        AstExpr::FieldAccessExpr(expr) => compile_field_access_expr(ctx, expr),
        AstExpr::LambdaExpr(expr) => compile_lambda_expr(ctx, expr, expected_type),
        AstExpr::Block(block) => compile_block(ctx, block, BlockKind::Expr, expected_type, true),
    };

    ctx.record_value(expr.syntax(), value)
}

/// Compiles a call that's an argument of a call whose generics are being inferred, as
/// `compile_expr` does, with a narrower type expected of it than `expected_type`, which its generics
/// that nothing else gives a type take (see `ArgumentOrder::widens_to_expected`).
pub(crate) fn compile_call_argument(
    ctx: &mut Compiler,
    expr: &AstExpr,
    call: &AstFunctionCallExpr,
    expected_type: Option<TypeId>,
    narrower: Option<TypeId>,
) -> Value {
    ctx.add_syntax(
        SyntaxItemKind::CompletionContext(CompletionContext::Expression),
        expr.syntax().text_range(),
    );

    let value = compile_function_call_expr(ctx, call, expected_type, narrower);
    ctx.record_value(expr.syntax(), value)
}

/// Compiles a lambda that's an argument of a call whose generics are being inferred, as
/// `compile_expr` does, with its untyped parameters compiled as references (see `compile_lambda`).
pub(crate) fn compile_lambda_argument(
    ctx: &mut Compiler,
    expr: &AstExpr,
    lambda: &AstLambdaExpr,
    expected_type: Option<TypeId>,
    wrap: &ParameterWrap<'_>,
) -> (Value, Vec<LambdaParameter>) {
    ctx.add_syntax(
        SyntaxItemKind::CompletionContext(CompletionContext::Expression),
        expr.syntax().text_range(),
    );

    let (value, parameters) = compile_lambda(ctx, lambda, expected_type, Some(wrap));
    (ctx.record_value(expr.syntax(), value), parameters)
}
