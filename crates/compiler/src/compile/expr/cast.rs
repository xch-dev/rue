use rue_ast::{AstCastExpr, AstNode};
use rue_hir::Value;

use crate::{Compiler, compile_expr, compile_type};

pub fn compile_cast_expr(ctx: &mut Compiler, cast: &AstCastExpr) -> Value {
    let expr = if let Some(expr) = cast.expr() {
        compile_expr(ctx, &expr)
    } else {
        ctx.builtins().unresolved.clone()
    };

    let ty = if let Some(ty) = cast.ty() {
        compile_type(ctx, &ty)
    } else {
        ctx.builtins().unresolved.ty
    };

    ctx.cast_type(cast.syntax(), expr.hir, expr.ty, ty);

    Value::new(expr.hir, ty)
}
