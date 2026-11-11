use log::debug;
use rue_ast::{AstCastExpr, AstNode};
use rue_hir::{Hir, Value};

use crate::{Compiler, compile_expr, compile_type};

pub fn compile_cast_expr(ctx: &mut Compiler, cast: &AstCastExpr) -> Value {
    let ty = if let Some(ty) = cast.ty() {
        compile_type(ctx, &ty)
    } else {
        debug!("Unresolved cast type");
        ctx.builtins().unresolved.ty
    };

    let expr = if let Some(expr) = cast.expr() {
        compile_expr(ctx, &expr, Some(ty))
    } else {
        debug!("Unresolved cast expr");
        ctx.builtins().unresolved.clone()
    };

    ctx.cast_type(cast.syntax(), expr.ty, ty);

    // The cast keeps the value's reference and type guard mappings, since it's the same value
    let hir = ctx.alloc_hir(Hir::Cast(expr.hir));
    expr.with_hir(hir).with_type(ty)
}
