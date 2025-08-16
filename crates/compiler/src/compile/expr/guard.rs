use rue_ast::{AstGuardExpr, AstNode};
use rue_hir::Value;

use crate::{Compiler, compile_expr, compile_type};

pub fn compile_guard_expr(ctx: &mut Compiler, guard: &AstGuardExpr) -> Value {
    let expr = if let Some(expr) = guard.expr() {
        compile_expr(ctx, &expr)
    } else {
        ctx.builtins().unresolved.clone()
    };

    let ty = if let Some(ty) = guard.ty() {
        compile_type(ctx, &ty)
    } else {
        ctx.builtins().unresolved.ty
    };

    let constraint = ctx.guard_type(guard.syntax(), expr.hir, expr.ty, ty);

    Value::with_mappings(
        constraint.hir,
        ctx.builtins().bool,
        constraint.then_map,
        constraint.else_map,
    )
}
