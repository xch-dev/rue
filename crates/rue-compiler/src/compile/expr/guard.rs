use log::debug;
use rue_ast::{AstGuardExpr, AstNode};
use rue_hir::Value;

use crate::{Compiler, compile_expr, compile_type};

pub fn compile_guard_expr(ctx: &mut Compiler, guard: &AstGuardExpr) -> Value {
    let ty = if let Some(ty) = guard.ty() {
        compile_type(ctx, &ty)
    } else {
        debug!("Unresolved guard type");
        ctx.builtins().unresolved.ty
    };

    let expr = if let Some(expr) = guard.expr() {
        compile_expr(ctx, &expr, Some(ty))
    } else {
        debug!("Unresolved guard expr");
        ctx.builtins().unresolved.clone()
    };

    ctx.guard_value(guard.syntax(), expr, ty)
}
