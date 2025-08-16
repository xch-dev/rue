use std::collections::HashMap;

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

    let hir = ctx.guard_type(guard.syntax(), expr.hir, expr.ty, ty);

    let mut then_map = HashMap::new();
    then_map.insert(expr.ty, ty);

    // TODO: Else map
    Value::with_mappings(hir, ctx.builtins().bool, then_map, HashMap::new())
}
