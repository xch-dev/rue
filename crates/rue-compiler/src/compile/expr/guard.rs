use log::debug;
use rue_ast::{AstGuardExpr, AstNode};
use rue_hir::{Guard, Hir, Value};

use crate::{Compiler, compile_expr, compile_type};

pub fn compile_guard_expr(ctx: &mut Compiler, guard: &AstGuardExpr) -> Value {
    let ty = if let Some(ty) = guard.ty() {
        compile_type(ctx, &ty)
    } else {
        debug!("Unresolved guard type");
        ctx.unresolved().ty
    };

    let expr = if let Some(expr) = guard.expr() {
        // The guard's type isn't expected of the value, since it's what the value is checked
        // against. Otherwise it would infer a call's generics, such as `T` in
        // `unchecked_cast(value) is Bytes32`, which leaves nothing to check.
        compile_expr(ctx, &expr, None)
    } else {
        debug!("Unresolved guard expr");
        ctx.unresolved()
    };

    let constraint = ctx.guard_type(guard.syntax(), expr.ty, ty);
    let hir = ctx.alloc_hir(Hir::Guard(Guard {
        value: expr.hir,
        ty,
        check: constraint.check,
    }));

    let mut value = Value::new(hir, ctx.builtins().types.bool);

    if let Some(reference) = expr.reference {
        let then_map = reference.narrowed_to(ty);
        let else_map = constraint
            .else_id
            .map(|else_id| reference.narrowed_to(else_id))
            .unwrap_or_default();

        value = value.with_mappings(then_map, else_map);
    }

    value
}
