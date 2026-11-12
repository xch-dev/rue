use std::collections::HashMap;

use log::debug;
use rue_ast::{AstGuardExpr, AstNode};
use rue_hir::{Guard, Hir, Value};

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

    let constraint = ctx.guard_type(guard.syntax(), expr.ty, ty);
    let hir = ctx.alloc_hir(Hir::Guard(Guard {
        value: expr.hir,
        ty,
        check: constraint.check,
    }));

    let mut value = Value::new(hir, ctx.builtins().types.bool);

    if let Some(reference) = expr.reference {
        let mut then_map = HashMap::new();
        let mut symbol_map = HashMap::new();
        symbol_map.insert(reference.path.clone(), ty);
        then_map.insert(reference.symbol, symbol_map);

        let mut else_map = HashMap::new();
        if let Some(else_id) = constraint.else_id {
            let mut symbol_map = HashMap::new();
            symbol_map.insert(reference.path.clone(), else_id);
            else_map.insert(reference.symbol, symbol_map);
        }

        value = value.with_mappings(then_map, else_map);
    }

    value
}
