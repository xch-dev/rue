use std::collections::HashMap;

use rue_ast::{AstGuardExpr, AstNode};
use rue_hir::{Value, generate_check_hir, simplify_check};

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

    let constraint = ctx.guard_type(guard.syntax(), expr.ty, ty);
    let builtins = ctx.builtins().clone();
    let check_hir = generate_check_hir(ctx, &builtins, simplify_check(constraint.check), expr.hir);

    let mut value = Value::new(check_hir, ctx.builtins().bool);

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
