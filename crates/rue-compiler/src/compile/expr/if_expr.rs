use log::debug;
use rue_ast::{AstIfExpr, AstNode};
use rue_hir::{Hir, Value};
use rue_types::{Type, TypeId, Union};

use crate::{Compiler, compile_expr};

pub fn compile_if_expr(
    ctx: &mut Compiler,
    expr: &AstIfExpr,
    expected_type: Option<TypeId>,
) -> Value {
    let condition = if let Some(condition) = expr.condition() {
        let value = compile_expr(ctx, &condition, None);
        ctx.check_condition(condition.syntax(), value.ty);
        value
    } else {
        debug!("Unresolved if condition");
        ctx.builtins().unresolved.clone()
    };

    let then_expr = if let Some(then_expr) = expr.then_expr() {
        if expr.inline().is_some() {
            // We can't type guard if the branches are eagerly evaluated
            compile_expr(ctx, &then_expr, expected_type)
        } else {
            let range = then_expr.syntax().text_range();
            let index = ctx.push_mappings(condition.then_map.clone(), range.start());
            let value = compile_expr(ctx, &then_expr, expected_type);
            ctx.revert_mappings(index, range.end());
            value
        }
    } else {
        debug!("Unresolved if then expr");
        ctx.builtins().unresolved.clone()
    };

    let else_expr = if let Some(else_expr) = expr.else_expr() {
        if expr.inline().is_some() {
            // We can't type guard if the branches are eagerly evaluated
            compile_expr(ctx, &else_expr, expected_type)
        } else {
            let range = else_expr.syntax().text_range();
            let index = ctx.push_mappings(condition.else_map.clone(), range.start());
            let value = compile_expr(ctx, &else_expr, expected_type);
            ctx.revert_mappings(index, range.end());
            value
        }
    } else {
        debug!("Unresolved if else expr");
        ctx.builtins().unresolved.clone()
    };

    let ty = ctx.alloc_type(Type::Union(Union::new(vec![then_expr.ty, else_expr.ty])));
    let hir = ctx.alloc_hir(Hir::If(
        condition.hir,
        then_expr.hir,
        else_expr.hir,
        expr.inline().is_some(),
    ));

    Value::new(hir, ty)
}
