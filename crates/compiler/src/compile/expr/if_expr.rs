use rue_ast::{AstIfExpr, AstNode};
use rue_hir::{Hir, Type, Value};

use crate::{Compiler, compile_expr};

pub fn compile_if_expr(ctx: &mut Compiler, expr: &AstIfExpr) -> Value {
    let condition = if let Some(condition) = expr.condition() {
        let value = compile_expr(ctx, &condition);
        ctx.assign_type(condition.syntax(), value.hir, value.ty, ctx.builtins().bool);
        value
    } else {
        ctx.builtins().unresolved.clone()
    };

    let then_expr = if let Some(then_expr) = expr.then_expr() {
        let index = ctx.push_mappings(condition.then_map.clone());
        let value = compile_expr(ctx, &then_expr);
        ctx.revert_mappings(index);
        value
    } else {
        ctx.builtins().unresolved.clone()
    };

    let else_expr = if let Some(else_expr) = expr.else_expr() {
        let index = ctx.push_mappings(condition.else_map.clone());
        let value = compile_expr(ctx, &else_expr);
        ctx.revert_mappings(index);
        value
    } else {
        ctx.builtins().unresolved.clone()
    };

    let ty = ctx.alloc_type(Type::Union(vec![then_expr.ty, else_expr.ty]));
    let hir = ctx.alloc_hir(Hir::If(condition.hir, then_expr.hir, else_expr.hir));

    Value::new(hir, ty)
}
