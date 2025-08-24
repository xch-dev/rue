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
        ctx.assign_type(condition.syntax(), value.ty, ctx.builtins().bool);
        value
    } else {
        ctx.builtins().unresolved.clone()
    };

    let then_expr = if let Some(then_expr) = expr.then_expr() {
        let index = ctx.push_mappings(condition.then_map.clone());
        let value = compile_expr(ctx, &then_expr, expected_type);
        ctx.revert_mappings(index);
        value
    } else {
        ctx.builtins().unresolved.clone()
    };

    let else_expr = if let Some(else_expr) = expr.else_expr() {
        let index = ctx.push_mappings(condition.else_map.clone());
        let value = compile_expr(ctx, &else_expr, expected_type);
        ctx.revert_mappings(index);
        value
    } else {
        ctx.builtins().unresolved.clone()
    };

    let ty = ctx.alloc_type(Type::Union(Union::new(vec![then_expr.ty, else_expr.ty])));
    let hir = ctx.alloc_hir(Hir::If(condition.hir, then_expr.hir, else_expr.hir));

    Value::new(hir, ty)
}
