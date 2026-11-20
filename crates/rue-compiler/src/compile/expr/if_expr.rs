use log::debug;
use rue_ast::{AstExpr, AstIfExpr, AstNode};
use rue_hir::{Hir, Value, control_terminates};
use rue_types::{Type, TypeId, Union};

use crate::{BlockKind, Compiler, compile_expr};

/// Compiles an `if` expression. If its branches are blocks of a given kind (see
/// [`Compiler::with_branches`]), such as the branches of a statement, so are those of the `else if`
/// expressions after it.
pub fn compile_if_expr(
    ctx: &mut Compiler,
    expr: &AstIfExpr,
    expected_type: Option<TypeId>,
    branches: Option<BlockKind>,
) -> Value {
    let condition = if let Some(condition) = expr.condition() {
        let value = compile_expr(ctx, &condition, None);
        ctx.check_condition(condition.syntax(), value.ty);
        value
    } else {
        debug!("Unresolved if condition");
        ctx.unresolved()
    };

    let then_expr = if let Some(then_expr) = expr.then_expr() {
        if expr.inline().is_some() {
            // We can't type guard if the branches are eagerly evaluated
            compile_branch(ctx, &then_expr, expected_type, branches)
        } else {
            let range = then_expr.syntax().text_range();
            let index = ctx.push_mappings(condition.then_map.clone(), range.start());
            let value = compile_branch(ctx, &then_expr, expected_type, branches);
            ctx.revert_mappings(index, range.end());
            value
        }
    } else {
        debug!("Unresolved if then expr");
        ctx.unresolved()
    };

    let else_expr = if let Some(else_expr) = expr.else_expr() {
        if expr.inline().is_some() {
            // We can't type guard if the branches are eagerly evaluated
            compile_branch(ctx, &else_expr, expected_type, branches)
        } else {
            let range = else_expr.syntax().text_range();
            let index = ctx.push_mappings(condition.else_map.clone(), range.start());
            let value = compile_branch(ctx, &else_expr, expected_type, branches);
            ctx.revert_mappings(index, range.end());
            value
        }
    } else {
        debug!("Unresolved if else expr");
        ctx.unresolved()
    };

    // An `else if` statement records what the rest of the block sees after it
    let else_narrowing = ctx.take_statement_narrowing();

    // The rest of the block after a statement only sees the values that the branch which falls
    // through to it does, if only one of them does. An `else if` chain that only falls through in
    // its last branch is like a series of `if` statements that exit early.
    if branches == Some(BlockKind::Statement) {
        let narrowing = if expr.inline().is_some() {
            None
        } else {
            match (
                control_terminates(ctx, then_expr.hir),
                control_terminates(ctx, else_expr.hir),
            ) {
                (true, false) => Some(match else_narrowing {
                    Some(else_narrowing) => {
                        ctx.merge_mappings(&condition.else_map, &else_narrowing)
                    }
                    None => condition.else_map.clone(),
                }),
                (false, true) => Some(condition.then_map.clone()),
                _ => None,
            }
        };

        ctx.set_statement_narrowing(narrowing);
    }

    let ty = ctx.alloc_type(Type::Union(Union::new(vec![then_expr.ty, else_expr.ty])));
    let hir = ctx.alloc_hir(Hir::If(
        condition.hir,
        then_expr.hir,
        else_expr.hir,
        expr.inline().is_some(),
    ));

    Value::new(hir, ty)
}

fn compile_branch(
    ctx: &mut Compiler,
    expr: &AstExpr,
    expected_type: Option<TypeId>,
    branches: Option<BlockKind>,
) -> Value {
    if let Some(kind) = branches {
        ctx.with_branches(kind, |ctx| compile_expr(ctx, expr, expected_type))
    } else {
        compile_expr(ctx, expr, expected_type)
    }
}
