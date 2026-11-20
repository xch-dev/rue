use log::debug;
use rue_ast::{AstExpr, AstIfExpr, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Block, Hir, Mappings, Value, control_fallthroughs};
use rue_types::{Type, TypeId, Union};

use crate::{Branches, Compiler, compile_branching_expr, compile_expr};

/// Compiles an `if` expression. If its branches are blocks of a given kind (see
/// [`compile_branching_expr`]), such as the branches of a statement, so are those of the `else if`
/// expressions after it, and for a statement, this also gives what the rest of the block after it
/// sees its values narrowed to.
pub fn compile_if_expr(
    ctx: &mut Compiler,
    expr: &AstIfExpr,
    expected_type: Option<TypeId>,
    branches: Option<Branches>,
) -> (Value, Option<Mappings>) {
    let condition = if let Some(condition) = expr.condition() {
        let value = compile_expr(ctx, &condition, None);
        ctx.check_condition(condition.syntax(), value.ty);
        value
    } else {
        debug!("Unresolved if condition");
        ctx.unresolved()
    };

    let (then_expr, _) = if let Some(then_expr) = expr.then_expr() {
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
        (ctx.unresolved(), None)
    };

    let (else_expr, else_narrowing) = if let Some(else_expr) = expr.else_expr() {
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
    } else if branches.is_some_and(|branches| !branches.require_value) {
        // An `else if` statement can leave out the `else`, which then falls through, like an `if`
        // statement without one, as can one whose value doesn't need to be given, such as at the
        // end of the body of a function without a return type
        let hir = ctx.alloc_hir(Hir::Block(Block {
            statements: Vec::new(),
            body: None,
        }));
        (Value::new(hir, ctx.builtins().types.nil), None)
    } else {
        // The `else` can only be left out at the start of a statement, which is parsed the same as
        // a value until it's known which one it is
        if expr.else_token().is_none() {
            ctx.diagnostic(expr.syntax(), DiagnosticKind::MissingElse);
        }
        debug!("Unresolved if else expr");
        let never = ctx.builtins().types.never;
        (ctx.unresolved().with_type(never), None)
    };

    // The rest of the block after a statement only sees the values that the branch which falls
    // through to it does, if only one of them does. An `else if` chain that only falls through in
    // its last branch is like a series of `if` statements that exit early.
    let narrowing = if branches.is_some_and(Branches::is_statement) && expr.inline().is_none() {
        match (
            control_fallthroughs(ctx, then_expr.hir) == 0,
            control_fallthroughs(ctx, else_expr.hir) == 0,
        ) {
            (true, false) => Some(match else_narrowing {
                Some(else_narrowing) => ctx.merge_mappings(&condition.else_map, &else_narrowing),
                None => condition.else_map.clone(),
            }),
            (false, true) => Some(condition.then_map.clone()),
            _ => None,
        }
    } else {
        None
    };

    let ty = ctx.alloc_type(Type::Union(Union::new(vec![then_expr.ty, else_expr.ty])));
    let hir = ctx.alloc_hir(Hir::If(
        condition.hir,
        then_expr.hir,
        else_expr.hir,
        expr.inline().is_some(),
    ));

    (Value::new(hir, ty), narrowing)
}

fn compile_branch(
    ctx: &mut Compiler,
    expr: &AstExpr,
    expected_type: Option<TypeId>,
    branches: Option<Branches>,
) -> (Value, Option<Mappings>) {
    if let Some(branches) = branches {
        compile_branching_expr(ctx, expr, expected_type, branches)
    } else {
        (compile_expr(ctx, expr, expected_type), None)
    }
}
