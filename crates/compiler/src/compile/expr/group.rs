use rue_ast::AstGroupExpr;

use crate::{Context, Value, compile_expr};

pub fn compile_group_expr(ctx: &mut Context, group: &AstGroupExpr) -> Value {
    let Some(expr) = group.expr() else {
        return ctx.builtins().unresolved.clone();
    };
    compile_expr(ctx, &expr)
}
