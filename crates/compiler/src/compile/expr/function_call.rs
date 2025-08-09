use rue_ast::AstFunctionCallExpr;

use crate::{Context, Value, compile_expr};

pub fn compile_function_call_expr(ctx: &mut Context, call: &AstFunctionCallExpr) -> Value {
    let Some(expr) = call.expr() else {
        return ctx.builtins().unresolved.clone();
    };

    compile_expr(ctx, &expr)
}
