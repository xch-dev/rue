use rue_ast::AstFunctionCallExpr;
use rue_hir::Value;

use crate::{Compiler, compile_expr};

pub fn compile_function_call_expr(ctx: &mut Compiler, call: &AstFunctionCallExpr) -> Value {
    let Some(expr) = call.expr() else {
        return ctx.builtins().unresolved.clone();
    };

    compile_expr(ctx, &expr)
}
