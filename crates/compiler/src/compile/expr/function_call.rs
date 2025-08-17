use rue_ast::AstFunctionCallExpr;
use rue_hir::{Hir, Type, Value};

use crate::{Compiler, compile_expr};

pub fn compile_function_call_expr(ctx: &mut Compiler, call: &AstFunctionCallExpr) -> Value {
    let Some(expr) = call.expr() else {
        return ctx.builtins().unresolved.clone();
    };

    let expr = compile_expr(ctx, &expr);

    let mut args = Vec::new();

    for arg in call.args() {
        let value = compile_expr(ctx, &arg);
        args.push(value.hir);
    }

    let hir = ctx.alloc_hir(Hir::FunctionCall(expr.hir, args));

    // TODO: Finish
    let ty = match ctx.ty(expr.ty).clone() {
        Type::Fn(function) => function.ret,
        _ => todo!(),
    };

    Value::new(hir, ty)
}
