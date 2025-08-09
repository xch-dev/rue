use rue_ast::AstPrefixExpr;
use rue_parser::T;

use crate::{Context, Hir, UnaryOp, Value, compile_expr};

pub fn compile_prefix_expr(ctx: &mut Context, prefix: &AstPrefixExpr) -> Value {
    let Some(expr) = prefix.expr() else {
        return ctx.builtins().unresolved.clone();
    };

    let value = compile_expr(ctx, &expr);

    let Some(op) = prefix.op() else {
        return value;
    };

    match op.kind() {
        T![!] => {
            let hir = ctx.alloc_hir(Hir::Unary(UnaryOp::Not, value.hir));
            Value::new(hir, ctx.builtins().unresolved.ty)
        }
        _ => todo!(),
    }
}
