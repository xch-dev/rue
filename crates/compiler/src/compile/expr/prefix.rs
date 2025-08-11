use rue_ast::AstPrefixExpr;
use rue_hir::{Hir, UnaryOp, Value};
use rue_parser::T;

use crate::{Compiler, compile_expr};

pub fn compile_prefix_expr(ctx: &mut Compiler, prefix: &AstPrefixExpr) -> Value {
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
