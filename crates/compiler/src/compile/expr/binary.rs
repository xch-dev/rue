use rue_ast::AstBinaryExpr;
use rue_hir::{Hir, Value};
use rue_parser::T;

use crate::{Context, compile_expr};

pub fn compile_binary_expr(ctx: &mut Context, binary: &AstBinaryExpr) -> Value {
    let left = if let Some(left) = binary.left() {
        compile_expr(ctx, &left)
    } else {
        ctx.builtins().unresolved.clone()
    };

    let right = if let Some(right) = binary.right() {
        compile_expr(ctx, &right)
    } else {
        ctx.builtins().unresolved.clone()
    };

    let Some(op) = binary.op() else {
        return ctx.builtins().unresolved.clone();
    };

    match op.kind() {
        T![+] => {
            let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::Add, left.hir, right.hir));
            Value::new(hir, ctx.builtins().unresolved.ty)
        }
        _ => todo!(),
    }
}
