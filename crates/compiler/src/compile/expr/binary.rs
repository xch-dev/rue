use rue_ast::{AstBinaryExpr, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{BinaryOp, Hir, Value};
use rue_parser::T;

use crate::{Compiler, compile_expr};

pub fn compile_binary_expr(ctx: &mut Compiler, binary: &AstBinaryExpr) -> Value {
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
            if ctx.is_assignable(left.ty, ctx.builtins().int) {
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::Add, left.hir, right.hir));
                return Value::unmapped(hir, ctx.builtins().int);
            }
        }
        T![-] => {
            if ctx.is_assignable(left.ty, ctx.builtins().int) {
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::Sub, left.hir, right.hir));
                return Value::unmapped(hir, ctx.builtins().int);
            }
        }
        T![*] => {
            if ctx.is_assignable(left.ty, ctx.builtins().int) {
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::Mul, left.hir, right.hir));
                return Value::unmapped(hir, ctx.builtins().int);
            }
        }
        T![/] => {
            if ctx.is_assignable(left.ty, ctx.builtins().int) {
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::Div, left.hir, right.hir));
                return Value::unmapped(hir, ctx.builtins().int);
            }
        }
        T![>] => {
            if ctx.is_assignable(left.ty, ctx.builtins().int) {
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::Gt, left.hir, right.hir));
                return Value::unmapped(hir, ctx.builtins().bool);
            }
        }
        T![<] => {
            if ctx.is_assignable(left.ty, ctx.builtins().int) {
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::Lt, left.hir, right.hir));
                return Value::unmapped(hir, ctx.builtins().bool);
            }
        }
        T![>=] => {
            if ctx.is_assignable(left.ty, ctx.builtins().int) {
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::Gte, left.hir, right.hir));
                return Value::unmapped(hir, ctx.builtins().bool);
            }
        }
        T![<=] => {
            if ctx.is_assignable(left.ty, ctx.builtins().int) {
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::Lte, left.hir, right.hir));
                return Value::unmapped(hir, ctx.builtins().bool);
            }
        }
        _ => {}
    }

    ctx.diagnostic(
        binary.syntax(),
        DiagnosticKind::IncompatibleBinaryOp(
            op.text().to_string(),
            ctx.type_name(left.ty),
            ctx.type_name(right.ty),
        ),
    );
    ctx.builtins().unresolved.clone()
}
