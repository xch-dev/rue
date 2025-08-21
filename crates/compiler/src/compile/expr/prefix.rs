use rue_ast::{AstNode, AstPrefixExpr};
use rue_diagnostic::DiagnosticKind;
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
            if ctx.is_assignable(value.ty, ctx.builtins().bool) {
                let hir = ctx.alloc_hir(Hir::Unary(UnaryOp::Not, value.hir));
                return value
                    .flip_mappings()
                    .with_hir(hir)
                    .with_type(ctx.builtins().bool);
            }
        }
        T![+] => {
            if ctx.is_assignable(value.ty, ctx.builtins().int) {
                ctx.diagnostic(prefix.syntax(), DiagnosticKind::UnnecessaryPlus);
                return Value::new(value.hir, ctx.builtins().int);
            }
        }
        T![-] => {
            if ctx.is_assignable(value.ty, ctx.builtins().int) {
                let hir = ctx.alloc_hir(Hir::Unary(UnaryOp::Neg, value.hir));
                return Value::new(hir, ctx.builtins().int);
            }
        }
        T![~] => {
            if ctx.is_assignable(value.ty, ctx.builtins().int) {
                let hir = ctx.alloc_hir(Hir::Unary(UnaryOp::BitwiseNot, value.hir));
                return Value::new(hir, ctx.builtins().int);
            }
        }
        _ => {}
    }

    ctx.diagnostic(
        prefix.syntax(),
        DiagnosticKind::IncompatibleUnaryOp(op.text().to_string(), ctx.type_name(value.ty)),
    );
    ctx.builtins().unresolved.clone()
}
