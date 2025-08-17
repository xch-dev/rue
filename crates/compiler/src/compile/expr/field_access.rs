use rue_ast::AstFieldAccessExpr;
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Hir, Type, UnaryOp, Value};

use crate::{Compiler, compile_expr};

pub fn compile_field_access_expr(ctx: &mut Compiler, access: &AstFieldAccessExpr) -> Value {
    let expr = if let Some(expr) = access.expr() {
        compile_expr(ctx, &expr)
    } else {
        ctx.builtins().unresolved.clone()
    };

    let Some(name) = access.field() else {
        return ctx.builtins().unresolved.clone();
    };

    let ty = ctx.unwrap_type(expr.ty);

    match ctx.ty(ty).clone() {
        Type::Alias(..) => unreachable!(),
        Type::Unresolved => ctx.builtins().unresolved.clone(),
        Type::Generic(..) | Type::Atom(..) | Type::Union(..) | Type::Fn(..) => {
            ctx.diagnostic(
                &name,
                DiagnosticKind::UnknownField(name.text().to_string(), ctx.type_name(expr.ty)),
            );
            ctx.builtins().unresolved.clone()
        }
        Type::Pair(first, rest) => match name.text() {
            "first" => {
                let hir = ctx.alloc_hir(Hir::Unary(UnaryOp::First, expr.hir));
                Value::new(hir, first)
            }
            "rest" => {
                let hir = ctx.alloc_hir(Hir::Unary(UnaryOp::Rest, expr.hir));
                Value::new(hir, rest)
            }
            _ => {
                ctx.diagnostic(
                    &name,
                    DiagnosticKind::UnknownField(name.text().to_string(), ctx.type_name(expr.ty)),
                );
                ctx.builtins().unresolved.clone()
            }
        },
    }
}
