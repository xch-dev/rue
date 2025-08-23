use rue_ast::AstFieldAccessExpr;
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Hir, UnaryOp, Value};
use rue_types::Type;

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

    let ty = rue_types::unwrap_semantic(ctx.types_mut(), expr.ty, true);

    match ctx.ty(ty).clone() {
        Type::Apply(_) | Type::Alias(_) | Type::Ref(_) => unreachable!(),
        Type::Unresolved => ctx.builtins().unresolved.clone(),
        Type::Generic | Type::Atom(_) | Type::Union(_) | Type::Function(_) => {
            let type_name = ctx.type_name(expr.ty);
            ctx.diagnostic(
                &name,
                DiagnosticKind::UnknownField(name.text().to_string(), type_name),
            );
            ctx.builtins().unresolved.clone()
        }
        Type::Pair(pair) => match name.text() {
            "first" => {
                let hir = ctx.alloc_hir(Hir::Unary(UnaryOp::First, expr.hir));
                Value::new(hir, pair.first)
            }
            "rest" => {
                let hir = ctx.alloc_hir(Hir::Unary(UnaryOp::Rest, expr.hir));
                Value::new(hir, pair.rest)
            }
            _ => {
                let type_name = ctx.type_name(expr.ty);
                ctx.diagnostic(
                    &name,
                    DiagnosticKind::UnknownField(name.text().to_string(), type_name),
                );
                ctx.builtins().unresolved.clone()
            }
        },
        Type::Struct(_) => todo!(),
    }
}
