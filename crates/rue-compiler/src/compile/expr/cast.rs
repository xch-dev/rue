use log::debug;
use rue_ast::{AstCastExpr, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Hir, Value};

use crate::{Compiler, compile_expr, compile_type};

pub fn compile_cast_expr(ctx: &mut Compiler, cast: &AstCastExpr) -> Value {
    let ty = if let Some(ty) = cast.ty() {
        compile_type(ctx, &ty)
    } else {
        debug!("Unresolved cast type");
        ctx.unresolved().ty
    };

    let expr = if let Some(expr) = cast.expr() {
        // The value only has to be guarded or cast to it
        ctx.with_expected_type_hint(true, |ctx| compile_expr(ctx, &expr, Some(ty)))
    } else {
        debug!("Unresolved cast expr");
        ctx.unresolved()
    };

    // Nothing can be cast to a type without values, such as `Never`. The cast is unresolved, so
    // that the code after it isn't reported as unreachable because of the mistake.
    if rue_types::is_uninhabited(ctx.types_mut(), ty)
        && ctx.try_is_assignable(expr.ty, ty) == Ok(false)
    {
        let from = ctx.type_name(expr.ty);
        let to = ctx.type_name(ty);
        ctx.diagnostic(cast.syntax(), DiagnosticKind::UninhabitedCast(from, to));
        return ctx.unresolved();
    }

    ctx.cast_type(cast.syntax(), expr.ty, ty);

    // The cast keeps the value's reference and type guard mappings, since it's the same value. A
    // guard on the cast narrows the value within its own semantics (see `rue_types::intersect`),
    // so the value still needs a cast to take on the semantics of the cast type.
    let hir = ctx.alloc_hir(Hir::Cast(expr.hir));
    expr.with_hir(hir).with_type(ty)
}
