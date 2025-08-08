use crate::{AstLiteralType, Context, DiagnosticKind, TypeId};

pub fn compile_literal_type(ctx: &mut Context, literal: &AstLiteralType) -> TypeId {
    let Some(value) = literal.value() else {
        return ctx.builtins().unresolved.ty;
    };

    let Some(ty) = ctx.resolve_type(value.text()) else {
        ctx.diagnostic(
            &value,
            DiagnosticKind::UndeclaredType(value.text().to_string()),
        );
        return ctx.builtins().unresolved.ty;
    };

    ty
}
