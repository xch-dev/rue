use crate::{AstLiteralType, AstType, Context, ErrorKind, TypeId};

pub fn compile_type(ctx: &mut Context, ty: &AstType) -> TypeId {
    match ty {
        AstType::LiteralType(literal) => compile_literal_type(ctx, literal),
    }
}

pub fn compile_literal_type(ctx: &mut Context, literal: &AstLiteralType) -> TypeId {
    let Some(value) = literal.value() else {
        return ctx.builtins().unresolved;
    };

    let Some(ty) = ctx.resolve_type(value.text()) else {
        ctx.error(&value, ErrorKind::UndeclaredType(value.text().to_string()));
        return ctx.builtins().unresolved;
    };

    ty
}
