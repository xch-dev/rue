use crate::{AstTypeAliasItem, Context, ErrorKind, Type, TypeId, UnresolvedType};

pub fn declare_type_alias(ctx: &mut Context, type_alias: &AstTypeAliasItem) -> TypeId {
    let ty = ctx.alloc_type(Type::Unresolved(UnresolvedType {
        name: type_alias.name(),
    }));

    if let Some(name) = type_alias.name() {
        if ctx.last_scope().ty(name.text()).is_some() {
            ctx.error(&name, ErrorKind::DuplicateType(name.text().to_string()));
        }

        ctx.last_scope_mut()
            .insert_type(name.text().to_string(), ty);
    }

    ty
}

pub fn compile_type_alias(ctx: &mut Context, type_alias: &AstTypeAliasItem, ty: TypeId) {}
