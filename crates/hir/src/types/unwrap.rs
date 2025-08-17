use crate::{ComparisonContext, Database, Type, TypeId};

pub fn unwrap_type(db: &Database, ctx: &mut ComparisonContext, mut ty: TypeId) -> TypeId {
    if let Some(new_ty) = ctx.inferred(ty) {
        ty = new_ty;
    }

    match db.ty(ty).clone() {
        Type::Alias(alias) => unwrap_type(db, ctx, alias.inner),
        _ => ty,
    }
}
