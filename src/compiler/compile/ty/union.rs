use crate::{AstUnionType, Context, Type, TypeId, compile_type};

pub fn compile_union_type(ctx: &mut Context, union: &AstUnionType) -> TypeId {
    let mut types = Vec::new();

    for ty in union.types() {
        types.push(compile_type(ctx, &ty));
    }

    if types.is_empty() {
        return ctx.builtins().unresolved.ty;
    }

    ctx.alloc_type(Type::Union(types))
}
