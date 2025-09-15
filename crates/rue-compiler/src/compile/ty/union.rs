use log::debug;
use rue_ast::AstUnionType;
use rue_types::{Type, TypeId, Union};

use crate::{Compiler, compile_type};

pub fn compile_union_type(ctx: &mut Compiler, union: &AstUnionType) -> TypeId {
    let mut types = Vec::new();

    for ty in union.types() {
        types.push(compile_type(ctx, &ty));
    }

    if types.is_empty() {
        debug!("Unresolved union type");
        return ctx.builtins().unresolved.ty;
    }

    ctx.alloc_type(Type::Union(Union::new(types)))
}
