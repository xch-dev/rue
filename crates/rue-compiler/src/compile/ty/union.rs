use log::debug;
use rue_ast::{AstNode, AstUnionType};
use rue_lint::{UnionMember, WrittenUnion};
use rue_types::{Type, TypeId, Union};

use crate::{Compiler, compile_type};

pub fn compile_union_type(ctx: &mut Compiler, union: &AstUnionType) -> TypeId {
    let mut members = Vec::new();

    for ty in union.types() {
        let srcloc = ctx.trimmed_srcloc(ty.syntax());
        let ty = compile_type(ctx, &ty);
        members.push(UnionMember { srcloc, ty });
    }

    if members.is_empty() {
        debug!("Unresolved union type");
        return ctx.unresolved().ty;
    }

    let types = members.iter().map(|member| member.ty).collect();

    ctx.lint_sites_mut()
        .written_unions
        .push(WrittenUnion { members });

    ctx.alloc_type(Type::Union(Union::new(types)))
}
