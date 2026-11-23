use log::debug;
use rue_ast::{AstNode, AstUnionType};
use rue_lint::WrittenUnion;
use rue_types::{Type, TypeId, Union};

use crate::{Compiler, compile_type};

pub fn compile_union_type(ctx: &mut Compiler, union: &AstUnionType) -> TypeId {
    let mut members = Vec::new();

    for ty in union.types() {
        let srcloc = ctx.trimmed_srcloc(ty.syntax());
        members.push((srcloc, compile_type(ctx, &ty)));
    }

    if members.is_empty() {
        debug!("Unresolved union type");
        return ctx.unresolved().ty;
    }

    let types = members.iter().map(|&(_, ty)| ty).collect();

    ctx.lint_sites_mut()
        .written_unions
        .push(WrittenUnion { members });

    ctx.alloc_type(Type::Union(Union::new(types)))
}
