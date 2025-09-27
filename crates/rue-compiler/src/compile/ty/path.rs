use log::debug;
use rue_ast::{AstNode, AstPathType};
use rue_hir::Declaration;
use rue_types::TypeId;

use crate::{Compiler, PathKind, PathResult, compile_path};

pub fn compile_path_type(ctx: &mut Compiler, path: &AstPathType) -> TypeId {
    let PathResult::Type(ty) = compile_path(ctx, path.syntax(), path.segments(), PathKind::Type)
    else {
        debug!("Unresolved path type {}", path.syntax().text());
        return ctx.builtins().unresolved.ty;
    };

    ctx.reference(Declaration::Type(ty));

    ty
}
