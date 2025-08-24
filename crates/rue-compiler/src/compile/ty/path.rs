use rue_ast::AstPathType;
use rue_types::TypeId;

use crate::{Compiler, PathKind, PathResult, compile_path};

pub fn compile_path_type(ctx: &mut Compiler, path: &AstPathType) -> TypeId {
    let PathResult::Type(ty) = compile_path(ctx, path.segments(), PathKind::Type) else {
        return ctx.builtins().unresolved.ty;
    };

    ty
}
