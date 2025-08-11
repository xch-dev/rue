mod path;
mod union;

pub use path::*;
pub use union::*;

use rue_ast::AstType;
use rue_hir::TypeId;

use crate::Compiler;

pub fn compile_type(ctx: &mut Compiler, ty: &AstType) -> TypeId {
    match ty {
        AstType::PathType(path) => compile_path_type(ctx, path),
        AstType::UnionType(union) => compile_union_type(ctx, union),
    }
}
