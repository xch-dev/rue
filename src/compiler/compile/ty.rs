mod path;
mod union;

pub use path::*;
pub use union::*;

use crate::{AstType, Context, TypeId};

pub fn compile_type(ctx: &mut Context, ty: &AstType) -> TypeId {
    match ty {
        AstType::PathType(path) => compile_path_type(ctx, path),
        AstType::UnionType(union) => compile_union_type(ctx, union),
    }
}
