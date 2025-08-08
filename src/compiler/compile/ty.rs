mod literal;
mod union;

pub use literal::*;
pub use union::*;

use crate::{AstType, Context, TypeId};

pub fn compile_type(ctx: &mut Context, ty: &AstType) -> TypeId {
    match ty {
        AstType::LiteralType(literal) => compile_literal_type(ctx, literal),
        AstType::UnionType(union) => compile_union_type(ctx, union),
    }
}
