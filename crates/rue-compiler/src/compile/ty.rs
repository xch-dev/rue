mod group;
mod literal;
mod pair;
mod path;
mod union;

pub use group::*;
pub use literal::*;
pub use pair::*;
pub use path::*;
pub use union::*;

use rue_ast::AstType;
use rue_types::TypeId;

use crate::Compiler;

pub fn compile_type(ctx: &mut Compiler, ty: &AstType) -> TypeId {
    match ty {
        AstType::PathType(path) => compile_path_type(ctx, path),
        AstType::UnionType(union) => compile_union_type(ctx, union),
        AstType::GroupType(group) => compile_group_type(ctx, group),
        AstType::PairType(pair) => compile_pair_type(ctx, pair),
        AstType::LiteralType(literal) => compile_literal_type(ctx, literal),
    }
}
