use rue_parser::{AstNode, Type};
use rue_typing::TypeId;

use super::Compiler;

mod function_type;
mod pair_type;
mod path_type;
mod union_type;

impl Compiler<'_> {
    pub fn compile_type(&mut self, ty: Type) -> TypeId {
        match ty {
            Type::PathType(path) => {
                self.compile_path_type(&path.items(), path.syntax().text_range())
            }
            Type::FunctionType(function) => self.compile_function_type(&function),
            Type::PairType(tuple) => self.compile_pair_type(&tuple),
            Type::UnionType(union) => self.compile_union_type(&union),
        }
    }
}
