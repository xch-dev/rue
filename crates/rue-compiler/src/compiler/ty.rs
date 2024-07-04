use rue_parser::{AstNode, Type};

use crate::TypeId;

use super::Compiler;

mod function_type;
mod list_type;
mod nullable_type;
mod pair_type;
mod path_type;

impl Compiler<'_> {
    pub fn compile_type(&mut self, ty: Type) -> TypeId {
        match ty {
            Type::PathType(path) => {
                self.compile_path_type(&path.idents(), path.syntax().text_range())
            }
            Type::ListType(list) => self.compile_list_type(&list),
            Type::FunctionType(function) => self.compile_function_type(&function),
            Type::PairType(tuple) => self.compile_pair_type(&tuple),
            Type::NullableType(optional) => self.compile_nullable_type(&optional),
        }
    }
}
