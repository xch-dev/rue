use rue_parser::UnionType;
use rue_typing::{Type, TypeId};

use crate::Compiler;

impl Compiler<'_> {
    pub fn compile_union_type(&mut self, union: &UnionType) -> TypeId {
        let mut types = Vec::new();
        for ty in union.types() {
            types.push(self.compile_type(ty));
        }
        self.ty.alloc(Type::Union(types))
    }
}
