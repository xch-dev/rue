use rue_parser::UnionType;

use crate::{compiler::Compiler, value::Type, TypeId};

impl Compiler<'_> {
    pub fn compile_union_type(&mut self, union_type: &UnionType) -> TypeId {
        let mut type_ids = Vec::new();

        for ty in union_type.types() {
            type_ids.push(self.compile_type(ty));
        }

        self.db.alloc_type(Type::Union(type_ids))
    }
}
