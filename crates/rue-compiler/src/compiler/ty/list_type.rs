use rue_parser::ListType;

use crate::{compiler::Compiler, value::Type, TypeId};

impl Compiler<'_> {
    pub fn compile_list_type(&mut self, list: &ListType) -> TypeId {
        let Some(inner) = list.ty() else {
            return self.builtins.unknown;
        };

        let item_type = self.compile_type(inner);
        self.db.alloc_type(Type::List(item_type))
    }
}
