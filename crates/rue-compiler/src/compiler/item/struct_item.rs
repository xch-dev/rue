use indexmap::IndexMap;
use rue_parser::{StructField, StructItem};

use crate::{
    compiler::Compiler,
    value::{StructType, Type},
    TypeId,
};

impl Compiler<'_> {
    /// Define a type for a struct in the current scope, but leave it as unknown for now.
    pub fn declare_struct_item(&mut self, struct_item: &StructItem) -> TypeId {
        let type_id = self.db.alloc_type(Type::Unknown);
        if let Some(name) = struct_item.name() {
            self.scope_mut().define_type(name.to_string(), type_id);
            self.db.insert_type_token(type_id, name);
        }
        type_id
    }

    /// Compile and resolve a struct type.
    pub fn compile_struct_item(&mut self, struct_item: &StructItem, type_id: TypeId) {
        self.type_definition_stack.push(type_id);
        let fields = self.compile_struct_fields(struct_item.fields());
        *self.db.ty_mut(type_id) = Type::Struct(StructType {
            original_type_id: type_id,
            fields,
        });
        self.type_definition_stack.pop().unwrap();
    }

    /// Compile and resolve the fields of a struct.
    pub fn compile_struct_fields(&mut self, fields: Vec<StructField>) -> IndexMap<String, TypeId> {
        let mut named_fields = IndexMap::new();

        for field in fields {
            let type_id = field
                .ty()
                .map_or(self.builtins.unknown, |ty| self.compile_type(ty));

            if let Some(name) = field.name() {
                named_fields.insert(name.to_string(), type_id);
            };
        }

        named_fields
    }
}
