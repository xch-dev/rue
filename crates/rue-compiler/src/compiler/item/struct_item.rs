use indexmap::IndexMap;
use rue_parser::{AstNode, StructField, StructItem};
use rue_typing::{construct_items, Struct, Type, TypeId};

use crate::{compiler::Compiler, ErrorKind};

impl Compiler<'_> {
    /// Define a type for a struct in the current scope, but leave it as unknown for now.
    pub fn declare_struct_item(&mut self, struct_item: &StructItem) -> TypeId {
        let type_id = self.ty.alloc(Type::Unknown);
        if let Some(name) = struct_item.name() {
            self.scope_mut().define_type(name.to_string(), type_id);
            self.db.insert_type_token(type_id, name);
        }
        type_id
    }

    /// Compile and resolve a struct type.
    pub fn compile_struct_item(&mut self, struct_item: &StructItem, struct_type_id: TypeId) {
        self.type_definition_stack.push(struct_type_id);

        let (fields, nil_terminated) = self.compile_struct_fields(struct_item.fields());
        let type_id = construct_items(self.ty, fields.values().copied(), nil_terminated);

        *self.ty.get_mut(struct_type_id) = Type::Struct(Struct {
            original_type_id: struct_type_id,
            field_names: fields.keys().cloned().collect(),
            type_id,
            nil_terminated,
            generic_types: Vec::new(),
        });

        self.type_definition_stack.pop().unwrap();
    }

    /// Compile and resolve the fields of a struct.
    pub fn compile_struct_fields(
        &mut self,
        fields: Vec<StructField>,
    ) -> (IndexMap<String, TypeId>, bool) {
        let mut named_fields = IndexMap::new();
        let mut nil_terminated = true;

        let len = fields.len();

        for (i, field) in fields.into_iter().enumerate() {
            let type_id = field
                .ty()
                .map_or(self.ty.std().unknown, |ty| self.compile_type(ty));

            // Check if it's a spread or optional parameter.
            let last = i + 1 == len;
            let spread = field.spread().is_some();

            if spread {
                if !last {
                    self.db
                        .error(ErrorKind::InvalidSpreadField, field.syntax().text_range());
                }
                nil_terminated = false;
            }

            if let Some(name) = field.name() {
                named_fields.insert(name.to_string(), type_id);
            };
        }

        (named_fields, nil_terminated)
    }
}
