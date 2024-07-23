use indexmap::IndexMap;
use rue_parser::{AstNode, StructField, StructItem};

use crate::{
    compiler::Compiler,
    value::{Rest, StructType, Type},
    ErrorKind, TypeId,
};

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
    pub fn compile_struct_item(&mut self, struct_item: &StructItem, type_id: TypeId) {
        self.type_definition_stack.push(type_id);
        let (fields, rest) = self.compile_struct_fields(struct_item.fields());
        *self.db.ty_mut(type_id) = Type::Struct(StructType {
            original_type_id: type_id,
            fields,
            rest,
        });
        self.type_definition_stack.pop().unwrap();
    }

    /// Compile and resolve the fields of a struct.
    pub fn compile_struct_fields(
        &mut self,
        fields: Vec<StructField>,
    ) -> (IndexMap<String, TypeId>, Rest) {
        let mut named_fields = IndexMap::new();
        let mut rest = Rest::Nil;

        let len = fields.len();

        for (i, field) in fields.into_iter().enumerate() {
            let type_id = field
                .ty()
                .map_or(self.ty.std().unknown, |ty| self.compile_type(ty));

            // Check if it's a spread or optional parameter.
            let last = i + 1 == len;
            let spread = field.spread().is_some();
            let optional = field.optional().is_some();

            if spread && optional {
                self.db
                    .error(ErrorKind::OptionalFieldSpread, field.syntax().text_range());
            } else if spread && !last {
                self.db
                    .error(ErrorKind::InvalidSpreadField, field.syntax().text_range());
            } else if optional && !last {
                self.db
                    .error(ErrorKind::InvalidOptionalField, field.syntax().text_range());
            } else if spread {
                rest = Rest::Spread;
            } else if optional {
                rest = Rest::Optional;
            }

            if let Some(name) = field.name() {
                named_fields.insert(name.to_string(), type_id);
            };
        }

        (named_fields, rest)
    }
}
