use indexmap::{IndexMap, IndexSet};
use rue_parser::EnumItem;

use crate::{
    compiler::Compiler,
    ty::{EnumType, EnumVariant, Type},
    ErrorKind, TypeId,
};

impl Compiler<'_> {
    /// Define a type for an enum in the current scope.
    /// This creates the enum variants as well, but they are left as unknown types.
    pub fn declare_enum_item(&mut self, enum_item: &EnumItem) -> TypeId {
        let mut variants = IndexMap::new();

        for variant in enum_item.variants() {
            let Some(name) = variant.name() else {
                continue;
            };

            // Silently ignore duplicate variants, since they will be caught later.
            if variants.contains_key(name.text()) {
                continue;
            }

            let type_id = self.db.alloc_type(Type::Unknown);
            variants.insert(name.to_string(), type_id);
            self.db.insert_type_token(type_id, name);
        }

        let type_id = self.db.alloc_type(Type::Enum(EnumType { variants }));

        if let Some(name) = enum_item.name() {
            self.scope_mut().define_type(name.to_string(), type_id);
            self.db.insert_type_token(type_id, name);
        }

        type_id
    }

    /// Compile and resolve an enum type, and each of its variants' struct fields.
    pub fn compile_enum_item(&mut self, enum_item: &EnumItem, type_id: TypeId) {
        self.type_definition_stack.push(type_id);

        let Type::Enum(enum_type) = self.db.ty(type_id).clone() else {
            unreachable!();
        };

        let mut visited_variants = IndexSet::new();

        for variant in enum_item.variants() {
            let Some(name) = variant.name() else {
                continue;
            };

            // If the variant is a duplicate, we don't want to overwrite the existing variant.
            if !visited_variants.insert(name.to_string()) {
                self.db.error(
                    ErrorKind::DuplicateEnumVariant(name.to_string()),
                    name.text_range(),
                );
                continue;
            }

            let variant_type = enum_type.variants[name.text()];

            self.type_definition_stack.push(variant_type);

            let fields = self.compile_struct_fields(variant.fields());

            let discriminant = variant
                .discriminant()
                .map_or(self.builtins.unknown_hir, |discriminant| {
                    self.compile_int_literal(&discriminant).hir_id
                });

            *self.db.ty_mut(variant_type) = Type::EnumVariant(EnumVariant {
                name: name.to_string(),
                enum_type: type_id,
                fields,
                discriminant,
            });

            self.type_definition_stack.pop().unwrap();
        }

        self.type_definition_stack.pop().unwrap();
    }
}
