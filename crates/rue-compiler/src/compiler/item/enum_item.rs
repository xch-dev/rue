use std::collections::HashSet;

use indexmap::IndexMap;
use num_bigint::BigInt;
use num_traits::Zero;
use rue_parser::EnumItem;

use crate::{
    compiler::Compiler,
    hir::Hir,
    value::{EnumType, EnumVariantType, Type},
    ErrorKind, TypeId,
};

impl Compiler<'_> {
    pub fn declare_enum_item(&mut self, enum_item: &EnumItem) -> TypeId {
        let mut variants = IndexMap::new();
        let mut has_fields = false;

        // Add each of the variants to the enum.
        for variant in enum_item.variants() {
            // We don't add variants with missing names, since
            // enum types don't support them currently.
            let Some(name) = variant.name() else {
                continue;
            };

            // Don't overwrite existing variant names.
            if variants.contains_key(name.text()) {
                self.db.error(
                    ErrorKind::DuplicateEnumVariant(name.text().to_string()),
                    name.text_range(),
                );
                continue;
            }

            // Check if the variant has fields.
            if variant.fields().is_some() {
                has_fields = true;
            }

            // Allocate a new type for the variant.
            // It has to be `Unknown` for now, since field types may not be declared yet.
            let type_id = self.db.alloc_type(Type::Unknown);

            // Add the variant to the enum and define the token for the variant.
            variants.insert(name.to_string(), type_id);
            self.db.insert_type_token(type_id, name);
        }

        // Allocate a new type for the enum.
        let type_id = self.db.alloc_type(Type::Enum(EnumType {
            has_fields,
            variants,
        }));

        // Add the enum to the scope and define the token for the enum.
        if let Some(name) = enum_item.name() {
            self.scope_mut().define_type(name.to_string(), type_id);
            self.db.insert_type_token(type_id, name);
        }

        type_id
    }

    pub fn compile_enum_item(&mut self, enum_item: &EnumItem, enum_type_id: TypeId) {
        let Type::Enum(enum_type) = self.db.ty(enum_type_id).clone() else {
            unreachable!();
        };

        // We add this here to track types that are referenced by the enum.
        self.type_definition_stack.push(enum_type_id);

        let mut names = HashSet::new();
        let mut discriminants = HashSet::new();
        let mut highest_discriminant = None;

        // Compile each of the variants.
        for variant in enum_item.variants() {
            // We don't add variants with missing names, since
            // enum types don't support them currently.
            let Some(name) = variant.name() else {
                continue;
            };

            // Don't overwrite existing variant names.
            if !names.insert(name.to_string()) {
                continue;
            }

            // Get the type id for the variant.
            let variant_type_id = enum_type.variants[name.text()];

            // Track types that are referenced by the variant.
            self.type_definition_stack.push(variant_type_id);

            // Compile the fields of the variant.
            let (fields, rest) = variant
                .fields()
                .map(|ast| self.compile_struct_fields(ast.fields()))
                .unwrap_or_default();

            // Get the discriminant of the variant.
            let discriminant = if let Some(token) = variant.discriminant() {
                let discriminant: BigInt = token
                    .text()
                    .replace('_', "")
                    .parse()
                    .expect("failed to parse integer literal");

                if !discriminants.insert(discriminant.clone()) {
                    self.db.error(
                        ErrorKind::DuplicateEnumDiscriminant(discriminant.to_string()),
                        token.text_range(),
                    );
                }

                if let Some(highest) = highest_discriminant.as_mut() {
                    if &discriminant > highest {
                        highest_discriminant = Some(discriminant.clone());
                    }
                } else {
                    highest_discriminant = Some(discriminant.clone());
                }

                discriminant
            } else if let Some(discriminant) = highest_discriminant.as_mut() {
                *discriminant += 1;
                discriminant.clone()
            } else {
                highest_discriminant = Some(BigInt::zero());
                BigInt::zero()
            };

            let atom = Self::bigint_to_bytes(discriminant).unwrap_or_else(|| {
                self.db.error(
                    ErrorKind::EnumDiscriminantTooLarge,
                    variant
                        .discriminant()
                        .map_or(name.text_range(), |token| token.text_range()),
                );
                Vec::new()
            });

            let discriminant = self.db.alloc_hir(Hir::Atom(atom));

            // Update the variant to use the real `EnumVariant` type.
            *self.db.ty_mut(variant_type_id) = Type::EnumVariant(EnumVariantType {
                enum_type: enum_type_id,
                original_type_id: variant_type_id,
                fields: if variant.fields().is_some() {
                    Some(fields)
                } else {
                    None
                },
                rest,
                discriminant,
            });

            self.type_definition_stack.pop().unwrap();
        }

        self.type_definition_stack.pop().unwrap();
    }
}
