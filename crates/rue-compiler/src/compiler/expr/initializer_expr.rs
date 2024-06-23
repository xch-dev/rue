use std::collections::HashMap;

use indexmap::IndexMap;
use rowan::TextRange;
use rue_parser::{AstNode, InitializerExpr, InitializerField};

use crate::{
    compiler::Compiler,
    hir::Hir,
    ty::{Type, Value},
    ErrorKind, HirId, TypeId,
};

impl Compiler<'_> {
    pub fn compile_initializer_expr(&mut self, initializer: &InitializerExpr) -> Value {
        let ty = initializer
            .path()
            .map(|path| self.compile_path_type(&path.idents(), path.syntax().text_range()));

        match ty.map(|ty| self.db.ty(ty)).cloned() {
            Some(Type::Struct(struct_type)) => {
                let hir_id = self.compile_initializer_fields(
                    ty.unwrap(),
                    &struct_type.fields,
                    initializer.fields(),
                    initializer.syntax().text_range(),
                );

                match ty {
                    Some(struct_type) => Value::new(hir_id, struct_type),
                    None => self.unknown(),
                }
            }
            Some(Type::EnumVariant(enum_variant)) => {
                let fields_hir_id = self.compile_initializer_fields(
                    ty.unwrap(),
                    &enum_variant.fields,
                    initializer.fields(),
                    initializer.syntax().text_range(),
                );

                let hir_id = self
                    .db
                    .alloc_hir(Hir::Pair(enum_variant.discriminant, fields_hir_id));

                match ty {
                    Some(struct_type) => Value::new(hir_id, struct_type),
                    None => self.unknown(),
                }
            }
            Some(_) => {
                self.db.error(
                    ErrorKind::UninitializableType(self.type_name(ty.unwrap())),
                    initializer.path().unwrap().syntax().text_range(),
                );
                self.unknown()
            }
            _ => self.unknown(),
        }
    }

    fn compile_initializer_fields(
        &mut self,
        struct_type: TypeId,
        struct_fields: &IndexMap<String, TypeId>,
        initializer_fields: Vec<InitializerField>,
        text_range: TextRange,
    ) -> HirId {
        let mut specified_fields = HashMap::new();

        for field in initializer_fields {
            let expected_type = field
                .name()
                .and_then(|name| struct_fields.get(name.text()).copied());

            let value = field
                .expr()
                .map(|expr| self.compile_expr(&expr, expected_type))
                .unwrap_or(self.unknown());

            // Check the type of the field initializer.
            self.type_check(
                value.type_id,
                expected_type.unwrap_or(self.builtins.unknown),
                field.syntax().text_range(),
            );

            let Some(name) = field.name() else {
                continue;
            };

            // Insert the field if it exists and hasn't already been assigned.
            if specified_fields.contains_key(name.text()) {
                self.db.error(
                    ErrorKind::DuplicateField(name.to_string()),
                    name.text_range(),
                );
            } else if !struct_fields.contains_key(name.text()) {
                self.db.error(
                    ErrorKind::UndefinedField {
                        field: name.to_string(),
                        ty: self.type_name(struct_type),
                    },
                    name.text_range(),
                );
            } else {
                specified_fields.insert(name.to_string(), value.hir_id);
            }
        }

        // Check for any missing fields and report them.
        let missing_fields: Vec<String> = struct_fields
            .keys()
            .filter(|name| !specified_fields.contains_key(*name))
            .cloned()
            .collect();

        if !missing_fields.is_empty() {
            self.db.error(
                ErrorKind::MissingFields {
                    fields: missing_fields,
                    ty: self.type_name(struct_type),
                },
                text_range,
            );
        }

        let mut hir_id = self.builtins.nil_hir;

        // Construct a nil-terminated list from the arguments.
        for field in struct_fields.keys().rev() {
            let field = specified_fields
                .get(field)
                .copied()
                .unwrap_or(self.builtins.unknown_hir);

            hir_id = self.db.alloc_hir(Hir::Pair(field, hir_id));
        }

        hir_id
    }
}
