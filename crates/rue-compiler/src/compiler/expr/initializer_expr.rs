use rue_typing::HashMap;

use indexmap::IndexMap;
use rowan::TextRange;
use rue_parser::{AstNode, InitializerExpr, InitializerField};
use rue_typing::{bigint_to_bytes, deconstruct_items, Type, TypeId};

use crate::{Compiler, ErrorKind, Hir, HirId, Value};

impl Compiler<'_> {
    pub fn compile_initializer_expr(&mut self, initializer: &InitializerExpr) -> Value {
        let ty = initializer
            .path()
            .map(|path| self.compile_path_type(&path.items(), path.syntax().text_range()));

        match ty.map(|ty| self.ty.get_unaliased(ty)).cloned() {
            Some(Type::Struct(struct_type)) => {
                let fields = deconstruct_items(
                    self.ty,
                    struct_type.type_id,
                    struct_type.field_names.len(),
                    struct_type.nil_terminated,
                )
                .expect("invalid variant type");

                let hir_id = self.compile_initializer_fields(
                    &struct_type.field_names.into_iter().zip(fields).collect(),
                    struct_type.nil_terminated,
                    initializer.fields(),
                    initializer.syntax().text_range(),
                );

                match ty {
                    Some(struct_type) => Value::new(hir_id, struct_type),
                    None => self.unknown(),
                }
            }
            Some(Type::Variant(enum_variant)) => {
                if let Some(field_names) = enum_variant.field_names {
                    let Type::Enum(enum_type) = self.ty.get(enum_variant.original_enum_type_id)
                    else {
                        unreachable!();
                    };

                    let fields = if enum_type.has_fields {
                        let type_id = self
                            .ty
                            .get_pair(enum_variant.type_id)
                            .expect("expected a pair")
                            .1;

                        deconstruct_items(
                            self.ty,
                            type_id,
                            field_names.len(),
                            enum_variant.nil_terminated,
                        )
                        .expect("invalid struct type")
                    } else {
                        Vec::new()
                    };

                    let fields_hir_id = self.compile_initializer_fields(
                        &field_names.into_iter().zip(fields).collect(),
                        enum_variant.nil_terminated,
                        initializer.fields(),
                        initializer.syntax().text_range(),
                    );

                    let discriminant = self
                        .db
                        .alloc_hir(Hir::Atom(bigint_to_bytes(enum_variant.discriminant)));

                    let hir_id = self.db.alloc_hir(Hir::Pair(discriminant, fields_hir_id));

                    match ty {
                        Some(struct_type) => Value::new(hir_id, struct_type),
                        None => self.unknown(),
                    }
                } else {
                    self.db.error(
                        ErrorKind::InvalidEnumVariantInitializer(self.type_name(ty.unwrap())),
                        initializer.path().unwrap().syntax().text_range(),
                    );
                    self.unknown()
                }
            }
            Some(..) => {
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
        struct_fields: &IndexMap<String, TypeId>,
        nil_terminated: bool,
        initializer_fields: Vec<InitializerField>,
        text_range: TextRange,
    ) -> HirId {
        let mut specified_fields = HashMap::new();

        for field in initializer_fields {
            let Some(name) = field.name() else {
                continue;
            };

            let expected_type = struct_fields.get(name.text()).copied();

            let value = field
                .expr()
                .map(|expr| self.compile_expr(&expr, expected_type))
                .unwrap_or(self.unknown());

            // Check the type of the field initializer.
            self.type_check(
                value.type_id,
                expected_type.unwrap_or(self.ty.std().unknown),
                field.syntax().text_range(),
            );

            // Insert the field if it exists and hasn't already been assigned.
            if specified_fields.contains_key(name.text()) {
                self.db.error(
                    ErrorKind::DuplicateInitializerField(name.to_string()),
                    name.text_range(),
                );
            } else if !struct_fields.contains_key(name.text()) {
                self.db.error(
                    ErrorKind::UnknownInitializerField(name.to_string()),
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
                ErrorKind::MissingInitializerFields(missing_fields),
                text_range,
            );
        }

        let mut hir_id = self.builtins.nil;

        // Construct a nil-terminated list from the arguments.
        for (i, field) in struct_fields.keys().rev().enumerate() {
            let value = specified_fields.get(field).copied();

            let field = value.unwrap_or(self.builtins.unknown);

            if i == 0 && !nil_terminated {
                hir_id = field;
            } else {
                hir_id = self.db.alloc_hir(Hir::Pair(field, hir_id));
            }
        }

        hir_id
    }
}
