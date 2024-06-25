use rue_parser::FieldAccessExpr;

use crate::{
    compiler::Compiler,
    hir::Hir,
    value::{Guard, GuardPathItem, PairType, Type, Value},
    ErrorKind,
};

impl Compiler<'_> {
    /// Compiles a field access expression, or special properties for certain types.
    pub fn compile_field_access_expr(&mut self, field_access: &FieldAccessExpr) -> Value {
        let Some(old_value) = field_access
            .expr()
            .map(|expr| self.compile_expr(&expr, None))
        else {
            return self.unknown();
        };

        let Some(field_name) = field_access.field() else {
            return self.unknown();
        };

        let mut new_value = match self.db.ty(old_value.type_id).clone() {
            Type::Struct(struct_type) => {
                if let Some(field) = struct_type.fields.get_full(field_name.text()) {
                    let (index, _, field_type) = field;
                    Value::new(
                        self.compile_index(old_value.hir_id, index, false),
                        *field_type,
                    )
                    .extend_guard_path(old_value, GuardPathItem::Field(field_name.to_string()))
                } else {
                    self.db.error(
                        ErrorKind::UndefinedField {
                            field: field_name.to_string(),
                            ty: self.type_name(old_value.type_id),
                        },
                        field_name.text_range(),
                    );
                    return self.unknown();
                }
            }
            Type::EnumVariant(variant_type) => {
                if let Some(field) = variant_type.fields.get_full(field_name.text()) {
                    let (index, _, field_type) = field;
                    Value::new(
                        self.compile_index(old_value.hir_id, index, false),
                        *field_type,
                    )
                    .extend_guard_path(old_value, GuardPathItem::Field(field_name.to_string()))
                } else {
                    self.db.error(
                        ErrorKind::UndefinedField {
                            field: field_name.to_string(),
                            ty: self.type_name(old_value.type_id),
                        },
                        field_name.text_range(),
                    );
                    return self.unknown();
                }
            }
            Type::Pair(PairType { first, rest }) => match field_name.text() {
                "first" => {
                    return Value::new(self.db.alloc_hir(Hir::First(old_value.hir_id)), first)
                        .extend_guard_path(old_value, GuardPathItem::First);
                }
                "rest" => {
                    return Value::new(self.db.alloc_hir(Hir::Rest(old_value.hir_id)), rest)
                        .extend_guard_path(old_value, GuardPathItem::Rest);
                }
                _ => {
                    self.db.error(
                        ErrorKind::InvalidFieldAccess {
                            field: field_name.to_string(),
                            ty: self.type_name(old_value.type_id),
                        },
                        field_name.text_range(),
                    );
                    return self.unknown();
                }
            },
            Type::Bytes | Type::Bytes32 if field_name.text() == "length" => Value::new(
                self.db.alloc_hir(Hir::Strlen(old_value.hir_id)),
                self.builtins.int,
            ),
            Type::PossiblyUndefined(inner) if field_name.text() == "exists" => {
                let maybe_nil_reference = self.db.alloc_hir(Hir::CheckExists(old_value.hir_id));
                let exists = self.db.alloc_hir(Hir::IsCons(maybe_nil_reference));
                let mut new_value = Value::new(exists, self.builtins.bool);

                if let Some(guard_path) = old_value.guard_path {
                    new_value
                        .guards
                        .insert(guard_path, Guard::new(inner, old_value.type_id));
                }

                new_value
            }
            _ => {
                self.db.error(
                    ErrorKind::InvalidFieldAccess {
                        field: field_name.to_string(),
                        ty: self.type_name(old_value.type_id),
                    },
                    field_name.text_range(),
                );
                return self.unknown();
            }
        };

        if let Some(guard_path) = new_value.guard_path.as_ref() {
            if let Some(type_id) = self.symbol_type(guard_path) {
                new_value.type_id = type_id;
            }
        }

        new_value
    }
}
