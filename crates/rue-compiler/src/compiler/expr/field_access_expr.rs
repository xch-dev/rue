use rue_parser::FieldAccessExpr;
use rue_typing::{deconstruct_items, Rest, Type};

use crate::{
    compiler::Compiler,
    hir::{Hir, Op},
    value::{GuardPathItem, Value},
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

        let mut new_value = match self.ty.get(old_value.type_id).clone() {
            Type::Struct(ty) => {
                let fields = deconstruct_items(self.ty, ty.type_id, ty.field_names.len(), ty.rest)
                    .expect("invalid struct type");

                if let Some(index) = ty.field_names.get_index_of(field_name.text()) {
                    let type_id = fields[index];

                    if index == ty.field_names.len() - 1 && ty.rest == Rest::Optional {
                        todo!();
                        // TODO: type_id = self.ty.alloc(Type::Optional(type_id));
                    }

                    Value::new(
                        self.compile_index(
                            old_value.hir_id,
                            index,
                            index == ty.field_names.len() - 1 && ty.rest != Rest::Nil,
                        ),
                        type_id,
                    )
                    .extend_guard_path(old_value, GuardPathItem::Field(field_name.to_string()))
                } else {
                    self.db.error(
                        ErrorKind::UnknownField(field_name.to_string()),
                        field_name.text_range(),
                    );
                    return self.unknown();
                }
            }
            Type::Variant(variant) => {
                let field_names = variant.field_names.clone().unwrap_or_default();

                let fields = variant
                    .field_names
                    .as_ref()
                    .map(|field_names| {
                        deconstruct_items(self.ty, variant.type_id, field_names.len(), variant.rest)
                            .expect("invalid struct type")
                    })
                    .unwrap_or_default();

                if let Some(index) = field_names.get_index_of(field_name.text()) {
                    let type_id = fields[index];

                    if index == fields.len() - 1 && variant.rest == Rest::Optional {
                        // TODO: type_id = self.ty.alloc(Type::Optional(type_id));
                        todo!()
                    }

                    let fields_hir_id = self.db.alloc_hir(Hir::Op(Op::Rest, old_value.hir_id));

                    Value::new(
                        self.compile_index(
                            fields_hir_id,
                            index,
                            index == fields.len() - 1 && variant.rest != Rest::Nil,
                        ),
                        type_id,
                    )
                    .extend_guard_path(old_value, GuardPathItem::Field(field_name.to_string()))
                } else {
                    self.db.error(
                        ErrorKind::UnknownField(field_name.to_string()),
                        field_name.text_range(),
                    );
                    return self.unknown();
                }
            }
            Type::Pair(first, rest) => match field_name.text() {
                "first" => Value::new(
                    self.db.alloc_hir(Hir::Op(Op::First, old_value.hir_id)),
                    first,
                )
                .extend_guard_path(old_value, GuardPathItem::First),
                "rest" => Value::new(self.db.alloc_hir(Hir::Op(Op::Rest, old_value.hir_id)), rest)
                    .extend_guard_path(old_value, GuardPathItem::Rest),
                _ => {
                    self.db.error(
                        ErrorKind::InvalidFieldAccess(
                            field_name.to_string(),
                            self.type_name(old_value.type_id),
                        ),
                        field_name.text_range(),
                    );
                    return self.unknown();
                }
            },
            Type::Bytes | Type::Bytes32 if field_name.text() == "length" => Value::new(
                self.db.alloc_hir(Hir::Op(Op::Strlen, old_value.hir_id)),
                self.ty.std().int,
            ),
            _ => {
                self.db.error(
                    ErrorKind::InvalidFieldAccess(
                        field_name.to_string(),
                        self.type_name(old_value.type_id),
                    ),
                    field_name.text_range(),
                );
                return self.unknown();
            }
        };

        if let Some(guard_path) = new_value.guard_path.as_ref() {
            if let Some(type_override) = self.symbol_type(guard_path) {
                new_value.type_id = type_override.type_id;
                new_value.hir_id = self.apply_mutation(new_value.hir_id, type_override.mutation);
            }
        }

        new_value
    }
}
