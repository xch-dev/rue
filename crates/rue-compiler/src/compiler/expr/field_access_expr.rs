use rue_parser::FieldAccess;

use crate::{
    compiler::Compiler,
    hir::Hir,
    ty::{PairType, Type, Value},
    ErrorKind,
};

impl Compiler<'_> {
    /// Compiles a field access expression, or special properties for certain types.
    pub fn compile_field_access_expr(&mut self, field_access: &FieldAccess) -> Value {
        let Some(value) = field_access
            .expr()
            .map(|expr| self.compile_expr(&expr, None))
        else {
            return self.unknown();
        };

        let Some(field_name) = field_access.field() else {
            return self.unknown();
        };

        match self.db.ty(value.type_id).clone() {
            Type::Struct(struct_type) => {
                if let Some(field) = struct_type.fields.get_full(field_name.text()) {
                    let (index, _, field_type) = field;
                    return Value::new(self.compile_index(value.hir_id, index, false), *field_type);
                }
                self.db.error(
                    ErrorKind::UndefinedField {
                        field: field_name.to_string(),
                        ty: self.type_name(value.type_id),
                    },
                    field_name.text_range(),
                );
                return self.unknown();
            }
            Type::Pair(PairType { first, rest }) => match field_name.text() {
                "first" => {
                    return Value::new(self.db.alloc_hir(Hir::First(value.hir_id)), first);
                }
                "rest" => {
                    return Value::new(self.db.alloc_hir(Hir::Rest(value.hir_id)), rest);
                }
                _ => {}
            },
            Type::Bytes | Type::Bytes32 if field_name.text() == "length" => {
                return Value::new(
                    self.db.alloc_hir(Hir::Strlen(value.hir_id)),
                    self.builtins.int,
                );
            }
            _ => {}
        }

        self.db.error(
            ErrorKind::InvalidFieldAccess {
                field: field_name.to_string(),
                ty: self.type_name(value.type_id),
            },
            field_name.text_range(),
        );
        self.unknown()
    }
}