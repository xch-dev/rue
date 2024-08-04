use rue_parser::{FieldAccessExpr, SyntaxToken};
use rue_typing::{deconstruct_items, index_to_path, Struct, Type, TypeId, TypePath, Variant};

use crate::{Compiler, ErrorKind, Hir, Op, Value};

impl Compiler<'_> {
    /// Compiles a field access expression, or special properties for certain types.
    pub fn compile_field_access_expr(&mut self, field_access: &FieldAccessExpr) -> Value {
        let Some(old_value) = field_access
            .expr()
            .map(|expr| self.compile_expr(&expr, None))
        else {
            return self.unknown();
        };

        let Some(name) = field_access.field() else {
            return self.unknown();
        };

        match self.ty.get(old_value.type_id).clone() {
            Type::Unknown => self.unknown(),
            Type::Struct(ty) => {
                let Some(value) = self.compile_struct_field_access(old_value, &ty, &name) else {
                    return self.unknown();
                };
                value
            }
            Type::Variant(ty) => {
                let Some(value) = self.compile_variant_field_access(old_value, &ty, &name) else {
                    return self.unknown();
                };
                value
            }
            Type::Pair(first, rest) => {
                let Some(value) = self.compile_pair_field_access(old_value, first, rest, &name)
                else {
                    return self.unknown();
                };
                value
            }
            Type::Bytes | Type::Bytes32 if name.text() == "length" => Value::new(
                self.db.alloc_hir(Hir::Op(Op::Strlen, old_value.hir_id)),
                self.ty.std().int,
            ),
            _ => {
                self.db.error(
                    ErrorKind::InvalidFieldAccess(
                        name.to_string(),
                        self.type_name(old_value.type_id),
                    ),
                    name.text_range(),
                );
                self.unknown()
            }
        }
    }

    fn compile_pair_field_access(
        &mut self,
        old_value: Value,
        first: TypeId,
        rest: TypeId,
        name: &SyntaxToken,
    ) -> Option<Value> {
        let path = match name.text() {
            "first" => TypePath::First,
            "rest" => TypePath::Rest,
            _ => {
                self.db.error(
                    ErrorKind::InvalidFieldAccess(
                        name.to_string(),
                        self.type_name(old_value.type_id),
                    ),
                    name.text_range(),
                );
                return None;
            }
        };

        let type_id = match path {
            TypePath::First => first,
            TypePath::Rest => rest,
        };

        let mut value = Value::new(self.hir_path(old_value.hir_id, &[path]), type_id);

        value.guard_path = old_value.guard_path.map(|mut guard_path| {
            guard_path.items.push(path);
            guard_path
        });

        Some(value)
    }

    fn compile_struct_field_access(
        &mut self,
        old_value: Value,
        ty: &Struct,
        name: &SyntaxToken,
    ) -> Option<Value> {
        let fields =
            deconstruct_items(self.ty, ty.type_id, ty.field_names.len(), ty.nil_terminated)
                .unwrap();

        let Some(index) = ty.field_names.get_index_of(name.text()) else {
            self.db
                .error(ErrorKind::UnknownField(name.to_string()), name.text_range());
            return None;
        };

        let type_id = fields[index];

        let path_items = index_to_path(
            index,
            index != ty.field_names.len() - 1 || ty.nil_terminated,
        );

        let mut value = Value::new(self.hir_path(old_value.hir_id, &path_items), type_id);

        value.guard_path = old_value.guard_path.map(|mut guard_path| {
            guard_path.items.extend(path_items);
            guard_path
        });

        Some(value)
    }

    fn compile_variant_field_access(
        &mut self,
        old_value: Value,
        ty: &Variant,
        name: &SyntaxToken,
    ) -> Option<Value> {
        let field_names = ty.field_names.clone().unwrap_or_default();

        let Type::Enum(enum_type) = self.ty.get(ty.original_enum_type_id) else {
            unreachable!();
        };

        let fields = if enum_type.has_fields {
            let type_id = self.ty.get_pair(ty.type_id).expect("expected a pair").1;

            ty.field_names
                .as_ref()
                .map(|field_names| {
                    deconstruct_items(self.ty, type_id, field_names.len(), ty.nil_terminated)
                        .unwrap()
                })
                .unwrap_or_default()
        } else {
            Vec::new()
        };

        let Some(index) = field_names.get_index_of(name.text()) else {
            self.db
                .error(ErrorKind::UnknownField(name.to_string()), name.text_range());
            return None;
        };

        let type_id = fields[index];

        let path_items = index_to_path(index + 1, index != fields.len() - 1 || ty.nil_terminated);

        let mut value = Value::new(self.hir_path(old_value.hir_id, &path_items), type_id);

        value.guard_path = old_value.guard_path.map(|mut guard_path| {
            guard_path.items.extend(path_items);
            guard_path
        });

        Some(value)
    }
}
