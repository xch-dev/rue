use rowan::TextRange;
use rue_parser::PathItem;
use rue_typing::{bigint_to_bytes, Type};

use crate::{
    compiler::{
        path::{Path, PathKind},
        Compiler,
    },
    hir::Hir,
    symbol::{Function, Symbol},
    value::{GuardPath, Value},
    ErrorKind,
};

impl Compiler<'_> {
    pub fn compile_path_expr(&mut self, items: &[PathItem], text_range: TextRange) -> Value {
        let Some(mut path) = self.resolve_base_path(&items[0], PathKind::Symbol, items.len() == 1)
        else {
            return self.unknown();
        };

        let mut last_name = items[0].name().unwrap().to_string();

        for (i, item) in items.iter().enumerate().skip(1) {
            let Some(next_path) =
                self.resolve_next_path(path, item, PathKind::Symbol, i == items.len() - 1)
            else {
                return self.unknown();
            };
            last_name = item.name().unwrap().to_string();
            path = next_path;
        }

        let symbol_id = match path {
            Path::Symbol(symbol_id) => symbol_id,
            Path::Type(type_id) => {
                if let Type::Variant(variant) = self.ty.get(type_id).clone() {
                    if variant.field_names.is_some() {
                        self.db.error(
                            ErrorKind::InvalidEnumVariantReference(self.type_name(type_id)),
                            text_range,
                        );
                    }

                    let Type::Enum(enum_type) = self.ty.get(variant.original_enum_type_id) else {
                        unreachable!();
                    };

                    let mut hir_id = self
                        .db
                        .alloc_hir(Hir::Atom(bigint_to_bytes(variant.discriminant)));

                    if enum_type.has_fields {
                        hir_id = self.db.alloc_hir(Hir::Pair(hir_id, self.builtins.nil));
                    }

                    return Value::new(hir_id, type_id);
                }
                self.db
                    .error(ErrorKind::ExpectedSymbolPath(last_name), text_range);
                return self.unknown();
            }
        };

        if matches!(self.db.symbol(symbol_id), Symbol::Module(..)) {
            self.db
                .error(ErrorKind::ModuleReference(last_name), text_range);
            return self.unknown();
        }

        if !self.is_callee && matches!(self.db.symbol(symbol_id), Symbol::InlineFunction(..)) {
            self.db
                .error(ErrorKind::InlineFunctionReference(last_name), text_range);
            return self.unknown();
        }

        let type_override = self.symbol_type(&GuardPath::new(symbol_id));
        let reference = self.db.alloc_hir(Hir::Reference(symbol_id, text_range));

        let mut value = match self.db.symbol(symbol_id).clone() {
            Symbol::Unknown | Symbol::Module(..) => unreachable!(),
            Symbol::Function(Function { type_id, .. })
            | Symbol::InlineFunction(Function { type_id, .. })
            | Symbol::Parameter(type_id) => Value::new(reference, type_override.unwrap_or(type_id)),
            Symbol::Let(mut value) | Symbol::Const(mut value) | Symbol::InlineConst(mut value) => {
                if let Some(type_id) = type_override {
                    value.type_id = type_id;
                }
                value.hir_id = reference;
                value
            }
        };
        value.guard_path = Some(GuardPath::new(symbol_id));
        value
    }
}
