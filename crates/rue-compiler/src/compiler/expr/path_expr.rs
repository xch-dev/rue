use rowan::TextRange;
use rue_parser::PathItem;

use crate::{
    compiler::{
        path::{Path, PathKind},
        Compiler,
    },
    hir::Hir,
    symbol::{Function, Symbol},
    value::{GuardPath, Type, Value},
    ErrorKind,
};

impl Compiler<'_> {
    pub fn compile_path_expr(&mut self, items: &[PathItem], text_range: TextRange) -> Value {
        let Some(mut path) = self.resolve_base_path(&items[0], PathKind::Symbol, items.len() == 1)
        else {
            return self.unknown();
        };

        let mut last_ident = items[0]
            .ident()
            .map(|name| name.to_string())
            .unwrap_or("{unknown}".to_string());

        for (i, item) in items.iter().enumerate().skip(1) {
            let Some(next_path) =
                self.resolve_next_path(path, item, PathKind::Symbol, i == items.len() - 1)
            else {
                return self.unknown();
            };

            if let Some(name) = item.ident() {
                last_ident = name.to_string();
            }

            path = next_path;
        }

        let symbol_id = match path {
            Path::Symbol(symbol_id) => symbol_id,
            Path::Type(type_id) => {
                if let Type::EnumVariant(variant_type) = self.db.ty(type_id).clone() {
                    if variant_type.fields.is_some() {
                        self.db.error(
                            ErrorKind::InvalidEnumVariantReference(self.type_name(type_id)),
                            text_range,
                        );
                    }

                    let Type::Enum(enum_type) = self.db.ty(variant_type.enum_type) else {
                        unreachable!();
                    };

                    let mut hir_id = variant_type.discriminant;

                    if enum_type.has_fields {
                        hir_id = self.db.alloc_hir(Hir::Pair(hir_id, self.builtins.nil_hir));
                    }

                    return Value::new(hir_id, type_id);
                }
                self.db
                    .error(ErrorKind::ExpectedSymbolPath(last_ident), text_range);
                return self.unknown();
            }
        };

        if matches!(self.db.symbol(symbol_id), Symbol::Module(..)) {
            self.db
                .error(ErrorKind::ModuleReference(last_ident), text_range);
            return self.unknown();
        }

        if !self.is_callee && matches!(self.db.symbol(symbol_id), Symbol::InlineFunction(..)) {
            self.db
                .error(ErrorKind::InlineFunctionReference(last_ident), text_range);
            return self.unknown();
        }

        let type_override = self.symbol_type(&GuardPath::new(symbol_id));
        let override_type_id = type_override.map(|ty| ty.type_id);
        let mut reference = self.db.alloc_hir(Hir::Reference(symbol_id, text_range));

        if let Some(mutation) = type_override.map(|ty| ty.mutation) {
            reference = self.apply_mutation(reference, mutation);
        }

        let mut value = match self.db.symbol(symbol_id).clone() {
            Symbol::Unknown | Symbol::Module(..) => unreachable!(),
            Symbol::Function(Function { ty, .. }) | Symbol::InlineFunction(Function { ty, .. }) => {
                let type_id = self.db.alloc_type(Type::Function(ty.clone()));
                Value::new(reference, override_type_id.unwrap_or(type_id))
            }
            Symbol::Parameter(type_id) => {
                Value::new(reference, override_type_id.unwrap_or(type_id))
            }
            Symbol::Let(mut value) | Symbol::Const(mut value) | Symbol::InlineConst(mut value) => {
                if let Some(type_id) = override_type_id {
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
