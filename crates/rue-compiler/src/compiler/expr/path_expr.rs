use rowan::TextRange;
use rue_parser::SyntaxToken;

use crate::{
    compiler::{
        path::{PathItem, PathKind},
        Compiler,
    },
    hir::Hir,
    symbol::{Const, Function, Let, Symbol},
    ty::{Type, Value},
    ErrorKind,
};

impl Compiler<'_> {
    pub fn compile_path_expr(&mut self, idents: &[SyntaxToken], text_range: TextRange) -> Value {
        let Some(mut item) =
            self.resolve_base_path(&idents[0], PathKind::Symbol, idents.len() == 1)
        else {
            return self.unknown();
        };

        for (i, name) in idents.iter().enumerate().skip(1) {
            let Some(next_item) =
                self.resolve_next_path(item, name, PathKind::Symbol, i == idents.len() - 1)
            else {
                return self.unknown();
            };
            item = next_item;
        }

        let symbol_id = match item {
            PathItem::Symbol(symbol_id) => symbol_id,
            PathItem::Type(type_id) => {
                if let Type::EnumVariant(variant_type) = self.db.ty(type_id).clone() {
                    let Type::Enum(enum_type) = self.db.ty(variant_type.enum_type) else {
                        unreachable!();
                    };

                    if enum_type.has_fields {
                        self.db.error(ErrorKind::EnumVariantWithFields, text_range);
                    }

                    return Value::new(variant_type.discriminant, type_id);
                }
                self.db.error(ErrorKind::ExpectedSymbolPath, text_range);
                return self.unknown();
            }
        };

        if matches!(self.db.symbol(symbol_id), Symbol::Module(..)) {
            self.db.error(ErrorKind::ModuleReference, text_range);
            return self.unknown();
        }

        if !self.is_callee && matches!(self.db.symbol(symbol_id), Symbol::InlineFunction(..)) {
            self.db
                .error(ErrorKind::InlineFunctionReference, text_range);
            return self.unknown();
        }

        Value::new(
            self.db.alloc_hir(Hir::Reference(symbol_id)),
            self.symbol_type(symbol_id)
                .unwrap_or_else(|| match self.db.symbol(symbol_id) {
                    Symbol::Unknown | Symbol::Module(..) => unreachable!(),
                    Symbol::Function(Function { ty, .. })
                    | Symbol::InlineFunction(Function { ty, .. }) => {
                        self.db.alloc_type(Type::Function(ty.clone()))
                    }
                    Symbol::Parameter(type_id)
                    | Symbol::Let(Let { type_id, .. })
                    | Symbol::Const(Const { type_id, .. })
                    | Symbol::InlineConst(Const { type_id, .. }) => *type_id,
                }),
        )
    }
}
