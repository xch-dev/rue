use rue_parser::SyntaxToken;

use crate::{symbol::Symbol, ty::Type, ErrorKind, SymbolId, TypeId};

use super::Compiler;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PathItem {
    Symbol(SymbolId),
    Type(TypeId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PathKind {
    Symbol,
    Type,
}

impl Compiler<'_> {
    pub fn resolve_base_path(
        &mut self,
        name: &SyntaxToken,
        path_kind: PathKind,
    ) -> Option<PathItem> {
        for &scope_id in self.scope_stack.iter().rev() {
            if let Some(type_id) = self.db.scope(scope_id).ty(name.text()) {
                self.type_reference(type_id);
                return Some(PathItem::Type(type_id));
            } else if let Some(symbol_id) = self.db.scope(scope_id).symbol(name.text()) {
                return Some(PathItem::Symbol(symbol_id));
            }
        }

        self.db.error(
            match path_kind {
                PathKind::Type => ErrorKind::UnknownType(name.to_string()),
                PathKind::Symbol => ErrorKind::UnknownSymbol(name.to_string()),
            },
            name.text_range(),
        );

        None
    }

    pub fn resolve_next_path(&mut self, item: PathItem, name: &SyntaxToken) -> Option<PathItem> {
        match item {
            PathItem::Type(type_id) => {
                let Type::Enum(enum_type) = self.db.ty(type_id) else {
                    self.db.error(
                        ErrorKind::InvalidTypePath(self.type_name(type_id)),
                        name.text_range(),
                    );
                    return None;
                };

                let Some(variant_type) = enum_type.variants.get(name.text()).copied() else {
                    self.db.error(
                        ErrorKind::UnknownEnumVariant(name.text().to_string()),
                        name.text_range(),
                    );
                    return None;
                };

                self.type_reference(variant_type);

                Some(PathItem::Type(variant_type))
            }
            PathItem::Symbol(symbol_id) => {
                let Symbol::Module(module) = self.db.symbol(symbol_id) else {
                    self.db
                        .error(ErrorKind::InvalidSymbolPath, name.text_range());
                    return None;
                };

                let scope = self.db.scope(module.scope_id);

                if let Some(type_id) = scope.ty(name.text()) {
                    if !module.exported_types.contains(&type_id) {
                        self.db.error(
                            ErrorKind::PrivateType(name.text().to_string()),
                            name.text_range(),
                        );
                        return None;
                    }

                    self.type_reference(type_id);

                    return Some(PathItem::Type(type_id));
                }

                if let Some(symbol_id) = scope.symbol(name.text()) {
                    if !module.exported_symbols.contains(&symbol_id) {
                        self.db.error(
                            ErrorKind::PrivateSymbol(name.text().to_string()),
                            name.text_range(),
                        );
                        return None;
                    }
                    return Some(PathItem::Symbol(symbol_id));
                }

                self.db.error(
                    ErrorKind::UnknownModulePath(name.text().to_string()),
                    name.text_range(),
                );
                None
            }
        }
    }
}
