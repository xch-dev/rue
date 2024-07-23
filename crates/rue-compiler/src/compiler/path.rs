use rue_parser::SyntaxToken;
use rue_typing::{Type, TypeId};

use crate::{symbol::Symbol, ErrorKind, SymbolId};

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
        last: bool,
    ) -> Option<PathItem> {
        for &scope_id in self.scope_stack.iter().rev() {
            let type_id = self.db.scope(scope_id).ty(name.text());
            let symbol_id = self.db.scope(scope_id).symbol(name.text());

            if let (Some(type_id), Some(symbol_id)) = (type_id, symbol_id) {
                if let Symbol::Module(..) = self.db.symbol(symbol_id) {
                    if !last {
                        return Some(PathItem::Symbol(symbol_id));
                    }
                }

                if let Type::Enum(..) = self.ty.get(type_id) {
                    if !last {
                        self.type_reference(type_id);
                        return Some(PathItem::Type(type_id));
                    }
                }

                match path_kind {
                    PathKind::Type => {
                        self.type_reference(type_id);
                        return Some(PathItem::Type(type_id));
                    }
                    PathKind::Symbol => {
                        return Some(PathItem::Symbol(symbol_id));
                    }
                }
            } else if let Some(type_id) = type_id {
                self.type_reference(type_id);
                return Some(PathItem::Type(type_id));
            } else if let Some(symbol_id) = symbol_id {
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

    pub fn resolve_next_path(
        &mut self,
        item: PathItem,
        name: &SyntaxToken,
        path_kind: PathKind,
        last: bool,
    ) -> Option<PathItem> {
        match item {
            PathItem::Type(type_id) => {
                let Type::Enum(enum_type) = self.ty.get(type_id) else {
                    self.db.error(
                        ErrorKind::InvalidTypePath(self.type_name(type_id)),
                        name.text_range(),
                    );
                    return None;
                };

                let Some(variant_type) = enum_type.variants.get(name.text()).copied() else {
                    self.db.error(
                        ErrorKind::UnknownEnumVariantPath(name.text().to_string()),
                        name.text_range(),
                    );
                    return None;
                };

                self.type_reference(variant_type);
                Some(PathItem::Type(variant_type))
            }
            PathItem::Symbol(module_id) => {
                let Symbol::Module(module) = self.db.symbol(module_id) else {
                    self.db.error(
                        ErrorKind::InvalidSymbolPath(self.symbol_name(module_id)),
                        name.text_range(),
                    );
                    return None;
                };

                let scope = self.db.scope(module.scope_id);

                let type_id = scope.ty(name.text());
                let symbol_id = scope.symbol(name.text());

                let private_type =
                    type_id.is_some_and(|type_id| !module.exported_types.contains(&type_id));
                let private_symbol = symbol_id
                    .is_some_and(|symbol_id| !module.exported_symbols.contains(&symbol_id));

                let type_id = type_id.filter(|type_id| module.exported_types.contains(type_id));
                let symbol_id =
                    symbol_id.filter(|symbol_id| module.exported_symbols.contains(symbol_id));

                if let (Some(type_id), Some(symbol_id)) = (type_id, symbol_id) {
                    if let Symbol::Module(..) = self.db.symbol(symbol_id) {
                        if !last {
                            return Some(PathItem::Symbol(symbol_id));
                        }
                    }

                    if let Type::Enum(..) = self.ty.get(type_id) {
                        if !last {
                            self.type_reference(type_id);
                            return Some(PathItem::Type(type_id));
                        }
                    }

                    match path_kind {
                        PathKind::Type => {
                            self.type_reference(type_id);
                            Some(PathItem::Type(type_id))
                        }
                        PathKind::Symbol => Some(PathItem::Symbol(symbol_id)),
                    }
                } else if let Some(type_id) = type_id {
                    self.type_reference(type_id);
                    Some(PathItem::Type(type_id))
                } else if let Some(symbol_id) = symbol_id {
                    Some(PathItem::Symbol(symbol_id))
                } else if private_type {
                    self.db.error(
                        ErrorKind::PrivateType(name.text().to_string()),
                        name.text_range(),
                    );
                    None
                } else if private_symbol {
                    self.db.error(
                        ErrorKind::PrivateSymbol(name.text().to_string()),
                        name.text_range(),
                    );
                    None
                } else {
                    self.db.error(
                        ErrorKind::UnknownModulePath(name.text().to_string()),
                        name.text_range(),
                    );
                    None
                }
            }
        }
    }
}
