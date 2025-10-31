use std::collections::HashMap;

use id_arena::Id;
use indexmap::{IndexMap, IndexSet};
use rue_parser::SyntaxToken;
use rue_types::TypeId;

use crate::SymbolId;

pub type ScopeId = Id<Scope>;

#[derive(Debug, Clone)]
pub struct Import {
    pub name: SyntaxToken,
    pub include_self: bool,
    pub include_all: bool,
    pub children: IndexMap<String, Import>,
}

impl Import {
    pub fn extend(&mut self, import: Import) {
        self.include_all |= import.include_all;
        self.include_self |= import.include_self;

        for (name, import) in import.children {
            self.children
                .entry(name)
                .or_insert(import.clone())
                .extend(import);
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Scope {
    symbols: IndexMap<String, SymbolId>,
    symbol_names: HashMap<SymbolId, String>,
    types: IndexMap<String, TypeId>,
    type_names: HashMap<TypeId, String>,
    symbol_types: HashMap<SymbolId, TypeId>,
    exported_symbols: IndexSet<SymbolId>,
    exported_types: IndexSet<TypeId>,
    imports: IndexMap<String, Import>,
}

impl Scope {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert_symbol(&mut self, name: String, symbol: SymbolId, exported: bool) {
        assert!(!self.symbols.contains_key(&name));

        self.symbols.insert(name.clone(), symbol);
        self.symbol_names.insert(symbol, name);

        if exported {
            self.exported_symbols.insert(symbol);
        }
    }

    pub fn insert_type(&mut self, name: String, ty: TypeId, exported: bool) {
        assert!(!self.types.contains_key(&name));

        self.types.insert(name.clone(), ty);
        self.type_names.insert(ty, name);

        if exported {
            self.exported_types.insert(ty);
        }
    }

    pub fn override_symbol_type(&mut self, symbol: SymbolId, ty: TypeId) {
        self.symbol_types.insert(symbol, ty);
    }

    pub fn is_symbol_exported(&self, symbol: SymbolId) -> bool {
        self.exported_symbols.contains(&symbol)
    }

    pub fn is_type_exported(&self, ty: TypeId) -> bool {
        self.exported_types.contains(&ty)
    }

    pub fn exported_symbols(&self) -> impl Iterator<Item = (&str, SymbolId)> {
        self.exported_symbols
            .iter()
            .map(|s| (self.symbol_names.get(s).unwrap().as_str(), *s))
    }

    pub fn exported_types(&self) -> impl Iterator<Item = (&str, TypeId)> {
        self.exported_types
            .iter()
            .map(|t| (self.type_names.get(t).unwrap().as_str(), *t))
    }

    pub fn symbol(&self, name: &str) -> Option<SymbolId> {
        self.symbols.get(name).copied()
    }

    pub fn symbol_name(&self, symbol: SymbolId) -> Option<&str> {
        self.symbol_names.get(&symbol).map(String::as_str)
    }

    pub fn ty(&self, name: &str) -> Option<TypeId> {
        self.types.get(name).copied()
    }

    pub fn type_name(&self, ty: TypeId) -> Option<&str> {
        self.type_names.get(&ty).map(String::as_str)
    }

    pub fn symbol_override_type(&self, symbol: SymbolId) -> Option<TypeId> {
        self.symbol_types.get(&symbol).copied()
    }

    pub fn symbol_names(&self) -> impl Iterator<Item = &str> {
        self.symbol_names.values().map(String::as_str)
    }

    pub fn type_names(&self) -> impl Iterator<Item = &str> {
        self.type_names.values().map(String::as_str)
    }

    pub fn import(&mut self, base: String, import: Import) {
        self.imports
            .entry(base)
            .or_insert(import.clone())
            .extend(import);
    }

    pub fn imports(&self) -> IndexMap<String, Import> {
        self.imports.clone()
    }
}
