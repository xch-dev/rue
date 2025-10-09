use std::collections::HashMap;

use id_arena::Id;
use indexmap::{IndexMap, IndexSet};
use rue_types::TypeId;

use crate::SymbolId;

pub type ScopeId = Id<Scope>;

#[derive(Debug, Default, Clone)]
pub struct Scope {
    symbols: IndexMap<String, SymbolId>,
    symbol_names: HashMap<SymbolId, String>,
    types: IndexMap<String, TypeId>,
    type_names: HashMap<TypeId, String>,
    exported_symbols: IndexSet<SymbolId>,
    exported_types: IndexSet<TypeId>,
}

impl Scope {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert_symbol(&mut self, name: String, symbol: SymbolId, exported: bool) {
        self.symbols.insert(name.clone(), symbol);
        self.symbol_names.insert(symbol, name);

        if exported {
            self.exported_symbols.insert(symbol);
        }
    }

    pub fn insert_type(&mut self, name: String, ty: TypeId, exported: bool) {
        self.types.insert(name.clone(), ty);
        self.type_names.insert(ty, name);

        if exported {
            self.exported_types.insert(ty);
        }
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

    /// Iterate over ALL symbols in this scope (including non-exported)
    pub fn symbols(&self) -> impl Iterator<Item = (&str, SymbolId)> {
        self.symbols.iter().map(|(name, id)| (name.as_str(), *id))
    }

    /// Iterate over ALL types in this scope (including non-exported)
    pub fn types(&self) -> impl Iterator<Item = (&str, TypeId)> {
        self.types.iter().map(|(name, id)| (name.as_str(), *id))
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
}
