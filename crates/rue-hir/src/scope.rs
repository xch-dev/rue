use std::collections::HashMap;

use id_arena::Id;
use indexmap::{IndexMap, IndexSet};
use rue_types::TypeId;

use crate::{ImportId, SymbolId};

pub type ScopeId = Id<Scope>;

#[derive(Debug, Clone)]
pub struct Scope {
    parent: Option<ScopeId>,
    symbols: IndexMap<String, SymbolId>,
    symbol_names: HashMap<SymbolId, String>,
    types: IndexMap<String, TypeId>,
    type_names: HashMap<TypeId, String>,
    symbol_types: HashMap<SymbolId, TypeId>,
    exported_symbols: IndexSet<SymbolId>,
    exported_types: IndexSet<TypeId>,
    imports: Vec<ImportId>,
}

impl Scope {
    pub fn new(parent: Option<ScopeId>) -> Self {
        Self {
            parent,
            symbols: IndexMap::new(),
            symbol_names: HashMap::new(),
            types: IndexMap::new(),
            type_names: HashMap::new(),
            symbol_types: HashMap::new(),
            exported_symbols: IndexSet::new(),
            exported_types: IndexSet::new(),
            imports: Vec::new(),
        }
    }

    pub fn parent(&self) -> Option<ScopeId> {
        self.parent
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

    pub fn add_import(&mut self, import: ImportId) {
        self.imports.push(import);
    }

    pub fn imports(&self) -> impl Iterator<Item = ImportId> {
        self.imports.iter().copied()
    }
}
