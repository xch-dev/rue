use std::collections::HashMap;

use indexmap::IndexSet;

use crate::{database::TypeId, SymbolId};

#[derive(Debug, Default)]
pub struct Scope {
    symbol_table: HashMap<String, SymbolId>,
    type_aliases: HashMap<String, TypeId>,
    type_names: HashMap<TypeId, String>,
    definitions: IndexSet<SymbolId>,
    used_symbols: IndexSet<SymbolId>,
}

impl Scope {
    pub fn define_symbol(&mut self, name: String, symbol_id: SymbolId) {
        self.symbol_table.insert(name, symbol_id);
        self.definitions.insert(symbol_id);
    }

    pub fn use_symbol(&mut self, symbol_id: SymbolId) {
        self.used_symbols.insert(symbol_id);
    }

    pub fn symbol(&self, name: &str) -> Option<SymbolId> {
        self.symbol_table.get(name).copied()
    }

    pub fn definitions(&self) -> &IndexSet<SymbolId> {
        &self.definitions
    }

    pub fn used_symbols(&self) -> &IndexSet<SymbolId> {
        &self.used_symbols
    }

    pub fn define_type_alias(&mut self, name: String, type_id: TypeId) {
        self.type_aliases.insert(name.clone(), type_id);
        self.type_names.insert(type_id, name);
    }

    pub fn type_alias(&self, name: &str) -> Option<TypeId> {
        self.type_aliases.get(name).copied()
    }

    pub fn type_name(&self, type_id: TypeId) -> Option<&str> {
        self.type_names.get(&type_id).map(|s| s.as_str())
    }
}
