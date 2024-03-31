use std::collections::HashMap;

use indexmap::IndexSet;

use crate::SymbolId;

#[derive(Debug, Default)]
pub struct Scope {
    symbol_table: HashMap<String, SymbolId>,
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

    pub fn get_symbol(&self, name: &str) -> Option<SymbolId> {
        self.symbol_table.get(name).copied()
    }

    pub fn definitions(&self) -> &IndexSet<SymbolId> {
        &self.definitions
    }

    pub fn used_symbols(&self) -> &IndexSet<SymbolId> {
        &self.used_symbols
    }
}
