use std::collections::HashMap;

use indexmap::IndexSet;

use crate::SymbolId;

#[derive(Debug, Default)]
pub struct Scope {
    symbols: HashMap<String, SymbolId>,
    local_definitions: IndexSet<SymbolId>,
    referenced_symbols: IndexSet<SymbolId>,
}

impl Scope {
    pub fn define_symbol(&mut self, name: String, symbol_id: SymbolId) {
        self.symbols.insert(name, symbol_id);
        self.local_definitions.insert(symbol_id);
    }

    pub fn symbol(&self, name: &str) -> Option<SymbolId> {
        self.symbols.get(name).copied()
    }

    pub fn reference_symbol(&mut self, symbol_id: SymbolId) {
        self.referenced_symbols.insert(symbol_id);
    }

    pub fn referenced_symbols(&self) -> &IndexSet<SymbolId> {
        &self.referenced_symbols
    }

    pub fn local_definitions(&self) -> &IndexSet<SymbolId> {
        &self.local_definitions
    }
}
