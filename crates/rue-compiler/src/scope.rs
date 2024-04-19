use std::collections::HashMap;

use indexmap::{IndexMap, IndexSet};

use crate::{database::TypeId, SymbolId};

#[derive(Debug, Default)]
pub struct Scope {
    symbol_table: HashMap<String, SymbolId>,
    type_aliases: HashMap<String, TypeId>,
    type_names: IndexMap<TypeId, String>,
    local_symbols: IndexSet<SymbolId>,
}

impl Scope {
    pub fn define_symbol(&mut self, name: String, symbol_id: SymbolId) {
        self.symbol_table.insert(name, symbol_id);
        self.local_symbols.insert(symbol_id);
    }

    pub fn symbol(&self, name: &str) -> Option<SymbolId> {
        self.symbol_table.get(name).copied()
    }

    pub fn define_type(&mut self, name: String, type_id: TypeId) {
        self.type_aliases.insert(name.clone(), type_id);
        self.type_names.insert(type_id, name);
    }

    pub fn type_alias(&self, name: &str) -> Option<TypeId> {
        self.type_aliases.get(name).copied()
    }

    pub fn type_name(&self, type_id: TypeId) -> Option<&str> {
        self.type_names.get(&type_id).map(|s| s.as_str())
    }

    pub fn is_local(&self, symbol_id: SymbolId) -> bool {
        self.local_symbols.contains(&symbol_id)
    }

    pub fn local_symbols(&self) -> Vec<SymbolId> {
        self.local_symbols.iter().copied().collect()
    }

    pub fn local_types(&self) -> Vec<TypeId> {
        self.type_names.keys().copied().collect()
    }
}
