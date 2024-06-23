use indexmap::IndexMap;

use crate::{database::TypeId, SymbolId};

#[derive(Debug, Default)]
pub struct Scope {
    symbol_table: IndexMap<String, SymbolId>,
    symbol_names: IndexMap<SymbolId, String>,
    type_aliases: IndexMap<String, TypeId>,
    type_names: IndexMap<TypeId, String>,
}

impl Scope {
    pub fn define_symbol(&mut self, name: String, symbol_id: SymbolId) {
        self.symbol_table.insert(name.clone(), symbol_id);
        self.symbol_names.insert(symbol_id, name);
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
        self.type_names.get(&type_id).map(String::as_str)
    }

    pub fn symbol_name(&self, symbol_id: SymbolId) -> Option<&str> {
        self.symbol_names.get(&symbol_id).map(String::as_str)
    }

    pub fn is_local(&self, symbol_id: SymbolId) -> bool {
        self.symbol_names.contains_key(&symbol_id)
    }

    pub fn local_symbols(&self) -> Vec<SymbolId> {
        self.symbol_names.keys().copied().collect()
    }

    pub fn local_types(&self) -> Vec<TypeId> {
        self.type_names.keys().copied().collect()
    }
}
