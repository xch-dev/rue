use std::collections::HashMap;

use id_arena::Id;
use indexmap::IndexMap;
use rue_types::TypeId;

use crate::SymbolId;

pub type ScopeId = Id<Scope>;

#[derive(Debug, Default, Clone)]
pub struct Scope {
    symbols: IndexMap<String, SymbolId>,
    types: IndexMap<String, TypeId>,
    type_names: HashMap<TypeId, String>,
}

impl Scope {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert_symbol(&mut self, name: String, symbol: SymbolId) {
        self.symbols.insert(name, symbol);
    }

    pub fn insert_type(&mut self, name: String, ty: TypeId) {
        self.types.insert(name.clone(), ty);
        self.type_names.insert(ty, name);
    }

    pub fn symbol(&self, name: &str) -> Option<SymbolId> {
        self.symbols.get(name).copied()
    }

    pub fn ty(&self, name: &str) -> Option<TypeId> {
        self.types.get(name).copied()
    }

    pub fn type_name(&self, ty: TypeId) -> Option<&str> {
        self.type_names.get(&ty).map(|s| s.as_str())
    }
}
