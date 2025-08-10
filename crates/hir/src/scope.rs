use id_arena::Id;
use indexmap::IndexMap;

use crate::{SymbolId, TypeId};

pub type ScopeId = Id<Scope>;

#[derive(Debug, Default, Clone)]
pub struct Scope {
    symbols: IndexMap<String, SymbolId>,
    types: IndexMap<String, TypeId>,
}

impl Scope {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert_symbol(&mut self, name: String, symbol: SymbolId) {
        self.symbols.insert(name, symbol);
    }

    pub fn insert_type(&mut self, name: String, ty: TypeId) {
        self.types.insert(name, ty);
    }

    pub fn symbol(&self, name: &str) -> Option<SymbolId> {
        self.symbols.get(name).copied()
    }

    pub fn ty(&self, name: &str) -> Option<TypeId> {
        self.types.get(name).copied()
    }
}
