use indexmap::IndexSet;

use crate::{EnvironmentId, SymbolId};

#[derive(Debug, Clone, Default)]
pub struct Environment {
    definitions: IndexSet<SymbolId>,
    captures: IndexSet<SymbolId>,
    parameters: IndexSet<SymbolId>,
    rest: bool,
    parent: Option<EnvironmentId>,
}

impl Environment {
    pub fn function(parameters: IndexSet<SymbolId>, rest: bool) -> Self {
        Self {
            parameters,
            rest,
            ..Default::default()
        }
    }

    pub fn binding(parent: EnvironmentId) -> Self {
        Self {
            parent: Some(parent),
            ..Default::default()
        }
    }

    pub fn remove_definition(&mut self, symbol_id: SymbolId) {
        self.definitions.shift_remove(&symbol_id);
    }

    pub fn define(&mut self, symbol_id: SymbolId) {
        self.definitions.insert(symbol_id);
    }

    pub fn capture(&mut self, symbol_id: SymbolId) {
        self.captures.insert(symbol_id);
    }

    pub fn definitions(&self) -> IndexSet<SymbolId> {
        self.definitions.clone()
    }

    pub fn captures(&self) -> IndexSet<SymbolId> {
        self.captures.clone()
    }

    pub fn parameters(&self) -> IndexSet<SymbolId> {
        self.parameters.clone()
    }

    pub fn rest(&self) -> bool {
        self.rest
    }

    pub fn parent(&self) -> Option<EnvironmentId> {
        self.parent
    }

    pub fn build(&self) -> IndexSet<SymbolId> {
        let mut symbol_ids = IndexSet::new();
        symbol_ids.extend(self.definitions.iter().copied());
        if self.parent.is_none() {
            symbol_ids.extend(self.captures.iter().copied());
        } else {
            assert!(self.parameters.is_empty());
        }
        symbol_ids.extend(self.parameters.iter().copied());
        symbol_ids
    }

    pub fn extend_from_child(&mut self, other: Self) {
        assert!(other.parameters.is_empty());
        assert!(other.parent.is_none());
        assert!(!other.rest);

        self.definitions.extend(other.definitions);

        for symbol_id in other.captures {
            if !self.definitions.contains(&symbol_id) {
                self.captures.insert(symbol_id);
            }
        }
    }
}
