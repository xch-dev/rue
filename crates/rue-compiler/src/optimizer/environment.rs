use indexmap::IndexSet;

use crate::{EnvironmentId, SymbolId};

#[derive(Debug, Clone, Default)]
pub struct Environment {
    defined_symbols: IndexSet<SymbolId>,
    captured_symbols: IndexSet<SymbolId>,
    parameters: IndexSet<SymbolId>,
    rest_parameter: bool,
    parent: Option<EnvironmentId>,
}

impl Environment {
    pub fn function(parameters: IndexSet<SymbolId>, rest_parameter: bool) -> Self {
        Self {
            parameters,
            rest_parameter,
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
        self.defined_symbols.shift_remove(&symbol_id);
    }

    pub fn define(&mut self, symbol_id: SymbolId) {
        self.defined_symbols.insert(symbol_id);
    }

    pub fn capture(&mut self, symbol_id: SymbolId) {
        self.captured_symbols.insert(symbol_id);
    }

    pub fn definitions(&self) -> IndexSet<SymbolId> {
        self.defined_symbols.clone()
    }

    pub fn captures(&self) -> IndexSet<SymbolId> {
        self.captured_symbols.clone()
    }

    pub fn parameters(&self) -> IndexSet<SymbolId> {
        self.parameters.clone()
    }

    pub fn rest_parameter(&self) -> bool {
        self.rest_parameter
    }

    pub fn parent(&self) -> Option<EnvironmentId> {
        self.parent
    }

    pub fn build(&self) -> IndexSet<SymbolId> {
        let mut symbol_ids = IndexSet::new();
        symbol_ids.extend(self.defined_symbols.iter().copied());

        if self.parent.is_none() {
            symbol_ids.extend(self.captured_symbols.iter().copied());
        } else {
            assert!(self.parameters.is_empty());
        }

        symbol_ids.extend(self.parameters.iter().copied());
        symbol_ids
    }

    pub fn extend_from_child(&mut self, other: Self) {
        assert!(other.parameters.is_empty());
        assert!(other.parent.is_none());
        assert!(!other.rest_parameter);

        self.defined_symbols.extend(other.defined_symbols);

        for symbol_id in other.captured_symbols {
            if !self.defined_symbols.contains(&symbol_id) {
                self.captured_symbols.insert(symbol_id);
            }
        }
    }
}
