use indexmap::IndexSet;

use crate::{EnvironmentId, SymbolId};

#[derive(Debug, Default)]
pub struct Environment {
    defined_symbols: IndexSet<SymbolId>,
    captured_symbols: IndexSet<SymbolId>,
    parameters: IndexSet<SymbolId>,
    parent: Option<EnvironmentId>,
    varargs: bool,
}

impl Environment {
    pub fn function(parameters: IndexSet<SymbolId>, varargs: bool) -> Self {
        Self {
            parameters,
            varargs,
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

    pub fn definitions(&self) -> Vec<SymbolId> {
        self.defined_symbols.iter().copied().collect()
    }

    pub fn captures(&self) -> Vec<SymbolId> {
        self.captured_symbols.iter().copied().collect()
    }

    pub fn parameters(&self) -> Vec<SymbolId> {
        self.parameters.iter().copied().collect()
    }

    pub fn varargs(&self) -> bool {
        self.varargs
    }

    pub fn parent(&self) -> Option<EnvironmentId> {
        self.parent
    }

    pub fn build(&self) -> Vec<SymbolId> {
        let mut symbol_ids = Vec::new();
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
        assert!(!other.varargs);

        self.defined_symbols.extend(other.defined_symbols);

        for symbol_id in other.captured_symbols {
            if !self.defined_symbols.contains(&symbol_id) {
                self.captured_symbols.insert(symbol_id);
            }
        }
    }
}
