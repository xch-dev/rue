use indexmap::IndexSet;

use crate::{scope::Scope, SymbolId};

#[derive(Debug, Clone)]
pub struct Environment {
    symbols: IndexSet<SymbolId>,
    external_references: IndexSet<SymbolId>,
}

impl Environment {
    pub fn from_scope(scope: &Scope) -> Self {
        let references = scope.referenced_symbols();
        let definitions = scope.local_definitions();

        let mut external_references = IndexSet::new();
        let mut parameters = IndexSet::new();

        for &symbol_id in references {
            if !definitions.contains(&symbol_id) {
                external_references.insert(symbol_id);
            } else {
                parameters.insert(symbol_id);
            }
        }
        Self {
            symbols: external_references
                .iter()
                .chain(parameters.iter())
                .copied()
                .collect(),
            external_references,
        }
    }

    pub fn symbols(&self) -> &IndexSet<SymbolId> {
        &self.symbols
    }

    pub fn external_references(&self) -> &IndexSet<SymbolId> {
        &self.external_references
    }
}
