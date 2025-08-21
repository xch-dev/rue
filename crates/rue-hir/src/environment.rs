use indexmap::IndexSet;

use crate::SymbolId;

#[derive(Debug, Default, Clone)]
pub struct Environment {
    symbols: IndexSet<SymbolId>,
}

impl Environment {
    pub fn new(captures: &[SymbolId], parameters: &[SymbolId]) -> Self {
        Self {
            symbols: captures.iter().chain(parameters).copied().collect(),
        }
    }

    pub fn with_bindings(&self, bindings: &[SymbolId]) -> Self {
        Self {
            symbols: bindings
                .iter()
                .chain(self.symbols.iter())
                .copied()
                .collect(),
        }
    }

    pub fn try_path(&self, symbol_id: SymbolId) -> Option<u32> {
        let mut path = 2;

        for _ in 0..self.symbols.get_index_of(&symbol_id)? {
            path *= 2;
            path += 1;
        }

        Some(path)
    }

    pub fn path(&self, symbol_id: SymbolId) -> u32 {
        self.try_path(symbol_id).expect("symbol not found")
    }
}
