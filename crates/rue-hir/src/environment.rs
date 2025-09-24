use indexmap::IndexSet;

use crate::SymbolId;

#[derive(Debug, Default, Clone)]
pub struct Environment {
    symbols: IndexSet<SymbolId>,
    nil_terminated: bool,
}

impl Environment {
    pub fn new(captures: &[SymbolId], parameters: &[SymbolId], nil_terminated: bool) -> Self {
        Self {
            symbols: captures.iter().chain(parameters).copied().collect(),
            nil_terminated,
        }
    }

    pub fn with_bindings(&self, bindings: &[SymbolId]) -> Self {
        Self {
            symbols: bindings
                .iter()
                .chain(self.symbols.iter())
                .copied()
                .collect(),
            nil_terminated: self.nil_terminated,
        }
    }

    pub fn try_path(&self, symbol_id: SymbolId) -> Option<u32> {
        let mut path = 1;

        let index = self.symbols.get_index_of(&symbol_id)?;

        if index + 1 < self.symbols.len() || self.nil_terminated {
            path *= 2;
        }

        for _ in 0..index {
            path *= 2;
            path += 1;
        }

        Some(path)
    }
}
