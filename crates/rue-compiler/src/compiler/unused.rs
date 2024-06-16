use indexmap::IndexSet;

use crate::{SymbolId, TypeId};

#[derive(Default)]
pub struct Unused {
    pub symbol_ids: IndexSet<SymbolId>,
    pub type_ids: IndexSet<TypeId>,
    pub exempt_symbols: IndexSet<SymbolId>,
    pub exempt_types: IndexSet<TypeId>,
}
