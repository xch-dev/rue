use crate::{SymbolId, TypeId};

#[derive(Debug, Clone)]
pub struct Declarations {
    pub type_ids: Vec<TypeId>,
    pub symbol_ids: Vec<SymbolId>,
    pub exported_types: Vec<TypeId>,
    pub exported_symbols: Vec<SymbolId>,
}
