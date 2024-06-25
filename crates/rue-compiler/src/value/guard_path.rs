use crate::SymbolId;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GuardPath {
    pub symbol_id: SymbolId,
    pub items: Vec<GuardPathItem>,
}

impl GuardPath {
    pub fn new(symbol_id: SymbolId) -> Self {
        Self {
            symbol_id,
            items: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GuardPathItem {
    Field(String),
    First,
    Rest,
}
