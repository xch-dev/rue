use crate::{SymbolId, TypeId};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PathItem {
    Symbol(SymbolId),
    Type(TypeId),
}
