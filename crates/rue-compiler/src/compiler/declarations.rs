use crate::{SymbolId, TypeId};

pub struct Declarations {
    pub type_ids: Vec<TypeId>,
    pub symbol_ids: Vec<SymbolId>,
}
