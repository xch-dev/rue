use std::collections::HashMap;

mod guard;
mod ty;

pub use guard::*;
pub use ty::*;

use crate::{HirId, SymbolId, TypeId};

#[derive(Debug, Clone)]
pub struct Value {
    pub hir_id: HirId,
    pub type_id: TypeId,
    pub guards: Guards,
}

impl Value {
    pub fn new(hir_id: HirId, type_id: TypeId) -> Self {
        Self {
            hir_id,
            type_id,
            guards: HashMap::new(),
        }
    }

    pub fn then_guards(&self) -> HashMap<SymbolId, TypeId> {
        self.guards.iter().map(|(k, v)| (*k, v.then_type)).collect()
    }

    pub fn else_guards(&self) -> HashMap<SymbolId, TypeId> {
        self.guards.iter().map(|(k, v)| (*k, v.else_type)).collect()
    }
}
