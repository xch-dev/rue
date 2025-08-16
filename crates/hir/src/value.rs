use std::collections::HashMap;

use crate::{HirId, SymbolId, TypeId, TypePath};

#[derive(Debug, Clone)]
pub struct Value {
    pub hir: HirId,
    pub ty: TypeId,
    pub symbol: Option<SymbolId>,
    pub then_map: HashMap<SymbolId, HashMap<Vec<TypePath>, TypeId>>,
    pub else_map: HashMap<SymbolId, HashMap<Vec<TypePath>, TypeId>>,
}

impl Value {
    pub fn new(hir: HirId, ty: TypeId) -> Self {
        Self {
            hir,
            ty,
            symbol: None,
            then_map: HashMap::new(),
            else_map: HashMap::new(),
        }
    }

    pub fn with_hir(self, hir: HirId) -> Self {
        Self { hir, ..self }
    }

    pub fn with_type(self, ty: TypeId) -> Self {
        Self { ty, ..self }
    }

    pub fn with_symbol(self, symbol: SymbolId) -> Self {
        Self {
            symbol: Some(symbol),
            ..self
        }
    }

    pub fn with_mappings(
        self,
        then_map: HashMap<SymbolId, HashMap<Vec<TypePath>, TypeId>>,
        else_map: HashMap<SymbolId, HashMap<Vec<TypePath>, TypeId>>,
    ) -> Self {
        Self {
            then_map,
            else_map,
            ..self
        }
    }

    pub fn flip_mappings(self) -> Self {
        Self {
            then_map: self.else_map,
            else_map: self.then_map,
            ..self
        }
    }
}
