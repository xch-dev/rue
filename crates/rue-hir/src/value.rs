use std::collections::HashMap;

use rue_types::TypeId;

use crate::{HirId, SymbolId, TypePath};

pub type Mappings = HashMap<SymbolId, HashMap<Vec<TypePath>, TypeId>>;

#[derive(Debug, Clone)]
pub struct Value {
    pub hir: HirId,
    pub ty: TypeId,
    pub reference: Option<SymbolPath>,
    pub then_map: Mappings,
    pub else_map: Mappings,
}

impl Value {
    pub fn new(hir: HirId, ty: TypeId) -> Self {
        Self {
            hir,
            ty,
            reference: None,
            then_map: Mappings::new(),
            else_map: Mappings::new(),
        }
    }

    pub fn with_hir(self, hir: HirId) -> Self {
        Self { hir, ..self }
    }

    pub fn with_type(self, ty: TypeId) -> Self {
        Self { ty, ..self }
    }

    pub fn with_reference(self, reference: SymbolPath) -> Self {
        Self {
            reference: Some(reference),
            ..self
        }
    }

    pub fn with_mappings(self, then_map: Mappings, else_map: Mappings) -> Self {
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

#[derive(Debug, Clone)]
pub struct SymbolPath {
    pub symbol: SymbolId,
    pub path: Vec<TypePath>,
}

pub fn merge_mappings(a: &Mappings, b: &Mappings) -> Mappings {
    let mut map = Mappings::new();

    for (&symbol, mappings) in a.iter().chain(b.iter()) {
        for (path, &ty) in mappings {
            map.entry(symbol).or_default().insert(path.clone(), ty);
        }
    }

    map
}
