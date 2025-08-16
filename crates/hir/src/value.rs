use std::collections::HashMap;

use crate::{HirId, TypeId};

#[derive(Debug, Clone)]
pub struct Value {
    pub hir: HirId,
    pub ty: TypeId,
    pub then_map: HashMap<TypeId, TypeId>,
    pub else_map: HashMap<TypeId, TypeId>,
}

impl Value {
    pub fn unmapped(hir: HirId, ty: TypeId) -> Self {
        Self {
            hir,
            ty,
            then_map: HashMap::new(),
            else_map: HashMap::new(),
        }
    }

    pub fn with_mappings(
        hir: HirId,
        ty: TypeId,
        then_map: HashMap<TypeId, TypeId>,
        else_map: HashMap<TypeId, TypeId>,
    ) -> Self {
        Self {
            hir,
            ty,
            then_map,
            else_map,
        }
    }
}
