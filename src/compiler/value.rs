use crate::{HirId, TypeId};

#[derive(Debug, Clone)]
pub struct Value {
    pub hir: HirId,
    pub ty: TypeId,
}

impl Value {
    pub fn new(hir: HirId, ty: TypeId) -> Self {
        Self { hir, ty }
    }
}
