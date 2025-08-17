use std::collections::HashMap;

use crate::{HirId, TypeId, TypePath};

#[derive(Debug, Clone)]
pub struct ComparisonContext {
    hir: Vec<HirId>,
    path: Vec<TypePath>,
    inferred: HashMap<TypeId, TypeId>,
}

impl ComparisonContext {
    pub fn new(hir: HirId, inferred: HashMap<TypeId, TypeId>) -> Self {
        Self {
            hir: vec![hir],
            path: vec![],
            inferred,
        }
    }

    pub fn hir(&self) -> HirId {
        *self.hir.last().unwrap()
    }

    pub fn push_first(&mut self, hir: HirId) {
        self.hir.push(hir);
        self.path.push(TypePath::First);
    }

    pub fn push_rest(&mut self, hir: HirId) {
        self.hir.push(hir);
        self.path.push(TypePath::Rest);
    }

    pub fn pop_hir(&mut self) {
        self.hir.pop().unwrap();
        self.path.pop().unwrap();
    }

    pub fn path(&self) -> Vec<TypePath> {
        self.path.clone()
    }

    pub fn into_inferred(self) -> HashMap<TypeId, TypeId> {
        self.inferred
    }

    pub fn inferred(&self, id: TypeId) -> Option<TypeId> {
        self.inferred.get(&id).copied()
    }

    pub fn infer(&mut self, generic: TypeId, concrete: TypeId) {
        self.inferred.insert(generic, concrete);
    }
}
