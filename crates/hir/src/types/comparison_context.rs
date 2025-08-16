use std::collections::HashMap;

use crate::{HirId, TypeId, TypePath};

#[derive(Debug, Clone)]
pub struct ComparisonContext {
    hir: Vec<HirId>,
    path: Vec<TypePath>,
    from_map: Vec<HashMap<TypeId, TypeId>>,
    to_map: Vec<HashMap<TypeId, TypeId>>,
    inferred: HashMap<TypeId, TypeId>,
}

impl ComparisonContext {
    pub fn new(hir: HirId, inferred: HashMap<TypeId, TypeId>) -> Self {
        Self {
            hir: vec![hir],
            path: vec![],
            from_map: vec![HashMap::new()],
            to_map: vec![HashMap::new()],
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

    pub fn from(&self, from_id: TypeId) -> Option<TypeId> {
        self.from_map.last().unwrap().get(&from_id).copied()
    }

    pub fn to(&self, to_id: TypeId) -> Option<TypeId> {
        self.to_map.last().unwrap().get(&to_id).copied()
    }

    pub fn inferred(&self, id: TypeId) -> Option<TypeId> {
        self.inferred.get(&id).copied()
    }

    pub fn push_from_map(&mut self, map: HashMap<TypeId, TypeId>) {
        self.from_map.push(map);
    }

    pub fn pop_from_map(&mut self) {
        self.from_map.pop().unwrap();
    }

    pub fn push_to_map(&mut self, map: HashMap<TypeId, TypeId>) {
        self.to_map.push(map);
    }

    pub fn pop_to_map(&mut self) {
        self.to_map.pop().unwrap();
    }

    pub fn infer(&mut self, generic: TypeId, concrete: TypeId) {
        self.inferred.insert(generic, concrete);
    }
}
