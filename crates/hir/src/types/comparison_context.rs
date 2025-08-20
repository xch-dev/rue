use std::collections::HashMap;

use crate::{TypeId, TypePath};

#[derive(Debug, Clone)]
pub struct ComparisonContext {
    path: Vec<TypePath>,
    inferred: HashMap<TypeId, TypeId>,
}

impl ComparisonContext {
    pub fn new(inferred: HashMap<TypeId, TypeId>) -> Self {
        Self {
            path: vec![],
            inferred,
        }
    }

    pub fn push(&mut self, path: TypePath) {
        self.path.push(path);
    }

    pub fn pop(&mut self) {
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
