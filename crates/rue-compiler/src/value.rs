use std::collections::HashMap;

mod guard;
mod guard_path;
mod ty;

pub use guard::*;
pub use guard_path::*;
pub use ty::*;

use crate::{HirId, TypeId};

#[derive(Debug, Clone)]
pub struct Value {
    pub hir_id: HirId,
    pub type_id: TypeId,
    pub guards: HashMap<GuardPath, Guard>,
    pub guard_path: Option<GuardPath>,
}

impl Value {
    pub fn new(hir_id: HirId, type_id: TypeId) -> Self {
        Self {
            hir_id,
            type_id,
            guards: HashMap::new(),
            guard_path: None,
        }
    }

    pub fn then_guards(&self) -> HashMap<GuardPath, TypeOverride> {
        self.guards
            .iter()
            .map(|(k, v)| (k.clone(), v.then_type))
            .collect()
    }

    pub fn else_guards(&self) -> HashMap<GuardPath, TypeOverride> {
        self.guards
            .iter()
            .map(|(k, v)| (k.clone(), v.else_type))
            .collect()
    }

    pub fn extend_guard_path(mut self, old_value: Value, item: GuardPathItem) -> Self {
        match old_value.guard_path {
            Some(mut path) => {
                path.items.push(item);
                self.guard_path = Some(path);
                self
            }
            None => self,
        }
    }
}
