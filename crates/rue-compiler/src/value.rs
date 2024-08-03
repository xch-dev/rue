use std::ops::Not;

use rue_typing::HashMap;

use rue_typing::TypeId;
use rue_typing::TypePath;

use crate::HirId;
use crate::SymbolId;

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

    pub fn then_guards(&self) -> HashMap<GuardPath, TypeId> {
        self.guards
            .iter()
            .map(|(guard_path, guard)| (guard_path.clone(), guard.then_type))
            .collect()
    }

    pub fn else_guards(&self) -> HashMap<GuardPath, TypeId> {
        self.guards
            .iter()
            .map(|(guard_path, guard)| (guard_path.clone(), guard.else_type))
            .collect()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Guard {
    pub then_type: TypeId,
    pub else_type: TypeId,
}

impl Guard {
    pub fn new(then_type: TypeId, else_type: TypeId) -> Self {
        Self {
            then_type,
            else_type,
        }
    }
}

impl Not for Guard {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self {
            then_type: self.else_type,
            else_type: self.then_type,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GuardPath {
    pub symbol_id: SymbolId,
    pub items: Vec<TypePath>,
}

impl GuardPath {
    pub fn new(symbol_id: SymbolId) -> Self {
        Self {
            symbol_id,
            items: Vec::new(),
        }
    }
}
