use std::collections::HashMap;

use crate::{HirId, TypeId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypePath {
    First,
    Rest,
}

#[derive(Debug, Clone)]
pub struct Constraint {
    pub hir: HirId,
    pub then_map: HashMap<Vec<TypePath>, TypeId>,
    pub else_map: HashMap<Vec<TypePath>, TypeId>,
}

impl Constraint {
    pub fn new(
        hir: HirId,
        then_map: HashMap<Vec<TypePath>, TypeId>,
        else_map: HashMap<Vec<TypePath>, TypeId>,
    ) -> Self {
        Self {
            hir,
            then_map,
            else_map,
        }
    }

    pub fn to(hir: HirId, from: Vec<TypePath>, to: TypeId) -> Self {
        let mut then_map = HashMap::new();
        then_map.insert(from, to);
        Self {
            hir,
            then_map,
            else_map: HashMap::new(),
        }
    }

    pub fn if_else(hir: HirId, from: Vec<TypePath>, to: TypeId, otherwise: TypeId) -> Self {
        let mut then_map = HashMap::new();
        then_map.insert(from.clone(), to);
        let mut else_map = HashMap::new();
        else_map.insert(from, otherwise);
        Self {
            hir,
            then_map,
            else_map,
        }
    }
}
