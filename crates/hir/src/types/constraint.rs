use crate::{Check, TypeId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypePath {
    First,
    Rest,
}

#[derive(Debug, Clone)]
pub struct Constraint {
    pub check: Check,
    pub else_id: Option<TypeId>,
}

impl Constraint {
    pub fn new(check: Check) -> Self {
        Self {
            check,
            else_id: None,
        }
    }

    pub fn with_else(self, else_id: TypeId) -> Self {
        Self {
            else_id: Some(else_id),
            ..self
        }
    }
}
