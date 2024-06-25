use std::ops::Not;

use crate::TypeId;

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
