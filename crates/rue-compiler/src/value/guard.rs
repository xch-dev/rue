use std::ops::Not;

use rue_typing::TypeId;

#[derive(Debug, Clone, Copy)]
pub struct Guard {
    pub then_type: TypeOverride,
    pub else_type: TypeOverride,
}

impl Guard {
    pub fn new(then_type: TypeOverride, else_type: TypeOverride) -> Self {
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

#[derive(Debug, Clone, Copy)]
pub struct TypeOverride {
    pub type_id: TypeId,
    pub mutation: Mutation,
}

impl TypeOverride {
    pub fn new(type_id: TypeId) -> Self {
        Self {
            type_id,
            mutation: Mutation::None,
        }
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub enum Mutation {
    #[default]
    None,
    UnwrapOptional,
}
