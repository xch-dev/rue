use crate::TypeId;

#[derive(Debug, Clone)]
pub struct Pair {
    pub first: TypeId,
    pub rest: TypeId,
}

impl Pair {
    pub fn new(first: TypeId, rest: TypeId) -> Self {
        Self { first, rest }
    }
}
