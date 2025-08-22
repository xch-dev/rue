use crate::TypeId;

#[derive(Debug, Clone)]
pub struct Union {
    pub types: Vec<TypeId>,
}

impl Union {
    pub fn new(types: Vec<TypeId>) -> Self {
        Self { types }
    }
}
