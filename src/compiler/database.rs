use id_arena::{Arena, Id};

use crate::Type;

pub type TypeId = Id<Type>;

#[derive(Debug, Default, Clone)]
pub struct Database {
    types: Arena<Type>,
}

impl Database {
    pub fn new() -> Self {
        Self::default()
    }
}
