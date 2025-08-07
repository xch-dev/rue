use indexmap::IndexMap;

use crate::TypeId;

#[derive(Debug, Default, Clone)]
pub struct Scope {
    types: IndexMap<String, TypeId>,
}

impl Scope {
    pub fn new() -> Self {
        Self::default()
    }
}
