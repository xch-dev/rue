use crate::TypeId;

#[derive(Debug, Clone)]
pub struct Alias {
    pub inner: TypeId,
}

impl Alias {
    pub fn new(inner: TypeId) -> Self {
        Self { inner }
    }
}
