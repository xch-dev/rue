use crate::TypeId;

#[derive(Debug, Clone)]
pub struct Alias {
    pub inner: TypeId,
}
