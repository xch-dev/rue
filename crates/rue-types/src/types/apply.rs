use std::collections::HashMap;

use crate::TypeId;

#[derive(Debug, Clone)]
pub struct Apply {
    pub inner: TypeId,
    pub generics: HashMap<TypeId, TypeId>,
}
