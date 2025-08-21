use indexmap::IndexMap;

use crate::TypeId;

#[derive(Debug, Clone)]
pub struct Struct {
    pub semantic: TypeId,
    pub inner: TypeId,
    pub fields: IndexMap<String, TypeId>,
}
