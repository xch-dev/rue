use indexmap::IndexSet;

use crate::TypeId;

#[derive(Debug, Clone)]
pub struct Struct {
    pub semantic: TypeId,
    pub inner: TypeId,
    pub fields: IndexSet<String>,
}
