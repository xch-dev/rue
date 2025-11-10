use indexmap::IndexSet;
use rue_diagnostic::Name;

use crate::TypeId;

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: Option<Name>,
    pub semantic: TypeId,
    pub inner: TypeId,
    pub generics: Vec<TypeId>,
    pub fields: IndexSet<String>,
    pub nil_terminated: bool,
}
