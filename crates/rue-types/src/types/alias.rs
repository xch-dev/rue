use rue_diagnostic::Name;

use crate::TypeId;

#[derive(Debug, Clone)]
pub struct Alias {
    pub name: Option<Name>,
    pub inner: TypeId,
    pub generics: Vec<TypeId>,
}
