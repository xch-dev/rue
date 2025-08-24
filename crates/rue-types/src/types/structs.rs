use indexmap::IndexSet;
use rue_parser::SyntaxToken;

use crate::TypeId;

#[derive(Debug, Clone)]
pub struct Struct {
    pub semantic: TypeId,
    pub inner: TypeId,
    pub name: Option<SyntaxToken>,
    pub generics: Vec<TypeId>,
    pub fields: IndexSet<String>,
}
