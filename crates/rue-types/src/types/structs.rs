use indexmap::IndexSet;
use rue_parser::SyntaxToken;

use crate::TypeId;

#[derive(Debug, Clone)]
pub struct Struct {
    pub semantic: TypeId,
    pub inner: TypeId,
    pub token: Option<SyntaxToken>,
    pub generics: Vec<TypeId>,
    pub fields: IndexSet<String>,
}
