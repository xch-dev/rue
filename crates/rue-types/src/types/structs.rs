use indexmap::IndexSet;
use rue_diagnostic::Source;
use rue_parser::SyntaxToken;

use crate::TypeId;

#[derive(Debug, Clone)]
pub struct Struct {
    pub semantic: TypeId,
    pub inner: TypeId,
    pub name: Option<SyntaxToken>,
    pub source: Source,
    pub generics: Vec<TypeId>,
    pub fields: IndexSet<String>,
    pub nil_terminated: bool,
}
