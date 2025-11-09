use rue_diagnostic::Source;
use rue_parser::SyntaxToken;

use crate::TypeId;

#[derive(Debug, Clone)]
pub struct Alias {
    pub inner: TypeId,
    pub name: Option<SyntaxToken>,
    pub source: Source,
    pub generics: Vec<TypeId>,
}
