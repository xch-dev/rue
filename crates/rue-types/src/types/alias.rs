use rue_parser::SyntaxToken;

use crate::TypeId;

#[derive(Debug, Clone)]
pub struct Alias {
    pub inner: TypeId,
    pub name: Option<SyntaxToken>,
    pub generics: Vec<TypeId>,
}
