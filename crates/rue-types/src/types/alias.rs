use rue_parser::SyntaxToken;

use crate::TypeId;

#[derive(Debug, Clone)]
pub struct Alias {
    pub inner: TypeId,
    pub token: Option<SyntaxToken>,
    pub generics: Vec<TypeId>,
}
