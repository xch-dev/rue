use crate::SyntaxToken;

#[derive(Debug, Clone)]
pub enum Type {
    Unresolved(UnresolvedType),
}

#[derive(Debug, Clone)]
pub struct UnresolvedType {
    pub name: Option<SyntaxToken>,
}
