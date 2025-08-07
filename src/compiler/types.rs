use crate::{SyntaxToken, TypeId};

#[derive(Debug, Clone)]
pub enum Type {
    Unresolved(UnresolvedType),
    Generic(GenericType),
    Var(VarType),
}

#[derive(Debug, Clone)]
pub struct UnresolvedType {
    pub name: Option<SyntaxToken>,
}

#[derive(Debug, Clone)]
pub struct GenericType {
    pub name: Option<SyntaxToken>,
    pub vars: Vec<TypeId>,
    pub body: TypeId,
}

#[derive(Debug, Clone)]
pub struct VarType {
    pub name: Option<SyntaxToken>,
}
