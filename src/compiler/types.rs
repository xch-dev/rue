use crate::{ScopeId, SyntaxToken, TypeId};

#[derive(Debug, Clone)]
pub enum Type {
    Unresolved,
    Binding(BindingType),
    Var(VarType),
}

#[derive(Debug, Clone)]
pub struct BindingType {
    pub name: Option<SyntaxToken>,
    pub scope: ScopeId,
    pub vars: Vec<TypeId>,
    pub parent: TypeId,
    pub is_transparent: bool,
    pub has_constraint: bool,
}

#[derive(Debug, Clone)]
pub struct VarType {
    pub name: Option<SyntaxToken>,
}
