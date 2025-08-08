use std::collections::HashMap;

use indexmap::IndexMap;

use crate::{HirId, ScopeId, SymbolId, SyntaxToken, TypeId};

#[derive(Debug, Clone)]
pub enum Type {
    Unresolved,
    Var(Var),
    Alias(Alias),
    Subtype(Subtype),
    Union(Vec<TypeId>),
    Apply(TypeId, HashMap<TypeId, TypeId>),
}

#[derive(Debug, Clone)]
pub struct Var {
    pub name: Option<SyntaxToken>,
}

#[derive(Debug, Clone)]
pub struct Alias {
    pub name: Option<SyntaxToken>,
    pub scope: ScopeId,
    pub vars: Vec<TypeId>,
    pub inner: TypeId,
}

#[derive(Debug, Clone)]
pub struct Subtype {
    pub name: Option<SyntaxToken>,
    pub scope: ScopeId,
    pub vars: Vec<SubtypeVar>,
    pub parent: Option<SubtypeParent>,
    pub fields: IndexMap<String, SubtypeField>,
    pub constraint: Option<HirId>,
}

#[derive(Debug, Clone)]
pub struct SubtypeVar {
    pub ty: TypeId,
    pub field: String,
}

#[derive(Debug, Clone)]
pub struct SubtypeParent {
    pub symbol: SymbolId,
    pub ty: TypeId,
}

#[derive(Debug, Clone)]
pub struct SubtypeField {
    pub name: SyntaxToken,
    pub ty: TypeId,
    pub accessor: HirId,
}
