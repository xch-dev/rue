use std::collections::HashMap;

use id_arena::Id;
use indexmap::IndexMap;
use rue_parser::SyntaxToken;

use crate::{HirId, ScopeId, SymbolId};

pub type TypeId = Id<Type>;

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
    pub parent: SubtypeParent,
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
