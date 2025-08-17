use id_arena::Id;
use rue_parser::SyntaxToken;

use crate::{Atom, ScopeId};

pub type TypeId = Id<Type>;

#[derive(Debug, Clone)]
pub enum Type {
    Unresolved,
    Atom(Atom),
    Pair(TypeId, TypeId),
    Generic(Generic),
    Alias(Alias),
    Union(Vec<TypeId>),
    Fn(FunctionType),
}

#[derive(Debug, Clone)]
pub struct Generic {
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
pub struct FunctionType {
    pub params: Vec<TypeId>,
    pub ret: TypeId,
}
