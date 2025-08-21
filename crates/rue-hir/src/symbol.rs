use id_arena::Id;
use rue_parser::SyntaxToken;

use crate::{HirId, ScopeId, TypeId};

pub type SymbolId = Id<Symbol>;

#[derive(Debug, Clone)]
pub enum Symbol {
    Function(FunctionSymbol),
    Parameter(ParameterSymbol),
    Binding(BindingSymbol),
}

#[derive(Debug, Clone)]
pub struct FunctionSymbol {
    pub name: Option<SyntaxToken>,
    pub ty: TypeId,
    pub scope: ScopeId,
    pub vars: Vec<TypeId>,
    pub parameters: Vec<SymbolId>,
    pub return_type: TypeId,
    pub body: HirId,
}

#[derive(Debug, Clone)]
pub struct ParameterSymbol {
    pub name: Option<SyntaxToken>,
    pub ty: TypeId,
}

#[derive(Debug, Clone)]
pub struct BindingSymbol {
    pub name: Option<SyntaxToken>,
    pub ty: TypeId,
    pub value: HirId,
}
