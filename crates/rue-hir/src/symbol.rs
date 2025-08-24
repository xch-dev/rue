use id_arena::Id;
use rue_parser::SyntaxToken;
use rue_types::TypeId;

use crate::{HirId, ScopeId};

pub type SymbolId = Id<Symbol>;

#[derive(Debug, Clone)]
pub enum Symbol {
    Function(FunctionSymbol),
    Parameter(ParameterSymbol),
    Constant(ConstantSymbol),
    Binding(BindingSymbol),
}

#[derive(Debug, Clone)]
pub struct FunctionSymbol {
    pub name: Option<SyntaxToken>,
    pub ty: TypeId,
    pub scope: ScopeId,
    pub vars: Vec<TypeId>,
    pub parameters: Vec<SymbolId>,
    pub nil_terminated: bool,
    pub return_type: TypeId,
    pub body: HirId,
    pub inline: bool,
}

#[derive(Debug, Clone)]
pub struct ParameterSymbol {
    pub name: Option<SyntaxToken>,
    pub ty: TypeId,
}

#[derive(Debug, Clone)]
pub struct ConstantSymbol {
    pub name: Option<SyntaxToken>,
    pub ty: TypeId,
    pub value: HirId,
    pub inline: bool,
}

#[derive(Debug, Clone)]
pub struct BindingSymbol {
    pub name: Option<SyntaxToken>,
    pub ty: TypeId,
    pub value: HirId,
    pub inline: bool,
}
