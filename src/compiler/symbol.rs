use crate::{ScopeId, SymbolId, SyntaxToken, TypeId};

#[derive(Debug, Clone)]
pub enum Symbol {
    Function(FunctionSymbol),
    Binding(BindingSymbol),
}

#[derive(Debug, Clone)]
pub struct FunctionSymbol {
    pub name: Option<SyntaxToken>,
    pub scope: ScopeId,
    pub parameters: Vec<SymbolId>,
    pub return_type: TypeId,
}

#[derive(Debug, Clone)]
pub struct BindingSymbol {
    pub name: Option<SyntaxToken>,
    pub ty: TypeId,
}
