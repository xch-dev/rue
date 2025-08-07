use crate::SyntaxToken;

#[derive(Debug, Clone)]
pub enum Symbol {
    Function(FunctionSymbol),
}

#[derive(Debug, Clone)]
pub struct FunctionSymbol {
    pub name: Option<SyntaxToken>,
}
