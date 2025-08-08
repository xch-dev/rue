use crate::{HirId, SymbolId};

#[derive(Debug, Clone)]
pub enum Hir {
    Unresolved,
    Block(Block),
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub body: Option<HirId>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let(SymbolId),
    Expr(HirId),
}
