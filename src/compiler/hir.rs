use crate::{HirId, SymbolId, Value};

#[derive(Debug, Clone)]
pub enum Hir {
    Unresolved,
    Atom(Vec<u8>),
    Unary(UnaryOp, HirId),
    Binary(BinaryOp, HirId, HirId),
    Block(Block),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Listp,
    Not,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub body: Option<Value>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let(SymbolId),
    Expr(Value),
}
