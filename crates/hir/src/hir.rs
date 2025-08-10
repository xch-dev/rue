use id_arena::Id;
use rue_mir::{BinaryOp, UnaryOp};

use crate::SymbolId;

pub type HirId = Id<Hir>;

#[derive(Debug, Clone)]
pub enum Hir {
    Unresolved,
    Atom(Vec<u8>),
    Reference(SymbolId),
    Block(Block),
    Unary(UnaryOp, HirId),
    Binary(BinaryOp, HirId, HirId),
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub body: Option<HirId>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(HirId),
}
