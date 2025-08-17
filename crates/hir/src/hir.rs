use id_arena::Id;
use num_bigint::BigInt;

use crate::{BinaryOp, SymbolId, UnaryOp};

pub type HirId = Id<Hir>;

#[derive(Debug, Clone)]
pub enum Hir {
    Unresolved,
    Nil,
    String(String),
    Int(BigInt),
    Bytes(Vec<u8>),
    Bool(bool),
    Pair(HirId, HirId),
    Reference(SymbolId),
    Block(Block),
    If(HirId, HirId, HirId),
    FunctionCall(HirId, Vec<HirId>),
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
    Let(SymbolId),
    If(HirId, HirId),
    Return(HirId),
    Assert(HirId),
    Raise(HirId),
}
