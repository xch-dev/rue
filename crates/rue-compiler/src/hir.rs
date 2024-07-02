use rowan::TextRange;

use crate::{
    database::{HirId, SymbolId},
    ScopeId,
};

#[derive(Debug, Clone)]
pub enum Hir {
    Unknown,
    Atom(Vec<u8>),
    Pair(HirId, HirId),
    Reference(SymbolId, TextRange),
    Definition(ScopeId, HirId),
    FunctionCall(HirId, Vec<HirId>, bool),
    Op(Op, HirId),
    BinaryOp(BinOp, HirId, HirId),
    Raise(Option<HirId>),
    If(HirId, HirId, HirId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Op {
    First,
    Rest,
    Sha256,
    Listp,
    Strlen,
    PubkeyForExp,
    Exists,
    Not,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    LessThan,
    GreaterThan,
    LessThanEquals,
    GreaterThanEquals,
    Equals,
    NotEquals,
    Concat,
    PointAdd,
    LogicalAnd,
    LogicalOr,
}
