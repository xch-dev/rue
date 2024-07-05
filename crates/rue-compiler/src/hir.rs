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
    Not,
    BitwiseNot,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOp {
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    LessThan,
    GreaterThan,
    LessThanEquals,
    GreaterThanEquals,
    LessThanBytes,
    GreaterThanBytes,
    LessThanBytesEquals,
    GreaterThanBytesEquals,
    Equals,
    NotEquals,
    Concat,
    PointAdd,
    LogicalAnd,
    LogicalOr,
}
