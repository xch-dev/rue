use rue_parser::BinaryOp;

use crate::database::{HirId, ScopeId, SymbolId};

#[derive(Debug, Clone)]
pub enum Hir {
    Unknown,
    Atom(Vec<u8>),
    Pair(HirId, HirId),
    Reference(SymbolId),
    Scope {
        scope_id: ScopeId,
        value: HirId,
    },
    FunctionCall {
        callee: HirId,
        args: HirId,
    },
    BinaryOp {
        op: HirBinaryOp,
        lhs: HirId,
        rhs: HirId,
    },
    First(HirId),
    Rest(HirId),
    Not(HirId),
    Raise(Option<HirId>),
    Sha256(HirId),
    IsCons(HirId),
    Strlen(HirId),
    If {
        condition: HirId,
        then_block: HirId,
        else_block: HirId,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HirBinaryOp {
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
}

impl From<BinaryOp> for HirBinaryOp {
    fn from(op: BinaryOp) -> Self {
        match op {
            BinaryOp::Add => HirBinaryOp::Add,
            BinaryOp::Subtract => HirBinaryOp::Subtract,
            BinaryOp::Multiply => HirBinaryOp::Multiply,
            BinaryOp::Divide => HirBinaryOp::Divide,
            BinaryOp::Remainder => HirBinaryOp::Remainder,
            BinaryOp::LessThan => HirBinaryOp::LessThan,
            BinaryOp::GreaterThan => HirBinaryOp::GreaterThan,
            BinaryOp::LessThanEquals => HirBinaryOp::LessThanEquals,
            BinaryOp::GreaterThanEquals => HirBinaryOp::GreaterThanEquals,
            BinaryOp::Equals => HirBinaryOp::Equals,
            BinaryOp::NotEquals => HirBinaryOp::NotEquals,
        }
    }
}
