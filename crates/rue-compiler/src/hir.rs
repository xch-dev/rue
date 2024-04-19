use crate::database::{HirId, ScopeId, SymbolId};

#[derive(Debug, Clone)]
pub enum Hir {
    Unknown,
    Atom(Vec<u8>),
    Pair(HirId, HirId),
    Reference(SymbolId),
    Scope {
        scope_id: ScopeId,
        hir_id: HirId,
    },
    FunctionCall {
        callee: HirId,
        args: HirId,
    },
    BinaryOp {
        op: BinOp,
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
    PubkeyForExp(HirId),
    If {
        condition: HirId,
        then_block: HirId,
        else_block: HirId,
    },
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
}
