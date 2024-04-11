use rue_parser::BinaryOp;

use crate::database::{HirId, ScopeId, SymbolId};

#[derive(Debug, Clone)]
pub enum Hir {
    Unknown,
    Atom(Vec<u8>),
    List {
        items: Vec<HirId>,
        nil_terminated: bool,
    },
    ListIndex {
        value: HirId,
        index: u32,
    },
    TupleIndex {
        value: HirId,
        index: u32,
        len: u32,
    },
    Reference(SymbolId),
    Scope {
        scope_id: ScopeId,
        value: HirId,
    },
    FunctionCall {
        callee: HirId,
        args: Vec<HirId>,
    },
    BinaryOp {
        op: BinaryOp,
        lhs: HirId,
        rhs: HirId,
    },
    Not(HirId),
    If {
        condition: HirId,
        then_block: HirId,
        else_block: HirId,
    },
}
