use rue_parser::BinaryOp;

use crate::database::{ScopeId, SymbolId};

#[derive(Debug, Clone)]
pub enum Value {
    Unknown,
    Atom(Vec<u8>),
    List(Vec<Value>),
    Reference(SymbolId),
    Scope {
        scope_id: ScopeId,
        value: Box<Value>,
    },
    FunctionCall {
        callee: Box<Value>,
        args: Vec<Value>,
    },
    BinaryOp {
        op: BinaryOp,
        lhs: Box<Value>,
        rhs: Box<Value>,
    },
    Not(Box<Value>),
    If {
        condition: Box<Value>,
        then_block: Box<Value>,
        else_block: Box<Value>,
    },
}
