use rue_parser::BinaryOp;

use crate::database::SymbolId;

#[derive(Debug, Clone)]
pub enum Value {
    Atom(Vec<u8>),
    List(Vec<Value>),
    Reference(SymbolId),
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
