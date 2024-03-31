use num_bigint::BigInt;

use crate::database::SymbolId;

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Int(BigInt),
    Function(SymbolId),
    Reference(SymbolId),
    FunctionCall {
        callee: Box<Value>,
        args: Vec<Value>,
    },
    Add(Vec<Value>),
    Subtract(Vec<Value>),
    Multiply(Vec<Value>),
    Divide(Vec<Value>),
    LessThan(Box<Value>, Box<Value>),
    GreaterThan(Box<Value>, Box<Value>),
    If {
        condition: Box<Value>,
        then_block: Box<Value>,
        else_block: Box<Value>,
    },
}
