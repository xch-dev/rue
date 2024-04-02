use crate::database::SymbolId;

#[derive(Debug, Clone)]
pub enum Value {
    Atom(Vec<u8>),
    List(Vec<Value>),
    Function(SymbolId),
    Reference(SymbolId),
    FunctionCall {
        callee: Box<Value>,
        args: Vec<Value>,
    },
    Add(Vec<Value>),
    Subtract(Vec<Value>),
    Multiply(Vec<Value>),
    Divide(Box<Value>, Box<Value>),
    Remainder(Box<Value>, Box<Value>),
    LessThan(Box<Value>, Box<Value>),
    GreaterThan(Box<Value>, Box<Value>),
    LessThanEquals(Box<Value>, Box<Value>),
    GreaterThanEquals(Box<Value>, Box<Value>),
    Equals(Box<Value>, Box<Value>),
    NotEquals(Box<Value>, Box<Value>),
    If {
        condition: Box<Value>,
        then_block: Box<Value>,
        else_block: Box<Value>,
    },
}
