#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Listp,
    First,
    Rest,
    Strlen,
    Not,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    And,
    Or,
}
