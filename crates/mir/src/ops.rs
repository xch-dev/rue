#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Listp,
    First,
    Rest,
    Not,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}
