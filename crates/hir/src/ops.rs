use derive_more::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Display)]
pub enum UnaryOp {
    #[display("listp")]
    Listp,

    #[display("first")]
    First,

    #[display("rest")]
    Rest,

    #[display("strlen")]
    Strlen,

    #[display("!")]
    Not,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Display)]
pub enum BinaryOp {
    #[display("+")]
    Add,

    #[display("-")]
    Sub,

    #[display("*")]
    Mul,

    #[display("/")]
    Div,

    #[display(">")]
    Gt,

    #[display("<")]
    Lt,

    #[display(">=")]
    Gte,

    #[display("<=")]
    Lte,

    #[display("==")]
    Eq,

    #[display("!=")]
    Ne,

    #[display("&&")]
    And,

    #[display("||")]
    Or,
}
