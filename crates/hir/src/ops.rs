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

    #[display("-")]
    Neg,

    #[display("~")]
    BitwiseNot,
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

    #[display("%")]
    Mod,

    #[display(">")]
    Gt,

    #[display("<")]
    Lt,

    #[display(">=")]
    Gte,

    #[display("<=")]
    Lte,

    #[display(">")]
    GtBytes,

    #[display("<")]
    LtBytes,

    #[display(">=")]
    GteBytes,

    #[display("<=")]
    LteBytes,

    #[display("==")]
    Eq,

    #[display("!=")]
    Ne,

    #[display("&&")]
    And,

    #[display("||")]
    Or,

    #[display("&")]
    All,

    #[display("|")]
    Any,

    #[display("&")]
    BitwiseAnd,

    #[display("|")]
    BitwiseOr,

    #[display("^")]
    BitwiseXor,

    #[display("<<")]
    LeftShift,

    #[display(">>")]
    RightShift,
}
