use derive_more::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Display)]
pub enum UnaryOp {
    #[display("listp")]
    Listp { can_be_truthy: bool },

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

    #[display("-")]
    G1Negate,

    #[display("-")]
    G2Negate,

    #[display("sha256")]
    Sha256,

    #[display("sha256")]
    Sha256Inline,

    #[display("keccak256")]
    Keccak256,

    #[display("keccak256")]
    Keccak256Inline,

    #[display("pubkey_for_exp")]
    PubkeyForExp,
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

    #[display("+")]
    Concat,

    #[display("+")]
    G1Add,

    #[display("-")]
    G1Subtract,

    #[display("*")]
    G1Multiply,

    #[display("+")]
    G2Add,

    #[display("-")]
    G2Subtract,

    #[display("*")]
    G2Multiply,

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
