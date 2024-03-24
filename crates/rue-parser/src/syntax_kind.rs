use num_derive::{FromPrimitive, ToPrimitive};

#[derive(
    Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, ToPrimitive, FromPrimitive,
)]
pub enum SyntaxKind {
    #[default]
    Eof,
    Error,
    Whitespace,
    Ident,
    Int,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Fun,
    Comma,
    Colon,
    Arrow,
    Root,
    FunctionItem,
    FunctionParamList,
    FunctionParam,
    Block,
}
