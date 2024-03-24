use num_derive::{FromPrimitive, ToPrimitive};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, ToPrimitive, FromPrimitive)]
pub enum SyntaxKind {
    Ident,
    Int,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Fun,
    Minus,
    GreaterThan,
    Whitespace,
    Error,
}
