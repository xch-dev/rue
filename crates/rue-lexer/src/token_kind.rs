#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
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
    Unknown,
}
