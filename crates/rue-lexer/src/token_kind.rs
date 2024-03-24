#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
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
    Whitespace,
    Unknown,
}
