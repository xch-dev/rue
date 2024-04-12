#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    Ident,
    Int,
    String { is_terminated: bool },

    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,

    Fun,
    Type,
    Struct,
    Let,
    Const,
    If,
    Else,
    Nil,
    True,
    False,

    Dot,
    Comma,
    Colon,
    Semicolon,
    Arrow,
    FatArrow,
    Spread,

    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Not,
    LessThan,
    GreaterThan,
    LessThanEquals,
    GreaterThanEquals,
    Equals,
    NotEquals,
    Assign,

    Whitespace,
    LineComment,
    BlockComment { is_terminated: bool },
    Unknown,
}
