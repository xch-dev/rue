use std::ops::Range;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub span: Range<usize>,
    pub kind: TokenKind,
}

impl Token {
    pub fn new(span: Range<usize>, kind: TokenKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Whitespace,
    LineComment,
    BlockComment { is_terminated: bool },

    // Literals
    String { is_terminated: bool },
    Hex { is_terminated: bool },
    Integer,
    Ident,

    // Keywords
    Nil,
    True,
    False,

    // Grouping
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,

    // Arithmetic operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,

    // Comparison operators
    Equals,
    LessThan,
    GreaterThan,

    // Logical operators
    Not,
    And,
    Or,

    // Bitwise operators
    Tilde,
    Xor,

    // Punctuation
    Dot,
    Comma,
    Colon,
    Semicolon,

    Unknown,
}
