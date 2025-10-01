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
    Export,
    Extern,
    Inline,
    Test,
    Mod,
    Fn,
    Const,
    Type,
    Struct,
    Let,
    If,
    Else,
    Return,
    Assert,
    Raise,
    Is,
    As,

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
