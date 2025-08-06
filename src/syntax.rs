use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};
use rowan::Language;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, ToPrimitive, FromPrimitive)]
pub enum SyntaxKind {
    Whitespace,
    LineComment,
    BlockComment,

    // Literals
    String,
    Bytes,
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
    NotEquals,
    LessThan,
    GreaterThan,
    LessThanEquals,
    GreaterThanEquals,

    // Logical operators
    Not,
    And,
    Or,

    // Bitwise operators
    BitwiseNot,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LeftShift,
    RightShift,

    // Punctuation
    Assign,
    Dot,
    Comma,
    Colon,
    Semicolon,

    Error,
}

pub type SyntaxNode = rowan::SyntaxNode<RueLang>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<RueLang>;
pub type SyntaxElement = rowan::SyntaxElement<RueLang>;
pub type SyntaxElementChildren = rowan::SyntaxElementChildren<RueLang>;
pub type SyntaxToken = rowan::SyntaxToken<RueLang>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RueLang;

impl Language for RueLang {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        SyntaxKind::from_u16(raw.0).expect("invalid syntax kind")
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind.to_u16().expect("invalid syntax kind"))
    }
}

#[macro_export]
macro_rules! T {
    [nil] => { SyntaxKind::Nil };
    [true] => { SyntaxKind::True };
    [false] => { SyntaxKind::False };
    ['('] => { SyntaxKind::OpenParen };
    [')'] => { SyntaxKind::CloseParen };
    ['{'] => { SyntaxKind::OpenBrace };
    ['}'] => { SyntaxKind::CloseBrace };
    ['['] => { SyntaxKind::OpenBracket };
    [']'] => { SyntaxKind::CloseBracket };
    [+] => { SyntaxKind::Plus };
    [-] => { SyntaxKind::Minus };
    [*] => { SyntaxKind::Star };
    [/] => { SyntaxKind::Slash };
    [%] => { SyntaxKind::Percent };
    [==] => { SyntaxKind::Equals };
    [!=] => { SyntaxKind::NotEquals };
    [<] => { SyntaxKind::LessThan };
    [>] => { SyntaxKind::GreaterThan };
    [<=] => { SyntaxKind::LessThanEquals };
    [>=] => { SyntaxKind::GreaterThanEquals };
    [!] => { SyntaxKind::Not };
    [&&] => { SyntaxKind::And };
    [||] => { SyntaxKind::Or };
    [&] => { SyntaxKind::BitwiseAnd };
    [|] => { SyntaxKind::BitwiseOr };
    [~] => { SyntaxKind::BitwiseNot };
    [^] => { SyntaxKind::BitwiseXor };
    [<<] => { SyntaxKind::LeftShift };
    [>>] => { SyntaxKind::RightShift };
    [=] => { SyntaxKind::Assign };
    [.] => { SyntaxKind::Dot };
    [,] => { SyntaxKind::Comma };
    [:] => { SyntaxKind::Colon };
    [;] => { SyntaxKind::Semicolon };
}
