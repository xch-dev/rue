use derive_more::Display;
use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};
use rowan::Language;

#[derive(
    Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, ToPrimitive, FromPrimitive,
)]
pub enum SyntaxKind {
    // Trivia
    #[display("whitespace")]
    Whitespace,

    #[display("line comment")]
    LineComment,

    #[display("block comment")]
    BlockComment,

    // Literals
    #[display("string literal")]
    String,

    #[display("hex literal")]
    Hex,

    #[display("integer literal")]
    Integer,

    #[display("identifier")]
    Ident,

    // Keywords
    #[display("`nil`")]
    Nil,

    #[display("`true`")]
    True,

    #[display("`false`")]
    False,

    // Grouping
    #[display("`(`")]
    OpenParen,

    #[display("`)`")]
    CloseParen,

    #[display("`{{`")]
    OpenBrace,

    #[display("`}}`")]
    CloseBrace,

    #[display("`[`")]
    OpenBracket,

    #[display("`]`")]
    CloseBracket,

    // Arithmetic operators
    #[display("`+`")]
    Plus,

    #[display("`-`")]
    Minus,

    #[display("`*`")]
    Star,

    #[display("`/`")]
    Slash,

    #[display("`%`")]
    Percent,

    // Comparison operators
    #[display("`==`")]
    Equals,

    #[display("`!=`")]
    NotEquals,

    #[display("`<`")]
    LessThan,

    #[display("`>`")]
    GreaterThan,

    #[display("`<=`")]
    LessThanEquals,

    #[display("`>=`")]
    GreaterThanEquals,

    // Logical operators
    #[display("`!`")]
    Not,

    #[display("`&&`")]
    And,

    #[display("`||`")]
    Or,

    // Bitwise operators
    #[display("`~`")]
    BitwiseNot,

    #[display("`&`")]
    BitwiseAnd,

    #[display("`|`")]
    BitwiseOr,

    #[display("`^`")]
    BitwiseXor,

    #[display("`<<`")]
    LeftShift,

    #[display("`>>`")]
    RightShift,

    // Punctuation
    #[display("`=`")]
    Assign,

    #[display("`.`")]
    Dot,

    #[display("`,`")]
    Comma,

    #[display("`:`")]
    Colon,

    #[display("`;`")]
    Semicolon,

    // Unexpected cases
    #[display("error")]
    Error,

    #[display("eof")]
    Eof,
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

impl SyntaxKind {
    pub fn is_trivia(&self) -> bool {
        matches!(
            self,
            Self::Whitespace | Self::LineComment | Self::BlockComment | Self::Error
        )
    }

    pub fn split(&self) -> &'static [Self] {
        match self {
            SyntaxKind::Whitespace => &[Self::Whitespace],
            SyntaxKind::LineComment => &[Self::LineComment],
            SyntaxKind::BlockComment => &[Self::BlockComment],
            SyntaxKind::String => &[Self::String],
            SyntaxKind::Hex => &[Self::Hex],
            SyntaxKind::Integer => &[Self::Integer],
            SyntaxKind::Ident => &[Self::Ident],
            T![nil] => &[T![nil]],
            T![true] => &[T![true]],
            T![false] => &[T![false]],
            T!['('] => &[T!['(']],
            T![')'] => &[T![')']],
            T!['{'] => &[T!['{']],
            T!['}'] => &[T!['}']],
            T!['['] => &[T!['[']],
            T![']'] => &[T![']']],
            T![+] => &[T![+]],
            T![-] => &[T![-]],
            T![*] => &[T![*]],
            T![/] => &[T![/]],
            T![%] => &[T![%]],
            T![==] => &[T![==]],
            T![!=] => &[T![!], T![=]],
            T![<] => &[T![<]],
            T![>] => &[T![>]],
            T![<=] => &[T![<], T![=]],
            T![>=] => &[T![>], T![=]],
            T![!] => &[T![!]],
            T![&&] => &[T![&], T![&]],
            T![||] => &[T![|], T![|]],
            T![~] => &[T![~]],
            T![&] => &[T![&]],
            T![|] => &[T![|]],
            T![^] => &[T![^]],
            T![<<] => &[T![<], T![<]],
            T![>>] => &[T![>], T![>]],
            T![=] => &[T![=]],
            T![.] => &[T![.]],
            T![,] => &[T![,]],
            T![:] => &[T![:]],
            T![;] => &[T![;]],
            SyntaxKind::Error => &[Self::Error],
            SyntaxKind::Eof => &[Self::Eof],
        }
    }
}
