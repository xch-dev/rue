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

    #[display("`fn`")]
    Fn,

    #[display("`const`")]
    Const,

    #[display("`type`")]
    Type,

    #[display("`subtype`")]
    Subtype,

    #[display("`let`")]
    Let,

    #[display("`if`")]
    If,

    #[display("`else`")]
    Else,

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

    #[display("`->`")]
    Arrow,

    #[display("`::`")]
    PathSeparator,

    // AST
    #[display("document")]
    Document,

    #[display("function")]
    FunctionItem,

    #[display("function parameter")]
    FunctionParameter,

    #[display("type alias")]
    TypeAliasItem,

    #[display("subtype")]
    SubtypeItem,

    #[display("subtype generic parameters")]
    SubtypeGenericParameters,

    #[display("subtype generic parameter")]
    SubtypeGenericParameter,

    #[display("subtype parameter")]
    SubtypeParameter,

    #[display("subtype constraint")]
    SubtypeConstraint,

    #[display("subtype fields")]
    SubtypeFields,

    #[display("subtype field")]
    SubtypeField,

    #[display("generic parameters")]
    GenericParameters,

    #[display("generic arguments")]
    GenericArguments,

    #[display("path type")]
    PathType,

    #[display("path type segment")]
    PathTypeSegment,

    #[display("union type")]
    UnionType,

    #[display("block")]
    Block,

    #[display("let statement")]
    LetStmt,

    #[display("expression statement")]
    ExprStmt,

    #[display("path expression")]
    PathExpr,

    #[display("path expression segment")]
    PathExprSegment,

    #[display("literal expression")]
    LiteralExpr,

    #[display("group expression")]
    GroupExpr,

    #[display("prefix expression")]
    PrefixExpr,

    #[display("binary expression")]
    BinaryExpr,

    #[display("function call expression")]
    FunctionCallExpr,

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
    [nil] => { $crate::SyntaxKind::Nil };
    [true] => { $crate::SyntaxKind::True };
    [false] => { $crate::SyntaxKind::False };
    [fn] => { $crate::SyntaxKind::Fn };
    [const] => { $crate::SyntaxKind::Const };
    [type] => { $crate::SyntaxKind::Type };
    [subtype] => { $crate::SyntaxKind::Subtype };
    [let] => { $crate::SyntaxKind::Let };
    [if] => { $crate::SyntaxKind::If };
    [else] => { $crate::SyntaxKind::Else };
    ['('] => { $crate::SyntaxKind::OpenParen };
    [')'] => { $crate::SyntaxKind::CloseParen };
    ['{'] => { $crate::SyntaxKind::OpenBrace };
    ['}'] => { $crate::SyntaxKind::CloseBrace };
    ['['] => { $crate::SyntaxKind::OpenBracket };
    [']'] => { $crate::SyntaxKind::CloseBracket };
    [+] => { $crate::SyntaxKind::Plus };
    [-] => { $crate::SyntaxKind::Minus };
    [*] => { $crate::SyntaxKind::Star };
    [/] => { $crate::SyntaxKind::Slash };
    [%] => { $crate::SyntaxKind::Percent };
    [==] => { SyntaxKind::Equals };
    [!=] => { SyntaxKind::NotEquals };
    [<] => { SyntaxKind::LessThan };
    [>] => { SyntaxKind::GreaterThan };
    [<=] => { $crate::SyntaxKind::LessThanEquals };
    [>=] => { $crate::SyntaxKind::GreaterThanEquals };
    [!] => { $crate::SyntaxKind::Not };
    [&&] => { $crate::SyntaxKind::And };
    [||] => { $crate::SyntaxKind::Or };
    [&] => { $crate::SyntaxKind::BitwiseAnd };
    [|] => { $crate::SyntaxKind::BitwiseOr };
    [~] => { $crate::SyntaxKind::BitwiseNot };
    [^] => { $crate::SyntaxKind::BitwiseXor };
    [<<] => { $crate::SyntaxKind::LeftShift };
    [>>] => { $crate::SyntaxKind::RightShift };
    [=] => { $crate::SyntaxKind::Assign };
    [.] => { $crate::SyntaxKind::Dot };
    [,] => { $crate::SyntaxKind::Comma };
    [:] => { $crate::SyntaxKind::Colon };
    [;] => { $crate::SyntaxKind::Semicolon };
    [->] => { $crate::SyntaxKind::Arrow };
    [::] => { $crate::SyntaxKind::PathSeparator };
}

impl SyntaxKind {
    pub const LITERAL_EXPR: &[Self] = &[
        SyntaxKind::String,
        SyntaxKind::Hex,
        SyntaxKind::Integer,
        T![nil],
        T![true],
        T![false],
    ];
    pub const PREFIX_OPS: &[Self] = &[T![!], T![-], T![+], T![~]];
    pub const BINARY_OPS: &[Self] = &[
        T![+],
        T![-],
        T![*],
        T![/],
        T![%],
        T![<],
        T![>],
        T![<=],
        T![>=],
        T![==],
        T![!=],
        T![&&],
        T![||],
        T![&],
        T![|],
        T![^],
        T![<<],
        T![>>],
    ];

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
            T![fn] => &[T![fn]],
            T![const] => &[T![const]],
            T![type] => &[T![type]],
            T![subtype] => &[T![subtype]],
            T![let] => &[T![let]],
            T![if] => &[T![if]],
            T![else] => &[T![else]],
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
            T![->] => &[T![-], T![>]],
            T![::] => &[T![:], T![:]],
            SyntaxKind::Document => &[SyntaxKind::Document],
            SyntaxKind::FunctionItem => &[SyntaxKind::FunctionItem],
            SyntaxKind::FunctionParameter => &[SyntaxKind::FunctionParameter],
            SyntaxKind::TypeAliasItem => &[SyntaxKind::TypeAliasItem],
            SyntaxKind::SubtypeItem => &[SyntaxKind::SubtypeItem],
            SyntaxKind::SubtypeGenericParameters => &[SyntaxKind::SubtypeGenericParameters],
            SyntaxKind::SubtypeGenericParameter => &[SyntaxKind::SubtypeGenericParameter],
            SyntaxKind::SubtypeParameter => &[SyntaxKind::SubtypeParameter],
            SyntaxKind::SubtypeConstraint => &[SyntaxKind::SubtypeConstraint],
            SyntaxKind::SubtypeFields => &[SyntaxKind::SubtypeFields],
            SyntaxKind::SubtypeField => &[SyntaxKind::SubtypeField],
            SyntaxKind::GenericParameters => &[SyntaxKind::GenericParameters],
            SyntaxKind::GenericArguments => &[SyntaxKind::GenericArguments],
            SyntaxKind::PathType => &[SyntaxKind::PathType],
            SyntaxKind::PathTypeSegment => &[SyntaxKind::PathTypeSegment],
            SyntaxKind::UnionType => &[SyntaxKind::UnionType],
            SyntaxKind::Block => &[SyntaxKind::Block],
            SyntaxKind::LetStmt => &[SyntaxKind::LetStmt],
            SyntaxKind::ExprStmt => &[SyntaxKind::ExprStmt],
            SyntaxKind::PathExpr => &[SyntaxKind::PathExpr],
            SyntaxKind::PathExprSegment => &[SyntaxKind::PathExprSegment],
            SyntaxKind::LiteralExpr => &[SyntaxKind::LiteralExpr],
            SyntaxKind::GroupExpr => &[SyntaxKind::GroupExpr],
            SyntaxKind::PrefixExpr => &[SyntaxKind::PrefixExpr],
            SyntaxKind::BinaryExpr => &[SyntaxKind::BinaryExpr],
            SyntaxKind::FunctionCallExpr => &[SyntaxKind::FunctionCallExpr],
            SyntaxKind::Error => &[SyntaxKind::Error],
            SyntaxKind::Eof => &[SyntaxKind::Eof],
        }
    }
}
