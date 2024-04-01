use std::fmt;

use num_derive::{FromPrimitive, ToPrimitive};

#[derive(
    Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, ToPrimitive, FromPrimitive,
)]
pub enum SyntaxKind {
    #[default]
    Eof,
    Error,
    Whitespace,
    LineComment,
    BlockComment,
    Ident,
    Int,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Fun,
    If,
    Else,
    Comma,
    Colon,
    Arrow,
    Plus,
    Minus,
    Star,
    Slash,
    LessThan,
    GreaterThan,
    Root,
    FunctionItem,
    FunctionParamList,
    FunctionParam,
    Block,
    LiteralExpr,
    BinaryExpr,
    IfExpr,
    FunctionCall,
    FunctionCallArgs,
    LiteralType,
    FunctionType,
    FunctionTypeParams,
}

impl fmt::Display for SyntaxKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                SyntaxKind::Eof => "end of file",
                SyntaxKind::Error => "error",
                SyntaxKind::Whitespace => "whitespace",
                SyntaxKind::LineComment => "line comment",
                SyntaxKind::BlockComment => "block comment",
                SyntaxKind::Ident => "identifier",
                SyntaxKind::Int => "integer",
                SyntaxKind::OpenParen => "'('",
                SyntaxKind::CloseParen => "')'",
                SyntaxKind::OpenBrace => "'{'",
                SyntaxKind::CloseBrace => "'}'",
                SyntaxKind::Fun => "'fun'",
                SyntaxKind::If => "'if'",
                SyntaxKind::Else => "'else'",
                SyntaxKind::Comma => "','",
                SyntaxKind::Colon => "':'",
                SyntaxKind::Arrow => "'->'",
                SyntaxKind::Plus => "'+'",
                SyntaxKind::Minus => "'-'",
                SyntaxKind::Star => "'*'",
                SyntaxKind::Slash => "'/'",
                SyntaxKind::LessThan => "'<'",
                SyntaxKind::GreaterThan => "'>'",
                SyntaxKind::Root => "root",
                SyntaxKind::FunctionItem => "function item",
                SyntaxKind::FunctionParamList => "function param list",
                SyntaxKind::FunctionParam => "function param",
                SyntaxKind::Block => "block",
                SyntaxKind::LiteralExpr => "literal expression",
                SyntaxKind::BinaryExpr => "binary expression",
                SyntaxKind::IfExpr => "if expression",
                SyntaxKind::FunctionCall => "function call",
                SyntaxKind::FunctionCallArgs => "function call arguments",
                SyntaxKind::LiteralType => "literal type",
                SyntaxKind::FunctionType => "function type",
                SyntaxKind::FunctionTypeParams => "function type parameters",
            }
        )
    }
}
