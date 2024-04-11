use std::fmt;

use num_derive::{FromPrimitive, ToPrimitive};

#[derive(
    Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, ToPrimitive, FromPrimitive,
)]
pub enum SyntaxKind {
    #[default]
    Eof,
    Error,

    Ident,
    Int,
    String,

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
    BlockComment,

    Root,
    FunctionItem,
    FunctionParam,
    TypeAliasItem,
    StructItem,
    StructField,
    ConstItem,

    LetStmt,

    Block,
    PathExpr,
    InitializerExpr,
    InitializerField,
    LiteralExpr,
    ListExpr,
    TupleExpr,
    LambdaExpr,
    LambdaParamList,
    LambdaParam,
    PrefixExpr,
    BinaryExpr,
    IfExpr,
    FunctionCall,
    FunctionCallArgs,
    FieldAccess,
    IndexAccess,

    PathType,
    ListType,
    TupleType,
    NilTerminatedTupleType,
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

                SyntaxKind::Ident => "identifier",
                SyntaxKind::Int => "integer",
                SyntaxKind::String => "string",

                SyntaxKind::OpenParen => "'('",
                SyntaxKind::CloseParen => "')'",
                SyntaxKind::OpenBracket => "'['",
                SyntaxKind::CloseBracket => "']'",
                SyntaxKind::OpenBrace => "'{'",
                SyntaxKind::CloseBrace => "'}'",

                SyntaxKind::Fun => "'fun'",
                SyntaxKind::Type => "'type'",
                SyntaxKind::Struct => "'struct'",
                SyntaxKind::Let => "'let'",
                SyntaxKind::Const => "'const'",
                SyntaxKind::If => "'if'",
                SyntaxKind::Else => "'else'",
                SyntaxKind::Nil => "'nil'",
                SyntaxKind::True => "'true'",
                SyntaxKind::False => "'false'",

                SyntaxKind::Dot => "'.'",
                SyntaxKind::Comma => "','",
                SyntaxKind::Colon => "':'",
                SyntaxKind::Semicolon => "';'",
                SyntaxKind::Arrow => "'->'",
                SyntaxKind::FatArrow => "'=>'",

                SyntaxKind::Plus => "'+'",
                SyntaxKind::Minus => "'-'",
                SyntaxKind::Star => "'*'",
                SyntaxKind::Slash => "'/'",
                SyntaxKind::Percent => "'%'",
                SyntaxKind::Not => "'!'",
                SyntaxKind::LessThan => "'<'",
                SyntaxKind::GreaterThan => "'>'",
                SyntaxKind::LessThanEquals => "'<='",
                SyntaxKind::GreaterThanEquals => "'>='",
                SyntaxKind::Equals => "'=='",
                SyntaxKind::NotEquals => "'!='",
                SyntaxKind::Assign => "'='",

                SyntaxKind::Whitespace => "whitespace",
                SyntaxKind::LineComment => "line comment",
                SyntaxKind::BlockComment => "block comment",

                SyntaxKind::Root => "root",
                SyntaxKind::FunctionItem => "function item",
                SyntaxKind::FunctionParam => "function param",
                SyntaxKind::TypeAliasItem => "type alias item",
                SyntaxKind::StructItem => "struct item",
                SyntaxKind::StructField => "struct field",
                SyntaxKind::ConstItem => "const item",

                SyntaxKind::LetStmt => "let statement",

                SyntaxKind::Block => "block",
                SyntaxKind::PathExpr => "path expression",
                SyntaxKind::InitializerExpr => "initializer expression",
                SyntaxKind::InitializerField => "initializer field",
                SyntaxKind::LiteralExpr => "literal expression",
                SyntaxKind::ListExpr => "list expression",
                SyntaxKind::TupleExpr => "tuple expression",
                SyntaxKind::LambdaExpr => "lambda expression",
                SyntaxKind::LambdaParamList => "lambda param list",
                SyntaxKind::LambdaParam => "lambda param",
                SyntaxKind::PrefixExpr => "prefix expression",
                SyntaxKind::BinaryExpr => "binary expression",
                SyntaxKind::IfExpr => "if expression",
                SyntaxKind::FunctionCall => "function call",
                SyntaxKind::FunctionCallArgs => "function call arguments",
                SyntaxKind::FieldAccess => "field access",
                SyntaxKind::IndexAccess => "index access",

                SyntaxKind::PathType => "path type",
                SyntaxKind::ListType => "list type",
                SyntaxKind::TupleType => "tuple type",
                SyntaxKind::NilTerminatedTupleType => "nil-terminated tuple type",
                SyntaxKind::FunctionType => "function type",
                SyntaxKind::FunctionTypeParams => "function type parameters",
            }
        )
    }
}
