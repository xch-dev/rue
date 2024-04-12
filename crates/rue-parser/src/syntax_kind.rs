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
    Enum,
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
    PathSeparator,
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
    BlockComment,

    Root,
    FunctionItem,
    FunctionParam,
    TypeAliasItem,
    StructItem,
    StructField,
    EnumItem,
    EnumVariant,
    ConstItem,

    LetStmt,

    Block,
    Path,

    InitializerExpr,
    InitializerField,
    LiteralExpr,
    ListExpr,
    ListItem,
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

    ListType,
    ListTypeItem,
    TupleType,
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
                SyntaxKind::Enum => "'enum'",
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
                SyntaxKind::PathSeparator => "'::'",
                SyntaxKind::Semicolon => "';'",
                SyntaxKind::Arrow => "'->'",
                SyntaxKind::FatArrow => "'=>'",
                SyntaxKind::Spread => "'...'",

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
                SyntaxKind::EnumItem => "enum item",
                SyntaxKind::EnumVariant => "enum variant",
                SyntaxKind::ConstItem => "const item",

                SyntaxKind::LetStmt => "let statement",

                SyntaxKind::Block => "block",
                SyntaxKind::Path => "identifier path",

                SyntaxKind::InitializerExpr => "initializer expression",
                SyntaxKind::InitializerField => "initializer field",
                SyntaxKind::LiteralExpr => "literal expression",
                SyntaxKind::ListExpr => "list expression",
                SyntaxKind::ListItem => "list item",
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

                SyntaxKind::ListType => "list type",
                SyntaxKind::ListTypeItem => "list type item",
                SyntaxKind::TupleType => "tuple type",
                SyntaxKind::FunctionType => "function type",
                SyntaxKind::FunctionTypeParams => "function type parameters",
            }
        )
    }
}
