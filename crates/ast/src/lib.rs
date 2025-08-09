use paste::paste;
use rue_parser::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken, T};

pub trait AstNode {
    fn cast(node: SyntaxNode) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxNode;
}

macro_rules! ast_nodes {
    ($( $kind:ident),+ $(,)?) => { paste! { $(
        #[derive(Debug, Clone)]
        pub struct [< Ast $kind >](SyntaxNode);

        impl AstNode for [< Ast $kind >] {
            fn cast(node: SyntaxNode) -> Option<Self> {
                match node.kind() {
                    SyntaxKind::$kind => Some(Self(node)),
                    _ => None,
                }
            }

            fn syntax(&self) -> &SyntaxNode {
                &self.0
            }
        }
    )+ } };
}

macro_rules! ast_enum {
    ($name:ident, $( $kind:ident ),+ $(,)? ) => { paste! {
        #[derive(Debug, Clone)]
        pub enum [< Ast $name >] {
            $( $kind([< Ast $kind >]), )+
        }

        impl AstNode for [< Ast $name >] {
            fn cast(node: SyntaxNode) -> Option<Self> {
                $( if let Some(node) = [< Ast $kind >]::cast(node.clone()) {
                    return Some(Self::$kind(node));
                } )+
                None
            }

            fn syntax(&self) -> &SyntaxNode {
                match self {
                    $( Self::$kind(node) => node.syntax(), )+
                }
            }
        }
    } };
}

ast_nodes!(
    Document,
    FunctionItem,
    FunctionParameter,
    TypeAliasItem,
    SubtypeItem,
    SubtypeGenericParameters,
    SubtypeGenericParameter,
    SubtypeParameter,
    SubtypeConstraint,
    SubtypeFields,
    SubtypeField,
    GenericParameters,
    GenericArguments,
    PathType,
    PathTypeSegment,
    UnionType,
    Block,
    LetStmt,
    ExprStmt,
    PathExpr,
    PathExprSegment,
    LiteralExpr,
    GroupExpr,
    PrefixExpr,
    BinaryExpr,
    FunctionCallExpr,
);

ast_enum!(Item, TypeItem, SymbolItem);
ast_enum!(TypeItem, TypeAliasItem, SubtypeItem);
ast_enum!(SymbolItem, FunctionItem);
ast_enum!(Stmt, LetStmt, ExprStmt);
ast_enum!(StmtOrExpr, Stmt, Expr);
ast_enum!(
    Expr,
    PathExpr,
    LiteralExpr,
    GroupExpr,
    PrefixExpr,
    BinaryExpr,
    FunctionCallExpr
);
ast_enum!(Type, PathType, UnionType);

impl AstDocument {
    pub fn items(&self) -> impl Iterator<Item = AstItem> {
        self.syntax().children().filter_map(AstItem::cast)
    }
}

impl AstFunctionItem {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    pub fn generic_parameters(&self) -> Option<AstGenericParameters> {
        self.syntax()
            .children()
            .find_map(AstGenericParameters::cast)
    }

    pub fn parameters(&self) -> impl Iterator<Item = AstFunctionParameter> {
        self.syntax()
            .children()
            .filter_map(AstFunctionParameter::cast)
    }

    pub fn return_type(&self) -> Option<AstType> {
        self.syntax().children().find_map(AstType::cast)
    }

    pub fn body(&self) -> Option<AstBlock> {
        self.syntax().children().find_map(AstBlock::cast)
    }
}

impl AstTypeAliasItem {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    pub fn generic_parameters(&self) -> Option<AstGenericParameters> {
        self.syntax()
            .children()
            .find_map(AstGenericParameters::cast)
    }

    pub fn ty(&self) -> Option<AstType> {
        self.syntax().children().find_map(AstType::cast)
    }
}

impl AstSubtypeItem {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    pub fn generic_parameters(&self) -> Option<AstSubtypeGenericParameters> {
        self.syntax()
            .children()
            .find_map(AstSubtypeGenericParameters::cast)
    }

    pub fn parameter(&self) -> Option<AstSubtypeParameter> {
        self.syntax().children().find_map(AstSubtypeParameter::cast)
    }

    pub fn constraint(&self) -> Option<AstSubtypeConstraint> {
        self.syntax()
            .children()
            .find_map(AstSubtypeConstraint::cast)
    }

    pub fn fields(&self) -> Option<AstSubtypeFields> {
        self.syntax().children().find_map(AstSubtypeFields::cast)
    }
}

impl AstSubtypeGenericParameters {
    pub fn parameters(&self) -> impl Iterator<Item = AstSubtypeGenericParameter> {
        self.syntax()
            .children()
            .filter_map(AstSubtypeGenericParameter::cast)
    }
}

impl AstSubtypeGenericParameter {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    pub fn field(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .filter(|token| token.kind() == SyntaxKind::Ident)
            .nth(1)
    }
}

impl AstSubtypeParameter {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    pub fn ty(&self) -> Option<AstType> {
        self.syntax().children().find_map(AstType::cast)
    }
}

impl AstSubtypeConstraint {
    pub fn expr(&self) -> Option<AstExpr> {
        self.syntax().children().find_map(AstExpr::cast)
    }
}

impl AstSubtypeFields {
    pub fn fields(&self) -> impl Iterator<Item = AstSubtypeField> {
        self.syntax().children().filter_map(AstSubtypeField::cast)
    }
}

impl AstSubtypeField {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    pub fn expr(&self) -> Option<AstExpr> {
        self.syntax().children().find_map(AstExpr::cast)
    }
}

impl AstFunctionParameter {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    pub fn ty(&self) -> Option<AstType> {
        self.syntax().children().find_map(AstType::cast)
    }
}

impl AstGenericParameters {
    pub fn names(&self) -> impl Iterator<Item = SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .filter(|token| token.kind() == SyntaxKind::Ident)
    }
}

impl AstGenericArguments {
    pub fn types(&self) -> impl Iterator<Item = AstType> {
        self.syntax().children().filter_map(AstType::cast)
    }
}

impl AstBlock {
    pub fn items(&self) -> impl Iterator<Item = AstStmtOrExpr> {
        self.syntax().children().filter_map(AstStmtOrExpr::cast)
    }
}

impl AstLetStmt {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    pub fn ty(&self) -> Option<AstType> {
        self.syntax().children().find_map(AstType::cast)
    }

    pub fn value(&self) -> Option<AstExpr> {
        self.syntax().children().find_map(AstExpr::cast)
    }
}

impl AstExprStmt {
    pub fn expr(&self) -> Option<AstExpr> {
        self.syntax().children().find_map(AstExpr::cast)
    }
}

impl AstPathExpr {
    pub fn segments(&self) -> impl Iterator<Item = AstPathExprSegment> {
        self.syntax()
            .children()
            .filter_map(AstPathExprSegment::cast)
    }
}

impl AstPathExprSegment {
    pub fn separator(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == T![::])
    }

    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    pub fn generic_arguments(&self) -> Option<AstGenericArguments> {
        self.syntax().children().find_map(AstGenericArguments::cast)
    }
}

impl AstLiteralExpr {
    pub fn value(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| SyntaxKind::LITERAL_EXPR.contains(&token.kind()))
    }
}

impl AstGroupExpr {
    pub fn expr(&self) -> Option<AstExpr> {
        self.syntax().children().find_map(AstExpr::cast)
    }
}

impl AstPrefixExpr {
    pub fn op(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| SyntaxKind::PREFIX_OPS.contains(&token.kind()))
    }

    pub fn expr(&self) -> Option<AstExpr> {
        self.syntax().children().find_map(AstExpr::cast)
    }
}

impl AstBinaryExpr {
    pub fn op(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| SyntaxKind::BINARY_OPS.contains(&token.kind()))
    }

    pub fn left(&self) -> Option<AstExpr> {
        self.syntax().children().find_map(AstExpr::cast)
    }

    pub fn right(&self) -> Option<AstExpr> {
        self.syntax().children().filter_map(AstExpr::cast).nth(1)
    }
}

impl AstFunctionCallExpr {
    pub fn expr(&self) -> Option<AstExpr> {
        self.syntax().children().find_map(AstExpr::cast)
    }

    pub fn args(&self) -> impl Iterator<Item = AstExpr> {
        self.syntax().children().filter_map(AstExpr::cast).skip(1)
    }
}

impl AstPathType {
    pub fn segments(&self) -> impl Iterator<Item = AstPathTypeSegment> {
        self.syntax()
            .children()
            .filter_map(AstPathTypeSegment::cast)
    }
}

impl AstPathTypeSegment {
    pub fn separator(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == T![::])
    }

    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    pub fn generic_arguments(&self) -> Option<AstGenericArguments> {
        self.syntax().children().find_map(AstGenericArguments::cast)
    }
}

impl AstUnionType {
    pub fn types(&self) -> impl Iterator<Item = AstType> {
        self.syntax().children().filter_map(AstType::cast)
    }
}
