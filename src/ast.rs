use crate::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};

use paste::paste;

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
    GenericParameters,
    LiteralType,
    Block,
    LetStmt,
    ExprStmt,
    LiteralExpr,
    GroupExpr,
    PrefixExpr,
    BinaryExpr
);

ast_enum!(Item, TypeItem, SymbolItem);
ast_enum!(TypeItem, TypeAliasItem);
ast_enum!(SymbolItem, FunctionItem);
ast_enum!(Stmt, LetStmt, ExprStmt);
ast_enum!(StmtOrExpr, Stmt, Expr);
ast_enum!(Expr, LiteralExpr, GroupExpr, PrefixExpr, BinaryExpr);
ast_enum!(Type, LiteralType);

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

impl AstLiteralExpr {
    pub fn value(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| SyntaxKind::LITERAL_KINDS.contains(&token.kind()))
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

impl AstLiteralType {
    pub fn value(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| SyntaxKind::LITERAL_KINDS.contains(&token.kind()))
    }
}
