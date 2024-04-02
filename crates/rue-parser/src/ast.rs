use crate::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};

pub trait AstNode {
    fn cast(node: SyntaxNode) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxNode;
}

macro_rules! ast_node {
    ($kind:ident) => {
        pub struct $kind(SyntaxNode);

        impl AstNode for $kind {
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
    };
}

macro_rules! ast_enum {
    ($name:ident, $( $kind:ident ),+ $(,)? ) => {
        pub enum $name {
            $( $kind($kind), )+
        }

        impl AstNode for $name {
            fn cast(node: SyntaxNode) -> Option<Self> {
                match node.kind() {
                    $( SyntaxKind::$kind => Some(Self::$kind($kind::cast(node)?)), )+
                    _ => None,
                }
            }

            fn syntax(&self) -> &SyntaxNode {
                match self {
                    $( Self::$kind(node) => node.syntax(), )+
                }
            }
        }
    };
}

ast_node!(Root);
ast_node!(FunctionItem);
ast_node!(FunctionParamList);
ast_node!(FunctionParam);
ast_node!(Block);

ast_enum!(Expr, LiteralExpr, BinaryExpr, IfExpr, FunctionCall);
ast_node!(LiteralExpr);
ast_node!(BinaryExpr);
ast_node!(IfExpr);
ast_node!(FunctionCall);
ast_node!(FunctionCallArgs);

ast_enum!(Type, LiteralType, FunctionType);
ast_node!(LiteralType);
ast_node!(FunctionType);
ast_node!(FunctionTypeParams);

impl Root {
    pub fn function_items(&self) -> Vec<FunctionItem> {
        self.syntax()
            .children()
            .filter_map(FunctionItem::cast)
            .collect()
    }
}

impl FunctionItem {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    pub fn param_list(&self) -> Option<FunctionParamList> {
        self.syntax().children().find_map(FunctionParamList::cast)
    }

    pub fn return_ty(&self) -> Option<Type> {
        self.syntax().children().find_map(Type::cast)
    }

    pub fn body(&self) -> Option<Block> {
        self.syntax().children().find_map(Block::cast)
    }
}

impl FunctionParamList {
    pub fn params(&self) -> Vec<FunctionParam> {
        self.syntax()
            .children()
            .filter_map(FunctionParam::cast)
            .collect()
    }
}

impl FunctionParam {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    pub fn ty(&self) -> Option<Type> {
        self.syntax().children().find_map(Type::cast)
    }
}

impl Block {
    pub fn expr(&self) -> Option<Expr> {
        self.syntax().children().filter_map(Expr::cast).next()
    }
}

impl LiteralExpr {
    pub fn value(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .next()
    }
}

impl BinaryExpr {
    pub fn lhs(&self) -> Option<Expr> {
        self.syntax().children().filter_map(Expr::cast).nth(0)
    }

    pub fn op(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| {
                matches!(
                    token.kind(),
                    SyntaxKind::Plus
                        | SyntaxKind::Minus
                        | SyntaxKind::Star
                        | SyntaxKind::Slash
                        | SyntaxKind::LessThan
                        | SyntaxKind::GreaterThan
                        | SyntaxKind::Equals
                )
            })
    }

    pub fn rhs(&self) -> Option<Expr> {
        self.syntax().children().filter_map(Expr::cast).nth(1)
    }
}

impl IfExpr {
    pub fn condition(&self) -> Option<Expr> {
        self.syntax().children().filter_map(Expr::cast).nth(0)
    }

    pub fn then_block(&self) -> Option<Block> {
        self.syntax().children().filter_map(Block::cast).nth(0)
    }

    pub fn else_block(&self) -> Option<Block> {
        self.syntax().children().filter_map(Block::cast).nth(1)
    }
}

impl FunctionCall {
    pub fn callee(&self) -> Option<Expr> {
        self.syntax().children().filter_map(Expr::cast).next()
    }

    pub fn args(&self) -> Option<FunctionCallArgs> {
        self.syntax()
            .children()
            .filter_map(FunctionCallArgs::cast)
            .next()
    }
}

impl FunctionCallArgs {
    pub fn exprs(&self) -> Vec<Expr> {
        self.syntax().children().filter_map(Expr::cast).collect()
    }
}

impl LiteralType {
    pub fn value(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .next()
    }
}

impl FunctionType {
    pub fn params(&self) -> Option<FunctionTypeParams> {
        self.syntax()
            .children()
            .filter_map(FunctionTypeParams::cast)
            .next()
    }

    pub fn ret(&self) -> Option<Type> {
        self.syntax().children().find_map(Type::cast)
    }
}

impl FunctionTypeParams {
    pub fn types(&self) -> Vec<Type> {
        self.syntax().children().filter_map(Type::cast).collect()
    }
}
