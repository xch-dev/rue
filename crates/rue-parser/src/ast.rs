use crate::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};

pub trait AstNode {
    fn cast(node: SyntaxNode) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxNode;
}

macro_rules! ast_node {
    ($kind:ident) => {
        #[derive(Debug, Clone)]
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

ast_enum!(Item, FunctionItem, TypeAliasItem);
ast_node!(TypeAliasItem);
ast_node!(FunctionItem);
ast_node!(FunctionParamList);
ast_node!(FunctionParam);
ast_node!(Block);

ast_enum!(
    Expr,
    LiteralExpr,
    ListExpr,
    PrefixExpr,
    BinaryExpr,
    IfExpr,
    FunctionCall
);
ast_node!(LiteralExpr);
ast_node!(ListExpr);
ast_node!(PrefixExpr);
ast_node!(BinaryExpr);
ast_node!(IfExpr);
ast_node!(FunctionCall);
ast_node!(FunctionCallArgs);

ast_enum!(Type, LiteralType, ListType, FunctionType);
ast_node!(LiteralType);
ast_node!(ListType);
ast_node!(FunctionType);
ast_node!(FunctionTypeParams);

impl Root {
    pub fn items(&self) -> Vec<Item> {
        self.syntax().children().filter_map(Item::cast).collect()
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

impl TypeAliasItem {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrefixOp {
    Not,
}

fn prefix_op(kind: SyntaxKind) -> Option<PrefixOp> {
    match kind {
        SyntaxKind::Not => Some(PrefixOp::Not),
        _ => None,
    }
}

impl PrefixExpr {
    pub fn op_token(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| prefix_op(token.kind()).is_some())
    }

    pub fn op(&self) -> Option<PrefixOp> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find_map(|token| prefix_op(token.kind()))
    }

    pub fn expr(&self) -> Option<Expr> {
        self.syntax().children().filter_map(Expr::cast).next()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Lt,
    Gt,
    LtEq,
    GtEq,
    Eq,
    NotEq,
}

fn binary_op(kind: SyntaxKind) -> Option<BinaryOp> {
    match kind {
        SyntaxKind::Plus => Some(BinaryOp::Add),
        SyntaxKind::Minus => Some(BinaryOp::Sub),
        SyntaxKind::Star => Some(BinaryOp::Mul),
        SyntaxKind::Slash => Some(BinaryOp::Div),
        SyntaxKind::Percent => Some(BinaryOp::Rem),
        SyntaxKind::LessThan => Some(BinaryOp::Lt),
        SyntaxKind::GreaterThan => Some(BinaryOp::Gt),
        SyntaxKind::LessThanEquals => Some(BinaryOp::LtEq),
        SyntaxKind::GreaterThanEquals => Some(BinaryOp::GtEq),
        SyntaxKind::Equals => Some(BinaryOp::Eq),
        SyntaxKind::NotEquals => Some(BinaryOp::NotEq),
        _ => None,
    }
}

impl BinaryExpr {
    pub fn lhs(&self) -> Option<Expr> {
        self.syntax().children().filter_map(Expr::cast).nth(0)
    }

    pub fn op_token(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| binary_op(token.kind()).is_some())
    }

    pub fn op(&self) -> Option<BinaryOp> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find_map(|token| binary_op(token.kind()))
    }

    pub fn rhs(&self) -> Option<Expr> {
        self.syntax().children().filter_map(Expr::cast).nth(1)
    }
}

impl ListExpr {
    pub fn items(&self) -> Vec<Expr> {
        self.syntax().children().filter_map(Expr::cast).collect()
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

impl ListType {
    pub fn ty(&self) -> Option<Type> {
        self.syntax().children().find_map(Type::cast)
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
