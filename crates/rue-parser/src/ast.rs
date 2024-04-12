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
        #[derive(Debug, Clone)]
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

ast_enum!(Item, FunctionItem, TypeAliasItem, ConstItem, StructItem);
ast_node!(FunctionItem);
ast_node!(FunctionParam);
ast_node!(TypeAliasItem);
ast_node!(StructItem);
ast_node!(ConstItem);
ast_node!(StructField);
ast_node!(Block);

ast_enum!(
    Expr,
    PathExpr,
    InitializerExpr,
    LiteralExpr,
    ListExpr,
    TupleExpr,
    Block,
    LambdaExpr,
    PrefixExpr,
    BinaryExpr,
    IfExpr,
    FunctionCall,
    FieldAccess,
    IndexAccess
);
ast_node!(PathExpr);
ast_node!(InitializerExpr);
ast_node!(InitializerField);
ast_node!(LiteralExpr);
ast_node!(ListExpr);
ast_node!(ListItem);
ast_node!(TupleExpr);
ast_node!(PrefixExpr);
ast_node!(BinaryExpr);
ast_node!(IfExpr);
ast_node!(FunctionCall);
ast_node!(FunctionCallArgs);
ast_node!(FieldAccess);
ast_node!(IndexAccess);

ast_node!(LambdaExpr);
ast_node!(LambdaParamList);
ast_node!(LambdaParam);

ast_enum!(Type, PathType, ListType, TupleType, FunctionType);
ast_node!(PathType);
ast_node!(ListType);
ast_node!(ListTypeItem);
ast_node!(TupleType);
ast_node!(FunctionType);
ast_node!(FunctionTypeParams);

ast_enum!(Stmt, LetStmt);
ast_node!(LetStmt);

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

    pub fn params(&self) -> Vec<FunctionParam> {
        self.syntax()
            .children()
            .filter_map(FunctionParam::cast)
            .collect()
    }

    pub fn return_type(&self) -> Option<Type> {
        self.syntax().children().find_map(Type::cast)
    }

    pub fn body(&self) -> Option<Block> {
        self.syntax().children().find_map(Block::cast)
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

impl StructItem {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    pub fn fields(&self) -> Vec<StructField> {
        self.syntax()
            .children()
            .filter_map(StructField::cast)
            .collect()
    }
}

impl StructField {
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

impl ConstItem {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    pub fn ty(&self) -> Option<Type> {
        self.syntax().children().find_map(Type::cast)
    }

    pub fn expr(&self) -> Option<Expr> {
        self.syntax().children().find_map(Expr::cast)
    }
}

impl Block {
    pub fn items(&self) -> Vec<Item> {
        self.syntax().children().filter_map(Item::cast).collect()
    }

    pub fn stmts(&self) -> Vec<Stmt> {
        self.syntax().children().filter_map(Stmt::cast).collect()
    }

    pub fn expr(&self) -> Option<Expr> {
        self.syntax().children().find_map(Expr::cast)
    }
}

impl LetStmt {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    pub fn ty(&self) -> Option<Type> {
        self.syntax().children().find_map(Type::cast)
    }

    pub fn expr(&self) -> Option<Expr> {
        self.syntax().children().find_map(Expr::cast)
    }
}

impl PathExpr {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .find_map(SyntaxElement::into_token)
    }
}

impl InitializerExpr {
    pub fn path(&self) -> Option<PathExpr> {
        self.syntax().children().find_map(PathExpr::cast)
    }

    pub fn fields(&self) -> Vec<InitializerField> {
        self.syntax()
            .children()
            .filter_map(InitializerField::cast)
            .collect()
    }
}

impl InitializerField {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

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
    Subtract,
    Multiply,
    Divide,
    Remainder,
    LessThan,
    GreaterThan,
    LessThanEquals,
    GreaterThanEquals,
    Equals,
    NotEquals,
}

fn binary_op(kind: SyntaxKind) -> Option<BinaryOp> {
    match kind {
        SyntaxKind::Plus => Some(BinaryOp::Add),
        SyntaxKind::Minus => Some(BinaryOp::Subtract),
        SyntaxKind::Star => Some(BinaryOp::Multiply),
        SyntaxKind::Slash => Some(BinaryOp::Divide),
        SyntaxKind::Percent => Some(BinaryOp::Remainder),
        SyntaxKind::LessThan => Some(BinaryOp::LessThan),
        SyntaxKind::GreaterThan => Some(BinaryOp::GreaterThan),
        SyntaxKind::LessThanEquals => Some(BinaryOp::LessThanEquals),
        SyntaxKind::GreaterThanEquals => Some(BinaryOp::GreaterThanEquals),
        SyntaxKind::Equals => Some(BinaryOp::Equals),
        SyntaxKind::NotEquals => Some(BinaryOp::NotEquals),
        _ => None,
    }
}

impl BinaryExpr {
    pub fn lhs(&self) -> Option<Expr> {
        self.syntax().children().find_map(Expr::cast)
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
    pub fn items(&self) -> Vec<ListItem> {
        self.syntax()
            .children()
            .filter_map(ListItem::cast)
            .collect()
    }
}

impl ListItem {
    pub fn expr(&self) -> Option<Expr> {
        self.syntax().children().find_map(Expr::cast)
    }

    pub fn op(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Spread)
    }
}

impl TupleExpr {
    pub fn items(&self) -> Vec<Expr> {
        self.syntax().children().filter_map(Expr::cast).collect()
    }
}

impl LambdaExpr {
    pub fn param_list(&self) -> Option<LambdaParamList> {
        self.syntax().children().find_map(LambdaParamList::cast)
    }

    pub fn ty(&self) -> Option<Type> {
        self.syntax().children().find_map(Type::cast)
    }

    pub fn body(&self) -> Option<Expr> {
        self.syntax().children().find_map(Expr::cast)
    }
}

impl LambdaParamList {
    pub fn params(&self) -> Vec<LambdaParam> {
        self.syntax()
            .children()
            .filter_map(LambdaParam::cast)
            .collect()
    }
}

impl LambdaParam {
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

impl IfExpr {
    pub fn condition(&self) -> Option<Expr> {
        self.syntax().children().find_map(Expr::cast)
    }

    pub fn then_block(&self) -> Option<Block> {
        self.syntax().children().find_map(Block::cast)
    }

    pub fn else_block(&self) -> Option<Block> {
        self.syntax().children().filter_map(Block::cast).nth(1)
    }
}

impl FunctionCall {
    pub fn callee(&self) -> Option<Expr> {
        self.syntax().children().find_map(Expr::cast)
    }

    pub fn args(&self) -> Option<FunctionCallArgs> {
        self.syntax().children().find_map(FunctionCallArgs::cast)
    }
}

impl FunctionCallArgs {
    pub fn exprs(&self) -> Vec<Expr> {
        self.syntax().children().filter_map(Expr::cast).collect()
    }
}

impl FieldAccess {
    pub fn expr(&self) -> Option<Expr> {
        self.syntax().children().find_map(Expr::cast)
    }

    pub fn field(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| matches!(token.kind(), SyntaxKind::Ident | SyntaxKind::Int))
    }
}

impl IndexAccess {
    pub fn expr(&self) -> Option<Expr> {
        self.syntax().children().find_map(Expr::cast)
    }

    pub fn index(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Int)
    }
}

impl PathType {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .find_map(SyntaxElement::into_token)
    }
}

impl ListType {
    pub fn ty(&self) -> Option<Type> {
        self.syntax().children().find_map(Type::cast)
    }
}

impl ListTypeItem {
    pub fn ty(&self) -> Option<Type> {
        self.syntax().children().find_map(Type::cast)
    }

    pub fn op(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Spread)
    }
}

impl TupleType {
    pub fn items(&self) -> Vec<Type> {
        self.syntax().children().filter_map(Type::cast).collect()
    }
}

impl FunctionType {
    pub fn params(&self) -> Option<FunctionTypeParams> {
        self.syntax().children().find_map(FunctionTypeParams::cast)
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
