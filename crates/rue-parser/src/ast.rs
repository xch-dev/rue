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

ast_enum!(
    Item,
    FunctionItem,
    TypeAliasItem,
    ConstItem,
    StructItem,
    EnumItem
);
ast_node!(FunctionItem);
ast_node!(FunctionParam);
ast_node!(TypeAliasItem);
ast_node!(StructItem);
ast_node!(EnumItem);
ast_node!(EnumVariant);
ast_node!(ConstItem);
ast_node!(StructField);

ast_node!(Block);
ast_node!(Path);

ast_enum!(
    Expr,
    Path,
    InitializerExpr,
    LiteralExpr,
    ListExpr,
    PairExpr,
    Block,
    LambdaExpr,
    PrefixExpr,
    BinaryExpr,
    GroupExpr,
    CastExpr,
    GuardExpr,
    IfExpr,
    FunctionCall,
    FieldAccess,
    IndexAccess
);
ast_node!(InitializerExpr);
ast_node!(InitializerField);
ast_node!(LiteralExpr);
ast_node!(ListExpr);
ast_node!(ListItem);
ast_node!(PairExpr);
ast_node!(PrefixExpr);
ast_node!(BinaryExpr);
ast_node!(GroupExpr);
ast_node!(CastExpr);
ast_node!(GuardExpr);
ast_node!(IfExpr);
ast_node!(FunctionCall);
ast_node!(FunctionCallArg);
ast_node!(FieldAccess);
ast_node!(IndexAccess);

ast_node!(LambdaExpr);
ast_node!(LambdaParam);

ast_enum!(Type, Path, ListType, PairType, FunctionType);
ast_node!(ListType);
ast_node!(ListTypeItem);
ast_node!(PairType);
ast_node!(FunctionType);
ast_node!(FunctionTypeParam);

ast_enum!(Stmt, LetStmt, IfStmt, ReturnStmt, RaiseStmt, AssertStmt);
ast_node!(LetStmt);
ast_node!(IfStmt);
ast_node!(ReturnStmt);
ast_node!(RaiseStmt);
ast_node!(AssertStmt);

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
    pub fn spread(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Spread)
    }

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

impl EnumItem {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    pub fn variants(&self) -> Vec<EnumVariant> {
        self.syntax()
            .children()
            .filter_map(EnumVariant::cast)
            .collect()
    }
}

impl EnumVariant {
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

    pub fn discriminant(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Int)
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
        self.syntax().children().filter_map(Expr::cast).last()
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

impl Path {
    pub fn idents(&self) -> Vec<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .filter(|token| token.kind() == SyntaxKind::Ident)
            .collect()
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
        let exprs: Vec<Expr> = self.syntax().children().filter_map(Expr::cast).collect();
        let types: Vec<Type> = self.syntax().children().filter_map(Type::cast).collect();
        if exprs.len() == 1 && types.len() == 1 {
            return None;
        }
        self.syntax().children().find_map(Type::cast)
    }

    pub fn expr(&self) -> Option<Expr> {
        self.syntax().children().filter_map(Expr::cast).last()
    }
}

impl IfStmt {
    pub fn condition(&self) -> Option<Expr> {
        self.syntax().children().find_map(Expr::cast)
    }

    pub fn then_block(&self) -> Option<Block> {
        self.syntax().children().find_map(Block::cast)
    }
}

impl ReturnStmt {
    pub fn expr(&self) -> Option<Expr> {
        self.syntax().children().find_map(Expr::cast)
    }
}

impl RaiseStmt {
    pub fn expr(&self) -> Option<Expr> {
        self.syntax().children().find_map(Expr::cast)
    }
}

impl AssertStmt {
    pub fn expr(&self) -> Option<Expr> {
        self.syntax().children().find_map(Expr::cast)
    }
}

impl InitializerExpr {
    pub fn path(&self) -> Option<Path> {
        self.syntax().children().find_map(Path::cast)
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

impl GroupExpr {
    pub fn expr(&self) -> Option<Expr> {
        self.syntax().children().find_map(Expr::cast)
    }
}

impl CastExpr {
    pub fn expr(&self) -> Option<Expr> {
        self.syntax().children().find_map(Expr::cast)
    }

    pub fn ty(&self) -> Option<Type> {
        self.syntax().children().filter_map(Type::cast).last()
    }
}

impl GuardExpr {
    pub fn expr(&self) -> Option<Expr> {
        self.syntax().children().find_map(Expr::cast)
    }

    pub fn ty(&self) -> Option<Type> {
        self.syntax().children().filter_map(Type::cast).last()
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

    pub fn spread(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Spread)
    }
}

impl PairExpr {
    pub fn first(&self) -> Option<Expr> {
        self.syntax().children().find_map(Expr::cast)
    }

    pub fn rest(&self) -> Option<Expr> {
        self.syntax().children().filter_map(Expr::cast).last()
    }
}

impl LambdaExpr {
    pub fn params(&self) -> Vec<LambdaParam> {
        self.syntax()
            .children()
            .filter_map(LambdaParam::cast)
            .collect()
    }

    pub fn ty(&self) -> Option<Type> {
        self.syntax().children().find_map(Type::cast)
    }

    pub fn body(&self) -> Option<Expr> {
        self.syntax().children().find_map(Expr::cast)
    }
}

impl LambdaParam {
    pub fn spread(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Spread)
    }

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

    pub fn args(&self) -> Vec<FunctionCallArg> {
        self.syntax()
            .children()
            .filter_map(FunctionCallArg::cast)
            .collect()
    }
}

impl FunctionCallArg {
    pub fn spread(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Spread)
    }

    pub fn expr(&self) -> Option<Expr> {
        self.syntax().children().find_map(Expr::cast)
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
            .find(|token| matches!(token.kind(), SyntaxKind::Ident))
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

impl ListType {
    pub fn ty(&self) -> Option<Type> {
        self.syntax().children().find_map(Type::cast)
    }
}

impl ListTypeItem {
    pub fn spread(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Spread)
    }

    pub fn ty(&self) -> Option<Type> {
        self.syntax().children().find_map(Type::cast)
    }
}

impl PairType {
    pub fn first(&self) -> Option<Type> {
        self.syntax().children().find_map(Type::cast)
    }

    pub fn rest(&self) -> Option<Type> {
        self.syntax().children().filter_map(Type::cast).last()
    }
}

impl FunctionType {
    pub fn params(&self) -> Vec<FunctionTypeParam> {
        self.syntax()
            .children()
            .filter_map(FunctionTypeParam::cast)
            .collect()
    }

    pub fn ret(&self) -> Option<Type> {
        self.syntax().children().find_map(Type::cast)
    }
}

impl FunctionTypeParam {
    pub fn spread(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Spread)
    }

    pub fn ty(&self) -> Option<Type> {
        self.syntax().children().find_map(Type::cast)
    }
}
