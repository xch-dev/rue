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
    ModuleItem,
    FunctionItem,
    TypeAliasItem,
    ConstItem,
    StructItem,
    EnumItem,
    ImportItem,
);
ast_node!(ModuleItem);
ast_node!(FunctionItem);
ast_node!(FunctionParam);
ast_node!(TypeAliasItem);
ast_node!(StructItem);
ast_node!(EnumItem);
ast_node!(EnumVariant);
ast_node!(EnumVariantFields);
ast_node!(ConstItem);
ast_node!(StructField);
ast_node!(ImportItem);
ast_node!(ImportPath);
ast_node!(ImportGroup);

ast_node!(Block);

ast_enum!(
    Expr,
    PathExpr,
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
    FunctionCallExpr,
    FieldAccessExpr,
);
ast_node!(PathExpr);
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
ast_node!(FunctionCallExpr);
ast_node!(FunctionCallArg);
ast_node!(FieldAccessExpr);

ast_node!(LambdaExpr);
ast_node!(LambdaParam);

ast_enum!(Type, PathType, PairType, FunctionType, UnionType);
ast_node!(PathType);
ast_node!(PairType);
ast_node!(FunctionType);
ast_node!(FunctionTypeParam);
ast_node!(UnionType);

ast_enum!(Stmt, LetStmt, IfStmt, ReturnStmt, RaiseStmt, AssertStmt, AssumeStmt);
ast_node!(LetStmt);
ast_node!(IfStmt);
ast_node!(ReturnStmt);
ast_node!(RaiseStmt);
ast_node!(AssertStmt);
ast_node!(AssumeStmt);

ast_node!(GenericParams);

impl Root {
    pub fn items(&self) -> Vec<Item> {
        self.syntax().children().filter_map(Item::cast).collect()
    }
}

impl Item {
    pub fn export(&self) -> Option<SyntaxToken> {
        match self {
            Item::ModuleItem(item) => item.export(),
            Item::FunctionItem(item) => item.export(),
            Item::TypeAliasItem(item) => item.export(),
            Item::StructItem(item) => item.export(),
            Item::EnumItem(item) => item.export(),
            Item::ConstItem(item) => item.export(),
            Item::ImportItem(_item) => None,
        }
    }
}

impl ModuleItem {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    pub fn items(&self) -> Vec<Item> {
        self.syntax().children().filter_map(Item::cast).collect()
    }

    pub fn export(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Export)
    }
}

impl FunctionItem {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    pub fn generic_params(&self) -> Option<GenericParams> {
        self.syntax().children().find_map(GenericParams::cast)
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

    pub fn export(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Export)
    }

    pub fn inline(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Inline)
    }
}

impl FunctionParam {
    pub fn spread(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Spread)
    }

    pub fn optional(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Question)
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

    pub fn export(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Export)
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

    pub fn export(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Export)
    }
}

impl StructField {
    pub fn spread(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Spread)
    }

    pub fn optional(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Question)
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

    pub fn export(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Export)
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

    pub fn export(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Export)
    }
}

impl EnumVariant {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    pub fn fields(&self) -> Option<EnumVariantFields> {
        self.syntax().children().find_map(EnumVariantFields::cast)
    }

    pub fn discriminant(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Int)
    }
}

impl EnumVariantFields {
    pub fn fields(&self) -> Vec<StructField> {
        self.syntax()
            .children()
            .filter_map(StructField::cast)
            .collect()
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

    pub fn export(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Export)
    }

    pub fn inline(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Inline)
    }
}

impl ImportItem {
    pub fn path(&self) -> Option<ImportPath> {
        self.syntax().children().find_map(ImportPath::cast)
    }
}

impl ImportPath {
    pub fn idents(&self) -> Vec<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .filter(|token| token.kind() == SyntaxKind::Ident)
            .collect()
    }

    pub fn group(&self) -> Option<ImportGroup> {
        self.syntax().children().find_map(ImportGroup::cast)
    }
}

impl ImportGroup {
    pub fn paths(&self) -> Vec<ImportPath> {
        self.syntax()
            .children()
            .filter_map(ImportPath::cast)
            .collect()
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

impl AssumeStmt {
    pub fn expr(&self) -> Option<Expr> {
        self.syntax().children().find_map(Expr::cast)
    }
}

impl PathExpr {
    pub fn idents(&self) -> Vec<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .filter(|token| token.kind() == SyntaxKind::Ident)
            .collect()
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
        self.syntax().children().find_map(Expr::cast)
    }
}

impl LiteralExpr {
    pub fn value(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .find_map(SyntaxElement::into_token)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrefixOp {
    BitwiseNot,
    Not,
    Positive,
    Negative,
}

fn prefix_op(kind: SyntaxKind) -> Option<PrefixOp> {
    match kind {
        SyntaxKind::BitwiseNot => Some(PrefixOp::BitwiseNot),
        SyntaxKind::Not => Some(PrefixOp::Not),
        SyntaxKind::Plus => Some(PrefixOp::Positive),
        SyntaxKind::Minus => Some(PrefixOp::Negative),
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
        self.syntax().children().find_map(Expr::cast)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LeftArithShift,
    RightArithShift,
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
    And,
    Or,
}

fn binary_op(kind: SyntaxKind) -> Option<BinaryOp> {
    match kind {
        SyntaxKind::BitwiseAnd => Some(BinaryOp::BitwiseAnd),
        SyntaxKind::BitwiseOr => Some(BinaryOp::BitwiseOr),
        SyntaxKind::BitwiseXor => Some(BinaryOp::BitwiseXor),
        SyntaxKind::LeftArithShift => Some(BinaryOp::LeftArithShift),
        SyntaxKind::RightArithShift => Some(BinaryOp::RightArithShift),
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
        SyntaxKind::And => Some(BinaryOp::And),
        SyntaxKind::Or => Some(BinaryOp::Or),
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
    pub fn generic_params(&self) -> Option<GenericParams> {
        self.syntax().children().find_map(GenericParams::cast)
    }

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

    pub fn optional(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Question)
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

impl FunctionCallExpr {
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

impl FieldAccessExpr {
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

impl PathType {
    pub fn idents(&self) -> Vec<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .filter(|token| token.kind() == SyntaxKind::Ident)
            .collect()
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

    pub fn return_type(&self) -> Option<Type> {
        self.syntax().children().find_map(Type::cast)
    }
}

impl FunctionTypeParam {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    pub fn optional(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Question)
    }

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

impl UnionType {
    pub fn types(&self) -> Vec<Type> {
        self.syntax().children().filter_map(Type::cast).collect()
    }
}

impl GenericParams {
    pub fn names(&self) -> Vec<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .filter(|token| token.kind() == SyntaxKind::Ident)
            .collect()
    }
}
