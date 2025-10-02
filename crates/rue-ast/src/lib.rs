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
    ModuleItem,
    FunctionItem,
    FunctionParameter,
    ConstantItem,
    TypeAliasItem,
    StructItem,
    StructField,
    GenericParameters,
    GenericArguments,
    LiteralType,
    PathType,
    UnionType,
    GroupType,
    PairType,
    ListType,
    ListTypeItem,
    LambdaType,
    LambdaParameter,
    Block,
    LetStmt,
    ExprStmt,
    IfStmt,
    ReturnStmt,
    AssertStmt,
    RaiseStmt,
    PathExpr,
    PathSegment,
    LeadingPathSeparator,
    StructInitializerExpr,
    StructInitializerField,
    LiteralExpr,
    GroupExpr,
    PairExpr,
    ListExpr,
    ListItem,
    PrefixExpr,
    BinaryExpr,
    FunctionCallExpr,
    IfExpr,
    GuardExpr,
    CastExpr,
    FieldAccessExpr,
    LambdaExpr,
    NamedBinding,
    PairBinding,
    ListBinding,
    ListBindingItem,
    StructBinding,
    StructFieldBinding,
);

ast_enum!(Item, TypeItem, SymbolItem);
ast_enum!(TypeItem, TypeAliasItem, StructItem);
ast_enum!(SymbolItem, ModuleItem, FunctionItem, ConstantItem);
ast_enum!(
    Stmt, LetStmt, ExprStmt, IfStmt, ReturnStmt, AssertStmt, RaiseStmt
);
ast_enum!(StmtOrExpr, Stmt, Expr);
ast_enum!(
    Expr,
    PathExpr,
    StructInitializerExpr,
    LiteralExpr,
    GroupExpr,
    PairExpr,
    ListExpr,
    PrefixExpr,
    BinaryExpr,
    FunctionCallExpr,
    Block,
    IfExpr,
    GuardExpr,
    CastExpr,
    FieldAccessExpr,
    LambdaExpr,
);
ast_enum!(
    Type,
    LiteralType,
    PathType,
    UnionType,
    GroupType,
    PairType,
    ListType,
    LambdaType,
);
ast_enum!(
    Binding,
    NamedBinding,
    PairBinding,
    ListBinding,
    StructBinding,
);

impl AstDocument {
    pub fn items(&self) -> impl Iterator<Item = AstItem> {
        self.syntax().children().filter_map(AstItem::cast)
    }
}

impl AstModuleItem {
    pub fn export(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == T![export])
    }

    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    pub fn items(&self) -> impl Iterator<Item = AstItem> {
        self.syntax().children().filter_map(AstItem::cast)
    }
}

impl AstFunctionItem {
    pub fn export(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == T![export])
    }

    pub fn extern_kw(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == T![extern])
    }

    pub fn inline(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == T![inline])
    }

    pub fn test(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == T![test])
    }

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

impl AstConstantItem {
    pub fn export(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == T![export])
    }

    pub fn inline(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == T![inline])
    }

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

impl AstTypeAliasItem {
    pub fn export(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == T![export])
    }

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

impl AstStructItem {
    pub fn export(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == T![export])
    }

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

    pub fn fields(&self) -> impl Iterator<Item = AstStructField> {
        self.syntax().children().filter_map(AstStructField::cast)
    }
}

impl AstStructField {
    pub fn spread(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == T![...])
    }

    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    pub fn ty(&self) -> Option<AstType> {
        self.syntax().children().find_map(AstType::cast)
    }

    pub fn expr(&self) -> Option<AstExpr> {
        self.syntax().children().find_map(AstExpr::cast)
    }
}

impl AstFunctionParameter {
    pub fn spread(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == T![...])
    }

    pub fn binding(&self) -> Option<AstBinding> {
        self.syntax().children().find_map(AstBinding::cast)
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
    pub fn inline(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == T![inline])
    }

    pub fn binding(&self) -> Option<AstBinding> {
        self.syntax().children().find_map(AstBinding::cast)
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

impl AstIfStmt {
    pub fn inline(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == T![inline])
    }

    pub fn condition(&self) -> Option<AstExpr> {
        self.syntax().children().find_map(AstExpr::cast)
    }

    pub fn then_block(&self) -> Option<AstBlock> {
        self.syntax()
            .children()
            .filter(|node| AstExpr::cast(node.clone()).is_some())
            .nth(1)
            .and_then(AstBlock::cast)
    }
}

impl AstReturnStmt {
    pub fn expr(&self) -> Option<AstExpr> {
        self.syntax().children().find_map(AstExpr::cast)
    }
}

impl AstAssertStmt {
    pub fn expr(&self) -> Option<AstExpr> {
        self.syntax().children().find_map(AstExpr::cast)
    }
}

impl AstRaiseStmt {
    pub fn expr(&self) -> Option<AstExpr> {
        self.syntax().children().find_map(AstExpr::cast)
    }
}

impl AstPathExpr {
    pub fn segments(&self) -> impl Iterator<Item = AstPathSegment> {
        self.syntax().children().filter_map(AstPathSegment::cast)
    }
}

impl AstPathSegment {
    pub fn initial_separator(&self) -> Option<AstLeadingPathSeparator> {
        self.syntax()
            .children()
            .find_map(AstLeadingPathSeparator::cast)
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

impl AstStructInitializerExpr {
    pub fn path(&self) -> Option<AstPathExpr> {
        self.syntax().children().find_map(AstPathExpr::cast)
    }

    pub fn fields(&self) -> impl Iterator<Item = AstStructInitializerField> {
        self.syntax()
            .children()
            .filter_map(AstStructInitializerField::cast)
    }
}

impl AstStructInitializerField {
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

impl AstLiteralExpr {
    pub fn value(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| SyntaxKind::LITERAL.contains(&token.kind()))
    }
}

impl AstGroupExpr {
    pub fn expr(&self) -> Option<AstExpr> {
        self.syntax().children().find_map(AstExpr::cast)
    }
}

impl AstPairExpr {
    pub fn first(&self) -> Option<AstExpr> {
        self.syntax().children().find_map(AstExpr::cast)
    }

    pub fn rest(&self) -> Option<AstExpr> {
        self.syntax().children().filter_map(AstExpr::cast).nth(1)
    }
}

impl AstListExpr {
    pub fn items(&self) -> impl Iterator<Item = AstListItem> {
        self.syntax().children().filter_map(AstListItem::cast)
    }
}

impl AstListItem {
    pub fn spread(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == T![...])
    }

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

    pub fn args(&self) -> impl Iterator<Item = AstListItem> {
        self.syntax().children().filter_map(AstListItem::cast)
    }
}

impl AstIfExpr {
    pub fn inline(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == T![inline])
    }

    pub fn condition(&self) -> Option<AstExpr> {
        self.syntax().children().find_map(AstExpr::cast)
    }

    pub fn then_expr(&self) -> Option<AstExpr> {
        self.syntax().children().filter_map(AstExpr::cast).nth(1)
    }

    pub fn else_expr(&self) -> Option<AstExpr> {
        self.syntax().children().filter_map(AstExpr::cast).nth(2)
    }
}

impl AstGuardExpr {
    pub fn expr(&self) -> Option<AstExpr> {
        self.syntax().children().find_map(AstExpr::cast)
    }

    pub fn ty(&self) -> Option<AstType> {
        self.syntax().children().find_map(AstType::cast)
    }
}

impl AstCastExpr {
    pub fn expr(&self) -> Option<AstExpr> {
        self.syntax().children().find_map(AstExpr::cast)
    }

    pub fn ty(&self) -> Option<AstType> {
        self.syntax().children().find_map(AstType::cast)
    }
}

impl AstFieldAccessExpr {
    pub fn expr(&self) -> Option<AstExpr> {
        self.syntax().children().find_map(AstExpr::cast)
    }

    pub fn field(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }
}

impl AstLambdaExpr {
    pub fn parameters(&self) -> impl Iterator<Item = AstFunctionParameter> {
        self.syntax()
            .children()
            .filter_map(AstFunctionParameter::cast)
    }

    pub fn ty(&self) -> Option<AstType> {
        self.syntax().children().find_map(AstType::cast)
    }

    pub fn body(&self) -> Option<AstExpr> {
        self.syntax().children().find_map(AstExpr::cast)
    }
}

impl AstLiteralType {
    pub fn value(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| SyntaxKind::LITERAL.contains(&token.kind()))
    }
}

impl AstPathType {
    pub fn segments(&self) -> impl Iterator<Item = AstPathSegment> {
        self.syntax().children().filter_map(AstPathSegment::cast)
    }
}

impl AstUnionType {
    pub fn types(&self) -> impl Iterator<Item = AstType> {
        self.syntax().children().filter_map(AstType::cast)
    }
}

impl AstGroupType {
    pub fn ty(&self) -> Option<AstType> {
        self.syntax().children().find_map(AstType::cast)
    }
}

impl AstPairType {
    pub fn first(&self) -> Option<AstType> {
        self.syntax().children().find_map(AstType::cast)
    }

    pub fn rest(&self) -> Option<AstType> {
        self.syntax().children().filter_map(AstType::cast).nth(1)
    }
}

impl AstListType {
    pub fn items(&self) -> impl Iterator<Item = AstListTypeItem> {
        self.syntax().children().filter_map(AstListTypeItem::cast)
    }
}

impl AstListTypeItem {
    pub fn spread(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == T![...])
    }

    pub fn ty(&self) -> Option<AstType> {
        self.syntax().children().find_map(AstType::cast)
    }
}

impl AstLambdaType {
    pub fn parameters(&self) -> impl Iterator<Item = AstLambdaParameter> {
        self.syntax()
            .children()
            .filter_map(AstLambdaParameter::cast)
    }

    pub fn return_type(&self) -> Option<AstType> {
        self.syntax().children().find_map(AstType::cast)
    }
}

impl AstLambdaParameter {
    pub fn spread(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == T![...])
    }

    pub fn ty(&self) -> Option<AstType> {
        self.syntax().children().find_map(AstType::cast)
    }
}

impl AstNamedBinding {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }
}

impl AstPairBinding {
    pub fn first(&self) -> Option<AstBinding> {
        self.syntax().children().find_map(AstBinding::cast)
    }

    pub fn rest(&self) -> Option<AstBinding> {
        self.syntax().children().filter_map(AstBinding::cast).nth(1)
    }
}

impl AstListBinding {
    pub fn items(&self) -> impl Iterator<Item = AstListBindingItem> {
        self.syntax()
            .children()
            .filter_map(AstListBindingItem::cast)
    }
}

impl AstListBindingItem {
    pub fn spread(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == T![...])
    }

    pub fn binding(&self) -> Option<AstBinding> {
        self.syntax().children().find_map(AstBinding::cast)
    }
}

impl AstStructBinding {
    pub fn fields(&self) -> impl Iterator<Item = AstStructFieldBinding> {
        self.syntax()
            .children()
            .filter_map(AstStructFieldBinding::cast)
    }
}

impl AstStructFieldBinding {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    pub fn binding(&self) -> Option<AstBinding> {
        self.syntax().children().find_map(AstBinding::cast)
    }
}
