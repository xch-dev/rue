use paste::paste;
use rue_parser::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken, T};

pub trait AstNode {
    fn cast(node: SyntaxNode) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxNode;
}

macro_rules! ast_nodes {
    ($( $kind:ident),+ $(,)?) => { paste! {
        /// The exhaustive set of syntax kinds with typed AST node wrappers.
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub enum AstNodeKind {
            $( $kind, )+
        }

        impl AstNodeKind {
            pub const ALL: &'static [Self] = &[
                $( Self::$kind, )+
            ];

            pub fn from_syntax(kind: SyntaxKind) -> Option<Self> {
                match kind {
                    $( SyntaxKind::$kind => Some(Self::$kind), )+
                    _ => None,
                }
            }

            pub fn of(node: &SyntaxNode) -> Option<Self> {
                Self::from_syntax(node.kind())
            }
        }

        impl TryFrom<SyntaxKind> for AstNodeKind {
            type Error = SyntaxKind;

            fn try_from(kind: SyntaxKind) -> Result<Self, Self::Error> {
                Self::from_syntax(kind).ok_or(kind)
            }
        }

        impl From<AstNodeKind> for SyntaxKind {
            fn from(kind: AstNodeKind) -> Self {
                match kind {
                    $( AstNodeKind::$kind => Self::$kind, )+
                }
            }
        }

        $(
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
        )+
    } };
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
    Attribute,
    FunctionItem,
    FunctionParameter,
    ConstantItem,
    TypeAliasItem,
    StructItem,
    StructField,
    EnumItem,
    EnumVariant,
    EnumVariantFields,
    EnumDiscriminant,
    ImportItem,
    ImportPath,
    ImportPathSegment,
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
    DebugStmt,
    PathExpr,
    PathSegment,
    StructInitializerExpr,
    StructInitializerField,
    LiteralExpr,
    ConstExpr,
    GroupExpr,
    PairExpr,
    ListExpr,
    ListItem,
    PrefixExpr,
    BinaryExpr,
    FunctionCallExpr,
    IfExpr,
    MatchExpr,
    MatchArm,
    GuardExpr,
    CastExpr,
    FieldAccessExpr,
    IndexExpr,
    SliceExpr,
    LambdaExpr,
    NamedBinding,
    PairBinding,
    ListBinding,
    ListBindingItem,
    StructBinding,
    StructFieldBinding,
);

ast_enum!(Item, TypeItem, SymbolItem, ImportItem);
ast_enum!(TypeItem, TypeAliasItem, StructItem, EnumItem);
ast_enum!(SymbolItem, ModuleItem, FunctionItem, ConstantItem);
ast_enum!(
    Stmt, LetStmt, ExprStmt, IfStmt, ReturnStmt, AssertStmt, RaiseStmt, DebugStmt
);
ast_enum!(StmtOrExpr, Stmt, Expr);
ast_enum!(
    Expr,
    PathExpr,
    StructInitializerExpr,
    LiteralExpr,
    ConstExpr,
    GroupExpr,
    PairExpr,
    ListExpr,
    PrefixExpr,
    BinaryExpr,
    FunctionCallExpr,
    Block,
    IfExpr,
    MatchExpr,
    GuardExpr,
    CastExpr,
    FieldAccessExpr,
    IndexExpr,
    SliceExpr,
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

    /// The inner attributes (such as `#![allow(unused_binding)]`) at the start of the file.
    pub fn inner_attributes(&self) -> impl Iterator<Item = AstAttribute> {
        leading_inner_attributes(self.syntax().children_with_tokens())
    }
}

impl AstItem {
    /// The attributes before the item, such as `#[allow(unused_function)]` before a function.
    /// This doesn't include the inner attributes at the start of a module's body.
    pub fn attributes(&self) -> impl Iterator<Item = AstAttribute> {
        leading_attributes(self.syntax().children_with_tokens())
    }
}

/// The attributes at the start of a sequence of elements, ignoring trivia.
fn leading_attributes(
    elements: impl Iterator<Item = SyntaxElement>,
) -> impl Iterator<Item = AstAttribute> {
    elements
        .filter(|element| !element.kind().is_trivia())
        .map_while(|element| element.into_node().and_then(AstAttribute::cast))
}

/// The inner attributes at the start of a sequence of elements, ignoring trivia. Inner attributes
/// after anything else (including outer attributes) aren't at the start, so they're misplaced.
fn leading_inner_attributes(
    elements: impl Iterator<Item = SyntaxElement>,
) -> impl Iterator<Item = AstAttribute> {
    leading_attributes(elements).take_while(AstAttribute::is_inner)
}

impl AstAttribute {
    /// Whether this is an inner attribute (such as `#![allow(unused_binding)]`), which applies to
    /// the file or module that it's in rather than to the next item.
    pub fn is_inner(&self) -> bool {
        // The `!` comes right after the `#`
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .filter(|token| !token.kind().is_trivia())
            .nth(1)
            .is_some_and(|token| token.kind() == T![!])
    }

    /// The name of the attribute, such as `allow` in `#[allow(unused_binding)]`.
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    /// The arguments in parentheses, such as `unused_binding` in `#[allow(unused_binding)]`.
    pub fn arguments(&self) -> impl Iterator<Item = SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .filter(|token| token.kind() == SyntaxKind::Ident)
            .skip(1)
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

    /// The inner attributes (such as `#![allow(unused_binding)]`) at the start of the module's
    /// body, after the opening brace.
    pub fn inner_attributes(&self) -> impl Iterator<Item = AstAttribute> {
        leading_inner_attributes(
            self.syntax()
                .children_with_tokens()
                .skip_while(|element| element.kind() != T!['{'])
                .skip(1),
        )
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

    pub fn source_path(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::String)
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

impl AstEnumItem {
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

    pub fn variants(&self) -> impl Iterator<Item = AstEnumVariant> {
        self.syntax().children().filter_map(AstEnumVariant::cast)
    }
}

impl AstEnumVariant {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    pub fn discriminant(&self) -> Option<AstEnumDiscriminant> {
        self.syntax().children().find_map(AstEnumDiscriminant::cast)
    }

    /// The braces with the variant's fields, which a variant without fields doesn't have.
    pub fn fields(&self) -> Option<AstEnumVariantFields> {
        self.syntax()
            .children()
            .find_map(AstEnumVariantFields::cast)
    }

    /// The fields in the variant's braces, if it has them.
    pub fn struct_fields(&self) -> impl Iterator<Item = AstStructField> {
        self.fields()
            .into_iter()
            .flat_map(|fields| fields.syntax().children())
            .filter_map(AstStructField::cast)
    }
}

impl AstEnumDiscriminant {
    pub fn minus(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == T![-])
    }

    /// The integer literal token, if it was written.
    pub fn value(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| SyntaxKind::INTEGER_LITERAL.contains(&token.kind()))
    }
}

impl AstImportItem {
    pub fn export(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == T![export])
    }

    pub fn path(&self) -> Option<AstImportPath> {
        self.syntax().children().find_map(AstImportPath::cast)
    }
}

impl AstImportPath {
    pub fn segments(&self) -> impl Iterator<Item = AstImportPathSegment> {
        self.syntax()
            .children()
            .filter_map(AstImportPathSegment::cast)
    }
}

impl AstImportPathSegment {
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
            .find(|token| matches!(token.kind(), SyntaxKind::Ident | T![super] | T![project]))
    }

    pub fn star(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == T![*])
    }

    pub fn items(&self) -> impl Iterator<Item = AstImportPath> {
        self.syntax().children().filter_map(AstImportPath::cast)
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
    /// The condition, which is before the `,` if there is one.
    pub fn expr(&self) -> Option<AstExpr> {
        self.syntax()
            .children_with_tokens()
            .take_while(|element| element.kind() != T![,])
            .filter_map(SyntaxElement::into_node)
            .find_map(AstExpr::cast)
    }

    /// The message after the `,`, which a debug build raises along with the location of the
    /// statement if the condition is false.
    pub fn message(&self) -> Option<AstExpr> {
        self.syntax()
            .children_with_tokens()
            .skip_while(|element| element.kind() != T![,])
            .filter_map(SyntaxElement::into_node)
            .find_map(AstExpr::cast)
    }
}

impl AstRaiseStmt {
    pub fn expr(&self) -> Option<AstExpr> {
        self.syntax().children().find_map(AstExpr::cast)
    }
}

impl AstDebugStmt {
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
    pub fn separator(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == T![::])
            .filter(|token| {
                let Some(name) = self.name() else {
                    return true;
                };
                token.text_range().start() < name.text_range().start()
            })
    }

    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| matches!(token.kind(), SyntaxKind::Ident | T![super] | T![project]))
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

impl AstConstExpr {
    pub fn block(&self) -> Option<AstBlock> {
        self.syntax().children().find_map(AstBlock::cast)
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

impl AstExpr {
    /// Whether the expression is a block, `if` or `match`, which ends at its closing brace rather
    /// than continuing with an operator where a statement or match arm can end.
    pub fn ends_with_brace(&self) -> bool {
        matches!(self, Self::Block(_) | Self::IfExpr(_) | Self::MatchExpr(_))
    }

    /// Whether the expression is an `if` or `match`, which can be a statement whose branches are
    /// blocks of statements.
    pub fn is_if_or_match(&self) -> bool {
        matches!(self, Self::IfExpr(_) | Self::MatchExpr(_))
    }
}

impl AstIfExpr {
    /// The `else` keyword, which an `else if` statement can leave out.
    pub fn else_token(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == T![else])
    }

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

impl AstMatchExpr {
    /// The `inline` keyword, which makes every arm evaluated eagerly, like an `inline if`.
    pub fn inline(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == T![inline])
    }

    /// The value being matched on.
    pub fn expr(&self) -> Option<AstExpr> {
        self.syntax().children().find_map(AstExpr::cast)
    }

    pub fn arms(&self) -> impl Iterator<Item = AstMatchArm> {
        self.syntax().children().filter_map(AstMatchArm::cast)
    }

    /// Whether the arms were parsed without errors, between both braces. Otherwise some of them
    /// could be missing, such as after an arm with an unclosed delimiter.
    pub fn arms_are_complete(&self) -> bool {
        let mut open = false;
        let mut close = false;

        for element in self.syntax().children_with_tokens() {
            match element.kind() {
                T!['{'] => open = true,
                T!['}'] => close = true,
                _ => {}
            }
        }

        open && close
            && !self
                .arms()
                .flat_map(|arm| arm.syntax().descendants_with_tokens().collect::<Vec<_>>())
                .chain(self.syntax().children_with_tokens())
                .any(|element| element.kind() == SyntaxKind::Error)
    }
}

impl AstMatchArm {
    /// The type that the value is checked against, or `_` (see [`AstType::is_placeholder`]) for
    /// every value that's left.
    pub fn ty(&self) -> Option<AstType> {
        self.syntax().children().find_map(AstType::cast)
    }

    /// The `as` before the arm's binding, which a struct binding can leave out.
    pub fn as_token(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == T![as])
    }

    /// The binding after the type, which names or destructures the value once it's been narrowed
    /// to the type, such as `{ x, y }` in `Point { x, y } => x + y` and `point` in
    /// `Point as point => point.x`. It doesn't affect which values the arm matches.
    pub fn binding(&self) -> Option<AstBinding> {
        self.syntax().children().find_map(AstBinding::cast)
    }

    pub fn expr(&self) -> Option<AstExpr> {
        self.syntax().children().find_map(AstExpr::cast)
    }

    /// Whether the arm's value is a block, `if` or `match`, which ends at its closing brace, so no
    /// comma is needed after the arm.
    pub fn ends_with_brace(&self) -> bool {
        self.expr().is_some_and(|expr| expr.ends_with_brace())
    }

    /// The comma after the arm, if there is one.
    pub fn comma(&self) -> Option<SyntaxToken> {
        self.comma_and_comments().0
    }

    /// The comma after the arm, if it can be left out: the arm ends with a brace, and there's no
    /// comment between the arm and the comma, which would otherwise be left next to the next arm.
    pub fn removable_comma(&self) -> Option<SyntaxToken> {
        let (comma, has_comments) = self.comma_and_comments();
        comma.filter(|_| self.ends_with_brace() && !has_comments)
    }

    fn comma_and_comments(&self) -> (Option<SyntaxToken>, bool) {
        let mut has_comments = false;
        let mut next = self.syntax().next_sibling_or_token();
        while let Some(element) = next {
            if !element.kind().is_trivia() {
                let comma = element.into_token().filter(|token| token.kind() == T![,]);
                return (comma, has_comments);
            }
            has_comments |= matches!(
                element.kind(),
                SyntaxKind::LineComment | SyntaxKind::BlockComment
            );
            next = element.next_sibling_or_token();
        }
        (None, has_comments)
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

    pub fn dot(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == T![.])
    }

    pub fn field(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }
}

impl AstIndexExpr {
    /// The value that's indexed.
    pub fn expr(&self) -> Option<AstExpr> {
        self.syntax().children().find_map(AstExpr::cast)
    }

    pub fn index(&self) -> Option<AstExpr> {
        self.syntax().children().filter_map(AstExpr::cast).nth(1)
    }
}

impl AstSliceExpr {
    /// The value that's sliced.
    pub fn expr(&self) -> Option<AstExpr> {
        self.syntax().children().find_map(AstExpr::cast)
    }

    pub fn range(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == T![..])
    }

    /// Whether the slice parsed without errors, so that a bound that's missing was left out
    /// rather than failing to parse.
    pub fn is_complete(&self) -> bool {
        self.syntax()
            .children_with_tokens()
            .all(|element| element.kind() != SyntaxKind::Error)
            && self
                .syntax()
                .last_token()
                .is_some_and(|token| token.kind() == T![']'])
    }

    /// The bound before the `..`, if there is one.
    pub fn start(&self) -> Option<AstExpr> {
        self.bounds().0
    }

    /// The bound after the `..`, if there is one.
    pub fn end(&self) -> Option<AstExpr> {
        self.bounds().1
    }

    fn bounds(&self) -> (Option<AstExpr>, Option<AstExpr>) {
        let mut start = None;
        let mut end = None;
        let mut in_brackets = false;
        let mut after_range = false;

        for element in self.syntax().children_with_tokens() {
            match element {
                SyntaxElement::Token(token) => {
                    in_brackets |= token.kind() == T!['['];
                    after_range |= token.kind() == T![..];
                }
                SyntaxElement::Node(node) => {
                    // The expression before the brackets is the value that's sliced
                    let Some(expr) = AstExpr::cast(node).filter(|_| in_brackets) else {
                        continue;
                    };
                    if after_range {
                        end.get_or_insert(expr);
                    } else {
                        start.get_or_insert(expr);
                    }
                }
            }
        }

        (start, end)
    }
}

impl AstLambdaExpr {
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

impl AstType {
    /// Whether the type is `_`, which stands for a type to be filled in, such as a generic argument
    /// to infer or a match arm that handles every value that's left.
    pub fn is_placeholder(&self) -> bool {
        let Self::PathType(path) = self else {
            return false;
        };

        let mut segments = path.segments();

        segments.next().is_some_and(|segment| {
            segment.separator().is_none()
                && segment.generic_arguments().is_none()
                && segment.name().is_some_and(|name| name.text() == "_")
        }) && segments.next().is_none()
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
    pub fn generic_parameters(&self) -> Option<AstGenericParameters> {
        self.syntax()
            .children()
            .find_map(AstGenericParameters::cast)
    }

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

    pub fn binding(&self) -> Option<AstBinding> {
        self.syntax().children().find_map(AstBinding::cast)
    }
}

#[cfg(test)]
mod tests {
    use num_traits::FromPrimitive;

    use super::*;

    #[test]
    fn every_parser_node_kind_has_an_ast_kind() {
        let first = SyntaxKind::Document as u16;
        let end = SyntaxKind::Error as u16;
        assert_eq!(AstNodeKind::ALL.len(), usize::from(end - first));

        for discriminant in first..end {
            let syntax = SyntaxKind::from_u16(discriminant)
                .expect("each contiguous parser node discriminant is valid");
            let ast = AstNodeKind::try_from(syntax)
                .unwrap_or_else(|kind| panic!("missing AST node kind for {kind:?}"));
            assert_eq!(SyntaxKind::from(ast), syntax);
        }
    }

    #[test]
    fn tokens_trivia_and_sentinels_are_not_ast_node_kinds() {
        for kind in [
            SyntaxKind::Whitespace,
            SyntaxKind::LineComment,
            SyntaxKind::Ident,
            SyntaxKind::Import,
            SyntaxKind::OpenBrace,
            SyntaxKind::Plus,
            SyntaxKind::Error,
            SyntaxKind::Eof,
        ] {
            assert_eq!(AstNodeKind::from_syntax(kind), None);
            assert_eq!(AstNodeKind::try_from(kind), Err(kind));
        }
    }
}
