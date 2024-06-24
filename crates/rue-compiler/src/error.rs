use std::ops::Range;

use thiserror::Error;

#[derive(Debug, Clone)]
pub struct Diagnostic {
    kind: DiagnosticKind,
    span: Range<usize>,
}

impl Diagnostic {
    pub fn new(kind: DiagnosticKind, span: Range<usize>) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &DiagnosticKind {
        &self.kind
    }

    pub fn span(&self) -> &Range<usize> {
        &self.span
    }

    pub fn is_error(&self) -> bool {
        matches!(self.kind, DiagnosticKind::Error(_))
    }

    pub fn is_warning(&self) -> bool {
        matches!(self.kind, DiagnosticKind::Warning(_))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DiagnosticKind {
    Warning(WarningKind),
    Error(ErrorKind),
}

#[derive(Debug, Error, Clone, PartialEq, Eq, Hash)]
pub enum WarningKind {
    #[error("unused function `{0}`")]
    UnusedFunction(String),

    #[error("unused inline function `{0}`")]
    UnusedInlineFunction(String),

    #[error("unused parameter `{0}`")]
    UnusedParameter(String),

    #[error("unused constant `{0}`")]
    UnusedConst(String),

    #[error("unused inline constant `{0}`")]
    UnusedInlineConst(String),

    #[error("unused let binding `{0}`")]
    UnusedLet(String),

    #[error("unused generic type `{0}`")]
    UnusedGenericType(String),

    #[error("unused enum `{0}`")]
    UnusedEnum(String),

    #[error("unused enum variant `{0}`")]
    UnusedEnumVariant(String),

    #[error("unused struct `{0}`")]
    UnusedStruct(String),

    #[error("unused type alias `{0}`")]
    UnusedTypeAlias(String),

    #[error("type is already optional")]
    RedundantOptional,

    #[error("value already has type `{0}`")]
    RedundantTypeCheck(String),
}

#[derive(Debug, Error, Clone, PartialEq, Eq, Hash)]
pub enum ErrorKind {
    #[error("type with namespace `{0}` is already defined")]
    NamespaceTakenType(String),

    #[error("symbol with namespace `{0}` is already defined")]
    NamespaceTakenSymbol(String),

    #[error("type `{0}` is already defined")]
    DuplicateType(String),

    #[error("symbol `{0}` is already defined")]
    DuplicateSymbol(String),

    #[error("unknown symbol `{0}`")]
    UnknownSymbol(String),

    #[error("unknown type `{0}`")]
    UnknownType(String),

    #[error("inline functions cannot be referenced without being called")]
    InlineFunctionReference,

    #[error("modules cannot be referenced, since they are not values")]
    ModuleReference,

    #[error("type aliases cannot reference themselves")]
    RecursiveTypeAlias,

    #[error("expected type `{expected}`, but found `{found}`")]
    TypeMismatch { expected: String, found: String },

    #[error("cannot cast type `{found}` to `{expected}`")]
    CastMismatch { expected: String, found: String },

    #[error("cannot call expression with type `{0}`")]
    UncallableType(String),

    #[error("expected {expected} arguments, but found {found}")]
    ArgumentMismatch { expected: usize, found: usize },

    #[error("expected at least {expected} arguments, but found {found}")]
    TooFewArgumentsWithVarargs { expected: usize, found: usize },

    #[error("uninitializable type `{0}`")]
    UninitializableType(String),

    #[error("duplicate field `{0}`")]
    DuplicateField(String),

    #[error("undefined field `{field}` on type `{ty}`")]
    UndefinedField { field: String, ty: String },

    #[error("missing fields on type `{ty}`: {}", join_names(.fields))]
    MissingFields { fields: Vec<String>, ty: String },

    #[error("cannot access field `{field}` of non-struct type `{ty}`")]
    InvalidFieldAccess { field: String, ty: String },

    #[error("cannot index into non-list type `{0}`")]
    IndexAccess(String),

    #[error("the spread operator can only be used on the last element")]
    NonFinalSpread,

    #[error("cannot spread expression in non-vararg function call")]
    NonVarargSpread,

    #[error("cannot pass arguments directly (without spreading) to non-list vararg function call")]
    NonListVararg,

    #[error("duplicate enum variant `{0}`")]
    DuplicateEnumVariant(String),

    #[error("duplicate enum discriminant `{0}`")]
    DuplicateEnumDiscriminant(String),

    #[error("enum discriminant too large")]
    EnumDiscriminantTooLarge,

    #[error("cannot reference enum variants of enums with fields")]
    EnumVariantWithFields,

    #[error("unknown enum variant `{0}`")]
    UnknownEnumVariant(String),

    #[error("could not resolve `{0}` in module")]
    UnknownModulePath(String),

    #[error("symbol `{0}` is private")]
    PrivateSymbol(String),

    #[error("type `{0}` is private")]
    PrivateType(String),

    #[error("cannot path into type `{0}`")]
    InvalidTypePath(String),

    #[error("cannot path into symbol")]
    InvalidSymbolPath,

    #[error("expected path to type, but found symbol")]
    ExpectedTypePath,

    #[error("expected path to symbol, but found type")]
    ExpectedSymbolPath,

    #[error("cannot check type `{from}` against `{to}`")]
    UnsupportedTypeGuard { from: String, to: String },

    #[error("cannot check `Any` against pair with types other than `Any`")]
    NonAnyPairTypeGuard,

    #[error("cannot check list against pair with types other than the list item type and the list itself")]
    NonListPairTypeGuard,

    #[error("implicit return is not allowed in if statements, use an explicit return statement")]
    ImplicitReturnInIf,

    #[error("explicit return is not allowed in expressions")]
    ExplicitReturnInExpr,

    #[error("block missing return value")]
    EmptyBlock,

    #[error("cannot check equality on non-atom type `{0}`")]
    NonAtomEquality(String),

    #[error("integer too large to allocate in memory")]
    IntegerTooLarge,
}

/// Join a list of names into a string, wrapped in backticks.
fn join_names(kinds: &[String]) -> String {
    let names: Vec<String> = kinds.iter().map(|kind| format!("`{kind}`")).collect();
    names.join(", ")
}
