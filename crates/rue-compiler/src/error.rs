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
    #[error("unused module `{0}`")]
    UnusedModule(String),

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

    #[error("unused enum `{0}`")]
    UnusedEnum(String),

    #[error("unused enum variant `{0}`")]
    UnusedEnumVariant(String),

    #[error("unused struct `{0}`")]
    UnusedStruct(String),

    #[error("unused type alias `{0}`")]
    UnusedTypeAlias(String),

    #[error("type is already optional")]
    UselessOptionalType,

    #[error("value already has type `{0}`")]
    RedundantTypeCheck(String),
}

#[derive(Debug, Error, Clone, PartialEq, Eq, Hash)]
pub enum ErrorKind {
    #[error("missing `main` function")]
    MissingMain,

    #[error("unknown symbol `{0}`")]
    UndefinedReference(String),

    #[error("unknown type `{0}`")]
    UndefinedType(String),

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

    #[error("unknown enum variant `{0}`")]
    UnknownEnumVariant(String),

    #[error("paths are not allowed in this context")]
    PathNotAllowed,

    #[error("cannot path into non-enum type `{0}`")]
    PathIntoNonEnum(String),

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

    #[error("blocks must either have a an expression value, return statement, or raise an error")]
    EmptyBlock,

    #[error("cannot check equality on non-atom type `{0}`")]
    NonAtomEquality(String),
}

/// Join a list of names into a string, wrapped in backticks.
fn join_names(kinds: &[String]) -> String {
    let names: Vec<String> = kinds.iter().map(|kind| format!("`{kind}`")).collect();
    names.join(", ")
}
