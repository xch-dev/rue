use std::ops::Range;

use thiserror::Error;

#[derive(Debug)]
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
    #[error("redundant optional type")]
    RedundantOptional,

    #[error("redundant check against same type `{0}`")]
    RedundantTypeGuard(String),

    #[error("unused symbol `{0}`")]
    UnusedSymbol(String),

    #[error("unused type `{0}`")]
    UnusedType(String),
}

#[derive(Debug, Error, Clone, PartialEq, Eq, Hash)]
pub enum ErrorKind {
    #[error("missing `main` function")]
    MissingMain,

    #[error("undefined reference `{0}`")]
    UndefinedReference(String),

    #[error("undefined type `{0}`")]
    UndefinedType(String),

    #[error("recursive type alias")]
    RecursiveTypeAlias,

    #[error("expected {expected} arguments, found {found}")]
    ArgumentMismatch { expected: usize, found: usize },

    #[error("expected type `{expected}`, found `{found}`")]
    TypeMismatch { expected: String, found: String },

    #[error("cannot cast type `{found}` to `{expected}`")]
    CastMismatch { expected: String, found: String },

    #[error("cannot call expression with type `{0}`")]
    UncallableType(String),

    #[error("uninitializable type `{0}`")]
    UninitializableType(String),

    #[error("duplicate field `{0}`")]
    DuplicateField(String),

    #[error("undefined field `{0}`")]
    UndefinedField(String),

    #[error("missing fields: {}", join_names(.0))]
    MissingFields(Vec<String>),

    #[error("cannot access named field of non-struct type `{0}`")]
    NonStructFieldAccess(String),

    #[error("unknown field of pair type `{0}`, expected `first` or `rest`")]
    PairFieldAccess(String),

    #[error("unknown field of bytes type `{0}`, expected `length`")]
    BytesFieldAccess(String),

    #[error("cannot index non-list type `{0}`")]
    IndexAccess(String),

    #[error("index `{0}` out of bounds, length is `{1}`")]
    IndexOutOfBounds(u32, u32),

    #[error("the spread operator can only be used on the last element")]
    NonFinalSpread,

    #[error("cannot spread expression in non-vararg function call")]
    NonVarargSpread,

    #[error("cannot pass arguments directly (without spreading) to non-list vararg function call")]
    NonListVararg,

    #[error("duplicate enum variant `{0}`")]
    DuplicateEnumVariant(String),

    #[error("paths are not allowed in this context")]
    PathNotAllowed,

    #[error("cannot path into non-enum type `{0}`")]
    PathIntoNonEnum(String),

    #[error("unknown enum variant `{0}`")]
    UnknownEnumVariant(String),

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
