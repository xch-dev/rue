use std::ops::Range;

use thiserror::Error;

#[derive(Debug)]
pub struct Diagnostic {
    kind: DiagnosticKind,
    info: DiagnosticInfo,
    span: Range<usize>,
}

impl Diagnostic {
    pub fn new(kind: DiagnosticKind, info: DiagnosticInfo, span: Range<usize>) -> Self {
        Self { kind, info, span }
    }

    pub fn kind(&self) -> DiagnosticKind {
        self.kind
    }

    pub fn info(&self) -> &DiagnosticInfo {
        &self.info
    }

    pub fn span(&self) -> &Range<usize> {
        &self.span
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DiagnosticKind {
    Warning,
    Error,
}

#[derive(Debug, Error, Clone, PartialEq, Eq, Hash)]
pub enum DiagnosticInfo {
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

    #[error("cannot access field of type `{0}`")]
    FieldAccess(String),

    #[error("cannot index type `{0}`")]
    IndexAccess(String),

    #[error("index `{0}` out of bounds, length is `{1}`")]
    IndexOutOfBounds(u32, u32),
}

/// Join a list of names into a string, wrapped in backticks.
fn join_names(kinds: &[String]) -> String {
    let names: Vec<String> = kinds.iter().map(|kind| format!("`{kind}`")).collect();
    names.join(", ")
}
