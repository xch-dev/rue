use std::ops::Range;

use thiserror::Error;

use crate::SyntaxKind;

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub span: Range<usize>,
    pub kind: DiagnosticKind,
}

impl Diagnostic {
    pub fn new(span: Range<usize>, kind: DiagnosticKind) -> Self {
        Self { span, kind }
    }

    pub fn start(&self, source: &str) -> LineCol {
        line_col(source, self.span.start)
    }

    pub fn end(&self, source: &str) -> LineCol {
        line_col(source, self.span.end)
    }

    pub fn message(&self, source: &str) -> String {
        let start = self.start(source);

        format!("{} at {}:{}", self.kind, start.line + 1, start.col + 1,)
    }
}

#[derive(Debug, Clone, Error)]
pub enum DiagnosticKind {
    #[error("Unknown `{0}`")]
    UnknownToken(String),

    #[error("Expected {}, found {}", list_of(.1), .0)]
    UnexpectedToken(SyntaxKind, Vec<SyntaxKind>),

    #[error("Unterminated block comment")]
    UnterminatedBlockComment,

    #[error("Unterminated string literal")]
    UnterminatedString,

    #[error("Unterminated hex literal")]
    UnterminatedHex,

    #[error("Duplicate symbol `{0}` found in scope")]
    DuplicateSymbol(String),

    #[error("Duplicate type `{0}` found in scope")]
    DuplicateType(String),

    #[error("Undeclared symbol `{0}`")]
    UndeclaredSymbol(String),

    #[error("Undeclared type `{0}`")]
    UndeclaredType(String),

    #[error("Unnecessary empty generic parameter list specified")]
    EmptyGenericParameters,
}

impl DiagnosticKind {
    pub fn severity(&self) -> DiagnosticSeverity {
        match self {
            Self::UnknownToken(..)
            | Self::UnexpectedToken(..)
            | Self::UnterminatedBlockComment
            | Self::UnterminatedString
            | Self::UnterminatedHex
            | Self::DuplicateSymbol(..)
            | Self::DuplicateType(..)
            | Self::UndeclaredSymbol(..)
            | Self::UndeclaredType(..) => DiagnosticSeverity::Error,
            Self::EmptyGenericParameters => DiagnosticSeverity::Warning,
        }
    }
}

fn list_of(kinds: &[SyntaxKind]) -> String {
    match kinds.len() {
        0 => "nothing".to_string(),
        1 => format!("{}", kinds[0]),
        _ => format!(
            "one of {}",
            kinds
                .iter()
                .map(|k| k.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        ),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LineCol {
    pub line: usize,
    pub col: usize,
}

/// Returns the line and column of the given index in the source.
/// Line and column numbers are from 0.
pub fn line_col(source: &str, index: usize) -> LineCol {
    let mut line = 0;
    let mut col = 0;

    for (i, character) in source.chars().enumerate() {
        if i == index {
            break;
        }

        if character == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }

    LineCol { line, col }
}
