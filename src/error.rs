use std::ops::Range;

use thiserror::Error;

use crate::SyntaxKind;

#[derive(Debug, Clone)]
pub struct Error {
    pub span: Range<usize>,
    pub kind: ErrorKind,
}

impl Error {
    pub fn new(span: Range<usize>, kind: ErrorKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, Clone, Error)]
pub enum ErrorKind {
    #[error("Unknown token `{0}`")]
    UnknownToken(String),

    #[error("Unexpected token {}, expected {}", .0, list_of(.1))]
    UnexpectedToken(SyntaxKind, Vec<SyntaxKind>),

    #[error("Unterminated block comment")]
    UnterminatedBlockComment,

    #[error("Unterminated string literal")]
    UnterminatedString,

    #[error("Unterminated hex literal")]
    UnterminatedHex,
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
