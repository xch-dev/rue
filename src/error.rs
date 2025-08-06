use std::ops::Range;

use thiserror::Error;

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

    #[error("Unterminated block comment")]
    UnterminatedBlockComment,

    #[error("Unterminated string literal")]
    UnterminatedString,

    #[error("Unterminated hex literal")]
    UnterminatedBytes,
}
