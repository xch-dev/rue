use thiserror::Error;

use crate::SyntaxKind;

#[derive(Debug, Error)]
pub enum ParserError {
    #[error("expected {expected}, found {found}")]
    UnexpectedToken {
        expected: SyntaxKind,
        found: SyntaxKind,
    },
}
