use std::ops::Range;

use thiserror::Error;

use crate::SyntaxKind;

#[derive(Debug)]
pub struct ParserError {
    kind: ParserErrorKind,
    range: Range<usize>,
}

impl ParserError {
    pub fn new(kind: ParserErrorKind, range: Range<usize>) -> Self {
        Self { kind, range }
    }

    pub fn kind(&self) -> &ParserErrorKind {
        &self.kind
    }

    pub fn range(&self) -> &Range<usize> {
        &self.range
    }
}

#[derive(Debug, Error)]
pub enum ParserErrorKind {
    #[error("expected {}, found {found}", join_kinds(.expected))]
    UnexpectedToken {
        expected: Vec<SyntaxKind>,
        found: SyntaxKind,
    },

    #[error("unknown token {0}")]
    UnknownToken(String),
}

/// Join a list of syntax kinds into a string, wrapped in backticks.
/// Prepend the result with "one of" if there is more than one kind.
fn join_kinds(kinds: &[SyntaxKind]) -> String {
    match kinds.len() {
        0 => "nothing".to_string(),
        1 => format!("{}", kinds[0]),
        _ => {
            let kinds: Vec<String> = kinds.iter().map(|kind| format!("{kind}")).collect();
            format!("one of {}", kinds.join(", "))
        }
    }
}
