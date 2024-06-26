use std::{fmt, ops::Range};

use crate::SyntaxKind;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParserError {
    kind: ParserErrorKind,
    span: Range<usize>,
}

impl ParserError {
    pub fn new(kind: ParserErrorKind, span: Range<usize>) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &ParserErrorKind {
        &self.kind
    }

    pub fn span(&self) -> &Range<usize> {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ParserErrorKind {
    UnexpectedToken(Vec<SyntaxKind>, SyntaxKind),
    UnknownToken(String),
    UnterminatedString,
    UnterminatedBlockComment,
    MissingHexDigits,
}

impl fmt::Display for ParserErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let message = match self {
            ParserErrorKind::UnexpectedToken(expected, found) => {
                format!("Expected {}, found {}.", join_kinds(expected), found)
            }
            ParserErrorKind::UnknownToken(token) => format!("Unknown token `{token}`."),
            ParserErrorKind::UnterminatedString => "Unterminated string literal.".to_string(),
            ParserErrorKind::UnterminatedBlockComment => "Unterminated block comment.".to_string(),
            ParserErrorKind::MissingHexDigits => "Missing digits in hex literal.".to_string(),
        };
        write!(f, "{}", message.trim())
    }
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
