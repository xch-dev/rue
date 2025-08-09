use std::ops::Range;

use crate::TokenKind;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub span: Range<usize>,
    pub kind: TokenKind,
}

impl Token {
    pub fn new(span: Range<usize>, kind: TokenKind) -> Self {
        Self { span, kind }
    }
}
