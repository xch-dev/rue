use crate::TokenKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Token {
    kind: TokenKind,
    len: usize,
}

impl Token {
    pub fn new(kind: TokenKind, len: usize) -> Self {
        Self { kind, len }
    }

    pub fn kind(&self) -> TokenKind {
        self.kind
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }
}
