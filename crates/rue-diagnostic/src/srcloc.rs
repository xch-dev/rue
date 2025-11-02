use std::{fmt, ops::Range, sync::Arc};

use derive_more::Display;

use crate::LineCol;

#[derive(Debug, Clone)]
pub struct Source {
    pub text: Arc<str>,
    pub kind: SourceKind,
}

impl Source {
    pub fn new(text: Arc<str>, kind: SourceKind) -> Self {
        Self { text, kind }
    }
}

#[derive(Debug, Clone, Display, PartialEq, Eq, Hash)]
pub enum SourceKind {
    #[display("std")]
    Std,

    #[display("{_0}")]
    File(String),
}

impl SourceKind {
    pub fn check_unused(&self) -> bool {
        match self {
            Self::Std => false,
            Self::File(_) => true,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SrcLoc {
    pub source: Source,
    pub span: Range<usize>,
}

impl SrcLoc {
    pub fn new(source: Source, span: Range<usize>) -> Self {
        Self { source, span }
    }

    pub fn start(&self) -> LineCol {
        LineCol::new(&self.source.text, self.span.start)
    }

    pub fn end(&self) -> LineCol {
        LineCol::new(&self.source.text, self.span.end)
    }
}

impl fmt::Display for SrcLoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.source.kind, self.start())
    }
}
