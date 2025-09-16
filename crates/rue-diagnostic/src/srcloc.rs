use std::{ops::Range, sync::Arc};

use derive_more::Display;

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

#[derive(Debug, Clone, Display)]
pub enum SourceKind {
    #[display("std")]
    Std,

    #[display("{_0}")]
    File(String),
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
}
