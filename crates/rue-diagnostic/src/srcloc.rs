use std::{ops::Range, path::Path, sync::Arc};

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SourceKind {
    Std,
    File(String),
}

impl SourceKind {
    pub fn check_unused(&self) -> bool {
        match self {
            Self::Std => false,
            Self::File(_) => true,
        }
    }

    pub fn display(&self, relative_to: &Path) -> String {
        match self {
            Self::Std => "std".to_string(),
            Self::File(path) => Path::new(path)
                .strip_prefix(relative_to)
                .map_or_else(|_| path.clone(), |path| path.to_string_lossy().to_string()),
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

    pub fn display(&self, relative_to: &Path) -> String {
        format!("{}:{}", self.source.kind.display(relative_to), self.start())
    }
}
