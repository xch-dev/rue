use std::{
    ops::Range,
    path::Path,
    sync::{Arc, OnceLock},
};

use crate::{LineCol, LineIndex};

#[derive(Debug, Clone)]
pub struct Source {
    /// The text of the file. The line indexes are built from it the first time they're needed, so
    /// it shouldn't be replaced afterwards; create a new `Source` instead.
    pub text: Arc<str>,
    pub kind: SourceKind,
    /// Shared between clones, so that each is only built once per file.
    line_index: Arc<OnceLock<LineIndex>>,
    line_index_with_carriage_returns: Arc<OnceLock<LineIndex>>,
}

impl Source {
    pub fn new(text: Arc<str>, kind: SourceKind) -> Self {
        Self {
            text,
            kind,
            line_index: Arc::default(),
            line_index_with_carriage_returns: Arc::default(),
        }
    }

    /// The lines of the text, as [`LineIndex::new`] counts them.
    pub fn line_index(&self) -> &LineIndex {
        self.line_index.get_or_init(|| LineIndex::new(&self.text))
    }

    /// The lines of the text, as [`LineIndex::with_carriage_returns`] counts them.
    pub fn line_index_with_carriage_returns(&self) -> &LineIndex {
        self.line_index_with_carriage_returns
            .get_or_init(|| LineIndex::with_carriage_returns(&self.text))
    }

    /// The line and column of the byte offset, as [`LineCol::new`] finds it.
    pub fn line_col(&self, offset: usize) -> LineCol {
        self.line_index().line_col(&self.text, offset)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SourceKind {
    Std(String),
    File(String),
}

impl SourceKind {
    pub fn check_unused(&self) -> bool {
        match self {
            Self::Std(_) => false,
            Self::File(_) => true,
        }
    }

    pub fn display(&self, relative_to: &Path) -> String {
        match self {
            Self::Std(path) => Path::new("std").join(path).to_string_lossy().to_string(),
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

    /// The source text at this location.
    pub fn text(&self) -> &str {
        &self.source.text[self.span.clone()]
    }

    pub fn start(&self) -> LineCol {
        self.source.line_col(self.span.start)
    }

    pub fn end(&self) -> LineCol {
        self.source.line_col(self.span.end)
    }

    pub fn display(&self, relative_to: &Path) -> String {
        format!("{}:{}", self.source.kind.display(relative_to), self.start())
    }
}
