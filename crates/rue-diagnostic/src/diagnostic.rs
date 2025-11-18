use std::path::Path;

use crate::{DiagnosticKind, LineCol, SrcLoc};

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub srcloc: SrcLoc,
    pub kind: DiagnosticKind,
}

impl Diagnostic {
    pub fn new(srcloc: SrcLoc, kind: DiagnosticKind) -> Self {
        Self { srcloc, kind }
    }

    pub fn start(&self) -> LineCol {
        self.srcloc.start()
    }

    pub fn end(&self) -> LineCol {
        self.srcloc.end()
    }

    pub fn message(&self, relative_to: &Path) -> String {
        let start = self.start();

        format!(
            "{} at {}:{}",
            self.kind,
            self.srcloc.source.kind.display(relative_to),
            start
        )
    }
}
