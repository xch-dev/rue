use std::path::Path;

use crate::{DiagnosticKind, DiagnosticSeverity, LineCol, Lint, SrcLoc};

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub srcloc: SrcLoc,
    pub kind: DiagnosticKind,
    /// The severity this diagnostic is reported with. For lints, this comes from the configured
    /// lint level rather than from the kind of diagnostic.
    pub severity: DiagnosticSeverity,
}

impl Diagnostic {
    pub fn new(srcloc: SrcLoc, kind: DiagnosticKind, severity: DiagnosticSeverity) -> Self {
        Self {
            srcloc,
            kind,
            severity,
        }
    }

    /// The lint this diagnostic belongs to, if any.
    pub fn lint(&self) -> Option<Lint> {
        self.kind.lint()
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
