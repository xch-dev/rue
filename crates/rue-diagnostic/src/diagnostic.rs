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
        LineCol::new(&self.srcloc.source.text, self.srcloc.span.start)
    }

    pub fn end(&self) -> LineCol {
        LineCol::new(&self.srcloc.source.text, self.srcloc.span.end)
    }

    pub fn message(&self) -> String {
        let start = self.start();

        format!(
            "{} at {}:{}:{}",
            self.kind,
            self.srcloc.source.kind,
            start.line + 1,
            start.col + 1
        )
    }
}
