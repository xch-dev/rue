use std::{fmt::Write, path::Path};

use crate::{DiagnosticKind, DiagnosticSeverity, LineCol, Lint, Note, SrcLoc, Suggestion};

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub srcloc: SrcLoc,
    pub kind: DiagnosticKind,
    /// The severity this diagnostic is reported with. For lints, this comes from the configured
    /// lint level rather than from the kind of diagnostic.
    pub severity: DiagnosticSeverity,
    pub notes: Vec<Note>,
    pub suggestions: Vec<Suggestion>,
}

impl Diagnostic {
    pub fn new(srcloc: SrcLoc, kind: DiagnosticKind, severity: DiagnosticSeverity) -> Self {
        Self {
            srcloc,
            kind,
            severity,
            notes: Vec::new(),
            suggestions: Vec::new(),
        }
    }

    #[must_use]
    pub fn with_note(mut self, note: Note) -> Self {
        self.notes.push(note);
        self
    }

    #[must_use]
    pub fn with_suggestion(mut self, suggestion: Suggestion) -> Self {
        self.suggestions.push(suggestion);
        self
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

    /// The message, followed by a line for each note and suggestion, such as
    /// `note: Any code after this is never evaluated at main.rue:2:5`.
    pub fn full_message(&self, relative_to: &Path) -> String {
        let mut message = self.message(relative_to);

        for note in &self.notes {
            message.push_str("\n  note: ");
            message.push_str(&note.message);

            if let Some(srcloc) = &note.srcloc {
                write!(
                    message,
                    " at {}:{}",
                    srcloc.source.kind.display(relative_to),
                    srcloc.start()
                )
                .unwrap();
            }
        }

        for suggestion in &self.suggestions {
            message.push_str("\n  help: ");
            message.push_str(&suggestion.message);
        }

        message
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
