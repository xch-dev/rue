use rue_diagnostic::{Applicability, DiagnosticSeverity, Source, SrcLoc};
use wasm_bindgen::prelude::*;

/// A diagnostic in the playground's file.
#[derive(Debug, Clone)]
#[wasm_bindgen(getter_with_clone)]
pub struct Diagnostic {
    /// Either `error` or `warning`.
    pub severity: String,
    pub message: String,
    /// The name of the lint that reported it, if any.
    pub lint: Option<String>,
    /// Where it starts, as a UTF-16 offset.
    pub from: u32,
    /// Where it ends, as a UTF-16 offset.
    pub to: u32,
    /// The line it starts on, from 1.
    pub line: u32,
    /// The column it starts at, from 1, in characters.
    pub column: u32,
    /// Notes, each with its location if it has one, such as
    /// `Any code after this is never evaluated (2:5)`.
    pub notes: Vec<String>,
    pub suggestions: Vec<Suggestion>,
}

/// A change that fixes a diagnostic.
#[derive(Debug, Clone)]
#[wasm_bindgen(getter_with_clone)]
pub struct Suggestion {
    pub message: String,
    pub edits: Vec<TextEdit>,
    /// Whether the suggestion is definitely what the code means, rather than only probably.
    #[wasm_bindgen(js_name = machineApplicable)]
    pub machine_applicable: bool,
}

/// A replacement of a range of the source, in UTF-16 offsets.
#[derive(Debug, Clone)]
#[wasm_bindgen(getter_with_clone)]
pub struct TextEdit {
    pub from: u32,
    pub to: u32,
    pub insert: String,
}

/// Converts the compiler's diagnostics in the source. Diagnostics in other sources (such as the
/// standard library) are placed at the start of the file, with their location in the message.
pub(crate) fn convert_diagnostics(
    source: &Source,
    diagnostics: &[rue_diagnostic::Diagnostic],
) -> Vec<Diagnostic> {
    diagnostics
        .iter()
        .map(|diagnostic| {
            let in_source = is_in(source, &diagnostic.srcloc);

            let (from, to) = if in_source {
                (
                    utf16_offset(&source.text, diagnostic.srcloc.span.start),
                    utf16_offset(&source.text, diagnostic.srcloc.span.end),
                )
            } else {
                (0, 0)
            };

            let start = diagnostic.start();

            let message = if in_source {
                diagnostic.kind.to_string()
            } else {
                format!(
                    "{} (in {})",
                    diagnostic.kind,
                    diagnostic.srcloc.display(".".as_ref())
                )
            };

            let notes = diagnostic
                .notes
                .iter()
                .map(|note| match &note.srcloc {
                    Some(srcloc) if is_in(source, srcloc) => {
                        format!("{} ({})", note.message, srcloc.start())
                    }
                    Some(srcloc) => {
                        format!("{} ({})", note.message, srcloc.display(".".as_ref()))
                    }
                    None => note.message.clone(),
                })
                .collect();

            // Suggestions that change other sources can't be applied in the playground
            let suggestions = diagnostic
                .suggestions
                .iter()
                .filter(|suggestion| {
                    suggestion
                        .edits
                        .iter()
                        .all(|edit| is_in(source, &edit.srcloc))
                })
                .map(|suggestion| Suggestion {
                    message: suggestion.message.clone(),
                    edits: suggestion
                        .edits
                        .iter()
                        .map(|edit| TextEdit {
                            from: utf16_offset(&source.text, edit.srcloc.span.start),
                            to: utf16_offset(&source.text, edit.srcloc.span.end),
                            insert: edit.replacement.clone(),
                        })
                        .collect(),
                    machine_applicable: suggestion.applicability
                        == Applicability::MachineApplicable,
                })
                .collect();

            Diagnostic {
                severity: match diagnostic.severity {
                    DiagnosticSeverity::Error => "error",
                    DiagnosticSeverity::Warning => "warning",
                }
                .to_string(),
                message,
                lint: diagnostic.lint().map(|lint| lint.name().to_string()),
                from,
                to,
                line: to_u32(if in_source { start.line + 1 } else { 1 }),
                column: to_u32(if in_source { start.col + 1 } else { 1 }),
                notes,
                suggestions,
            }
        })
        .collect()
}

fn is_in(source: &Source, srcloc: &SrcLoc) -> bool {
    srcloc.source.kind == source.kind
}

/// The number of UTF-16 code units before the byte offset. An offset inside a character counts
/// from the start of that character.
pub(crate) fn utf16_offset(text: &str, offset: usize) -> u32 {
    let mut offset = offset.min(text.len());

    while !text.is_char_boundary(offset) {
        offset -= 1;
    }

    to_u32(text[..offset].encode_utf16().count())
}

fn to_u32(value: usize) -> u32 {
    u32::try_from(value).unwrap_or(u32::MAX)
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use rue_diagnostic::{DiagnosticKind, Edit, Note, SourceKind};

    use super::*;

    /// Diagnostics, notes and suggestions in other sources (such as the standard library) can't
    /// point into the playground's file.
    #[test]
    fn other_sources() {
        let main = Source::new(
            Arc::from("fn main() {}\n"),
            SourceKind::File("main.rue".to_string()),
        );
        let std = Source::new(
            Arc::from("a\nbc\n"),
            SourceKind::Std("prelude.rue".to_string()),
        );

        let mut diagnostic = rue_diagnostic::Diagnostic::new(
            SrcLoc::new(std.clone(), 2..4),
            DiagnosticKind::UnterminatedString,
            DiagnosticSeverity::Error,
        );
        diagnostic
            .notes
            .push(Note::at(SrcLoc::new(std.clone(), 0..1), "In std"));
        diagnostic
            .notes
            .push(Note::at(SrcLoc::new(main.clone(), 3..7), "In main"));
        diagnostic.suggestions.push(rue_diagnostic::Suggestion::new(
            "Edit std",
            vec![Edit::new(SrcLoc::new(std.clone(), 0..1), "x")],
            Applicability::MachineApplicable,
        ));
        diagnostic.suggestions.push(rue_diagnostic::Suggestion::new(
            "Edit main",
            vec![Edit::new(SrcLoc::new(main.clone(), 3..7), "start")],
            Applicability::MaybeIncorrect,
        ));

        let [converted] = convert_diagnostics(&main, &[diagnostic])
            .try_into()
            .unwrap();

        assert_eq!(converted.severity, "error");
        assert_eq!((converted.from, converted.to), (0, 0));
        assert_eq!((converted.line, converted.column), (1, 1));
        assert_eq!(
            converted.message,
            "Unterminated string literal (in std/prelude.rue:2:1)"
        );
        assert_eq!(
            converted.notes,
            ["In std (std/prelude.rue:1:1)", "In main (1:4)"]
        );

        let [suggestion] = converted.suggestions.try_into().unwrap();
        assert_eq!(suggestion.message, "Edit main");
        assert!(!suggestion.machine_applicable);
        assert_eq!((suggestion.edits[0].from, suggestion.edits[0].to), (3, 7));
    }

    #[test]
    fn utf16_offsets() {
        let text = "a\u{e9}\u{1F600}b";

        assert_eq!(utf16_offset(text, 0), 0);
        assert_eq!(utf16_offset(text, 1), 1);
        // `é` is two bytes but one UTF-16 code unit
        assert_eq!(utf16_offset(text, 3), 2);
        // The emoji is four bytes and two UTF-16 code units
        assert_eq!(utf16_offset(text, 7), 4);
        assert_eq!(utf16_offset(text, 8), 5);
        // Inside a character, and past the end
        assert_eq!(utf16_offset(text, 5), 2);
        assert_eq!(utf16_offset(text, 100), 5);
    }
}
