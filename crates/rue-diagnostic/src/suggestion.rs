use crate::SrcLoc;

/// Extra information attached to a diagnostic, which can point to another location (such as the
/// `return` that makes the code after it unreachable).
#[derive(Debug, Clone)]
pub struct Note {
    pub message: String,
    pub srcloc: Option<SrcLoc>,
}

impl Note {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            srcloc: None,
        }
    }

    pub fn at(srcloc: SrcLoc, message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            srcloc: Some(srcloc),
        }
    }
}

/// A change to the source code that fixes a diagnostic, such as removing an unnecessary operator.
#[derive(Debug, Clone)]
pub struct Suggestion {
    /// What the change does, such as "Remove the `+`".
    pub message: String,
    pub edits: Vec<Edit>,
    pub applicability: Applicability,
}

impl Suggestion {
    pub fn new(message: impl Into<String>, edits: Vec<Edit>, applicability: Applicability) -> Self {
        Self {
            message: message.into(),
            edits,
            applicability,
        }
    }

    /// A suggestion that replaces a single location.
    pub fn replace(
        message: impl Into<String>,
        srcloc: SrcLoc,
        replacement: impl Into<String>,
        applicability: Applicability,
    ) -> Self {
        Self::new(message, vec![Edit::new(srcloc, replacement)], applicability)
    }
}

/// A replacement of the text at a location.
#[derive(Debug, Clone)]
pub struct Edit {
    pub srcloc: SrcLoc,
    pub replacement: String,
}

impl Edit {
    pub fn new(srcloc: SrcLoc, replacement: impl Into<String>) -> Self {
        Self {
            srcloc,
            replacement: replacement.into(),
        }
    }
}

/// How confident a suggestion is, which decides whether it can be applied automatically.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Applicability {
    /// The suggestion is definitely what the code means, so it can be applied automatically.
    MachineApplicable,
    /// The suggestion is probably right, but should be checked by a person first (for example,
    /// because it removes code).
    MaybeIncorrect,
}

/// Applies the edits to the text of a source, which must all be for that source. Edits are
/// applied from the end of the text, so the earlier ones' locations stay valid. Edits that overlap
/// a later one are skipped.
pub fn apply_edits(text: &str, edits: &[&Edit]) -> String {
    let mut edits = edits.to_vec();
    edits.sort_by_key(|edit| (edit.srcloc.span.start, edit.srcloc.span.end));

    let mut result = text.to_string();
    let mut limit = text.len();

    for edit in edits.into_iter().rev() {
        if edit.srcloc.span.end > limit {
            continue;
        }

        result.replace_range(edit.srcloc.span.clone(), &edit.replacement);
        limit = edit.srcloc.span.start;
    }

    result
}
