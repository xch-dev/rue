use rue_diagnostic::{Applicability, Lint, SrcLoc, Suggestion};
use rue_lexer::{Lexer, TokenKind};

use crate::{LintContext, LintInput, text::comment_count};

pub(crate) const LINTS: &[Lint] = &[Lint::UnusedStructSpread];

/// Reports the values spread in struct initializers that nothing is read from, since every field
/// is either written or a constant. Which fields a spread provides is decided while compiling, so
/// these are recorded as sites then.
pub(crate) fn check(cx: &mut LintContext<'_>, input: &LintInput<'_>) {
    for site in &input.sites.unused_struct_spreads {
        let message = if site.constants {
            format!(
                "Every other field of `{}` is a constant, so nothing is read from the spread value",
                site.struct_name
            )
        } else {
            format!(
                "Every field of `{}` is specified, so nothing is copied from the spread value",
                site.struct_name
            )
        };

        let Some(diagnostic) = cx.report(Lint::UnusedStructSpread, site.srcloc.clone(), message)
        else {
            continue;
        };

        // A field without a default value would be missing without the spread
        if !site.removable {
            continue;
        }

        let removed = removed_range(&site.srcloc);

        // A value that isn't a path is still evaluated, in case it fails, and a comment in the
        // spread would be removed with it
        let applicability = if site.evaluated || comment_count(removed.text()) > 0 {
            Applicability::MaybeIncorrect
        } else {
            Applicability::MachineApplicable
        };

        diagnostic.suggestions.push(Suggestion::replace(
            "Remove the spread",
            removed,
            "",
            applicability,
        ));
    }
}

/// The spread, along with the comma after it and the whitespace up to the first field, so that
/// removing it leaves the fields where the spread was. A comment before the comma is removed with
/// it.
fn removed_range(srcloc: &SrcLoc) -> SrcLoc {
    let text = &srcloc.source.text;
    let mut end = srcloc.span.end;
    let mut after_comma = false;

    for token in Lexer::new(&text[srcloc.span.end..]) {
        match token.kind {
            TokenKind::Whitespace => {}
            TokenKind::LineComment | TokenKind::BlockComment { .. } if !after_comma => {}
            TokenKind::Comma if !after_comma => after_comma = true,
            _ => {
                end = srcloc.span.end + token.span.start;
                break;
            }
        }
    }

    SrcLoc::new(srcloc.source.clone(), srcloc.span.start..end)
}
