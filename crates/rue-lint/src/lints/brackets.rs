use rue_diagnostic::{Applicability, Edit, Lint, SrcLoc, Suggestion};

use crate::{LintContext, LintInput, text::comment_count};

pub(crate) const LINTS: &[Lint] = &[Lint::BracketedFor];

/// Reports the lists whose only item is a `for` that makes a list. Whether it does is only known
/// while the list is compiled, so these are recorded as sites then.
pub(crate) fn check(cx: &mut LintContext<'_>, input: &LintInput<'_>) {
    for site in &input.sites.bracketed_fors {
        let Some(diagnostic) = cx.report(
            Lint::BracketedFor,
            site.list.clone(),
            "A `for` expression is a list already, so the brackets around it make a list with one list in it",
        ) else {
            continue;
        };

        let source = &site.list.source;
        let text = &source.text;
        let open = site.list.span.start..site.for_expr.span.start;
        let close = site.for_expr.span.end..site.list.span.end;

        // Neither suggestion is applied automatically, since each changes the type of the value,
        // which may be what the code around it needs to change for
        if comment_count(&text[open.clone()]) == 0 && comment_count(&text[close.clone()]) == 0 {
            let (before, after) = if site.parenthesize {
                ("(", ")")
            } else if text[..open.start]
                .chars()
                .next_back()
                .is_some_and(|char| char.is_alphanumeric() || char == '_')
            {
                // The `for` would otherwise join the word before the bracket, as in `return[…]`
                (" ", "")
            } else {
                ("", "")
            };

            diagnostic.suggestions.push(Suggestion::new(
                "Remove the brackets",
                vec![
                    Edit::new(SrcLoc::new(source.clone(), open), before),
                    Edit::new(SrcLoc::new(source.clone(), close), after),
                ],
                Applicability::MaybeIncorrect,
            ));
        }

        let start = site.for_expr.span.start;

        diagnostic.suggestions.push(Suggestion::replace(
            "Spread the `for` into the list with `...`",
            SrcLoc::new(source.clone(), start..start),
            "...",
            Applicability::MaybeIncorrect,
        ));
    }
}
