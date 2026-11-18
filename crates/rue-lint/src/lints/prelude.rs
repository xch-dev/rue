use std::collections::HashSet;

use rue_diagnostic::{Applicability, Lint, Note, Suggestion};

use crate::{LintContext, LintInput, srcloc_key};

pub(crate) const LINTS: &[Lint] = &[Lint::PreludeOverEnclosingItem];

/// Reports names in inline modules that refer to the prelude or a builtin, while a module that
/// encloses them has an item with the same name. An inline module doesn't see the items of the
/// modules it's in, so the name means something different from what it would mean outside of it.
pub(crate) fn check(cx: &mut LintContext<'_>, input: &LintInput<'_>) {
    // A path can be compiled more than once, so each name is only reported once
    let mut reported = HashSet::new();

    for reference in &input.sites.enclosed_prelude_references {
        let Some(srcloc) = reference.name.srcloc().cloned() else {
            continue;
        };

        if !reported.insert(srcloc_key(&srcloc)) {
            continue;
        }

        let name = reference.name.text();
        let path = format!("{}{name}", "super::".repeat(reference.depth));
        let kind = if reference.builtin {
            "builtin"
        } else {
            "prelude's item"
        };

        let Some(diagnostic) = cx.report(
            Lint::PreludeOverEnclosingItem,
            srcloc.clone(),
            format!(
                "`{name}` refers to the {kind}, not the enclosing module's `{path}`, since inline modules don't see the items of the modules they're in"
            ),
        ) else {
            continue;
        };

        if let Some(enclosing) = reference.enclosing.clone() {
            let verb = if reference.imported {
                "imported"
            } else {
                "declared"
            };

            diagnostic.notes.push(Note::at(
                enclosing,
                format!("The enclosing module's `{name}` is {verb} here"),
            ));
        }

        if !reference.builtin {
            diagnostic.notes.push(Note::new(format!(
                "To keep using the prelude's `{name}`, import it explicitly"
            )));
        }

        let replacement = if reference.field_shorthand {
            format!("{name}: {path}")
        } else {
            path.clone()
        };

        // The prelude's item might be the one that's meant
        diagnostic.suggestions.push(Suggestion::replace(
            format!("Use `{path}` to refer to the enclosing module's item"),
            srcloc,
            replacement,
            Applicability::MaybeIncorrect,
        ));
    }
}
