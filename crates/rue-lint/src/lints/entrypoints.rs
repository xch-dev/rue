use rue_diagnostic::{Lint, Note};
use rue_hir::Symbol;

use crate::{LintContext, LintInput};

pub(crate) const LINTS: &[Lint] = &[Lint::MissingEntrypointReturnType];

/// Reports entrypoints (`main` and exported functions, which are compiled into programs) whose
/// return type accepts any value, so the type checker can't check what the program outputs.
/// Tests aren't programs, so they're left alone.
pub(crate) fn check(cx: &mut LintContext<'_>, input: &LintInput<'_>) {
    for &symbol in input.programs(cx.db()) {
        let Symbol::Function(function) = cx.db().symbol(symbol) else {
            continue;
        };

        let return_type = function.return_type;

        let Some(name) = function.name.clone() else {
            continue;
        };

        let Some(srcloc) = name.srcloc().cloned() else {
            continue;
        };

        if cx.contains_unresolved(return_type) {
            continue;
        }

        let any = cx.builtin_types().recursive_any;

        if !cx.is_assignable(any, return_type) {
            continue;
        }

        if let Some(diagnostic) = cx.report(
            Lint::MissingEntrypointReturnType,
            srcloc,
            format!(
                "Entrypoint `{}` can return any value, so what it outputs isn't checked",
                name.text()
            ),
        ) {
            diagnostic.notes.push(Note::new(
                "Declare the type that it returns instead, such as `List<Condition>`",
            ));
        }
    }
}
