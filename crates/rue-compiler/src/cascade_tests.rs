//! A diagnostic that shows a type with a part that couldn't be compiled (`{unresolved}`) only
//! follows from the error that was reported for that part, so it's left out, as long as an error
//! that doesn't show one is left.

use rue_diagnostic::{Diagnostic, DiagnosticKind, DiagnosticSeverity, Lint, SrcLoc};

use crate::test_utils::compile;

/// The messages of the diagnostics that are left of the given ones, in order.
fn taken(diagnostics: Vec<(DiagnosticKind, DiagnosticSeverity)>) -> Vec<String> {
    let mut compiled = compile("");
    let source = compiled.ctx.source().clone();

    let diagnostics = diagnostics
        .into_iter()
        .enumerate()
        .map(|(index, (kind, severity))| {
            Diagnostic::new(SrcLoc::new(source.clone(), index..index), kind, severity)
        })
        .collect();

    compiled.ctx.extend_diagnostics(diagnostics);

    compiled
        .ctx
        .take_diagnostics()
        .into_iter()
        .map(|diagnostic| diagnostic.kind.to_string())
        .collect()
}

fn error(kind: DiagnosticKind) -> (DiagnosticKind, DiagnosticSeverity) {
    (kind, DiagnosticSeverity::Error)
}

fn lint(message: &str, severity: DiagnosticSeverity) -> (DiagnosticKind, DiagnosticSeverity) {
    let kind = DiagnosticKind::Lint {
        lint: Lint::UnusedBinding,
        message: message.to_string(),
    };

    (kind, severity)
}

fn incompatible(from: &str, to: &str) -> DiagnosticKind {
    DiagnosticKind::IncompatibleType(from.to_string(), to.to_string())
}

fn undeclared() -> DiagnosticKind {
    DiagnosticKind::UndeclaredSymbol("missing".to_string())
}

#[test]
fn error_that_shows_an_unresolved_type_follows_from_another_error() {
    assert_eq!(
        taken(vec![
            error(incompatible("List<{unresolved}>", "Int")),
            error(undeclared()),
            error(incompatible("Int", "({unresolved}, Bytes)")),
        ]),
        ["Undeclared symbol `missing`"]
    );
}

#[test]
fn lint_that_shows_an_unresolved_type_follows_from_an_error() {
    assert_eq!(
        taken(vec![
            lint(
                "Unused binding of `List<{unresolved}>`",
                DiagnosticSeverity::Warning
            ),
            lint("Unused binding `x`", DiagnosticSeverity::Warning),
            error(undeclared()),
        ]),
        ["Unused binding `x`", "Undeclared symbol `missing`"]
    );
}

/// Without an error that says why the type couldn't be compiled, the ones that show it are all
/// that say anything is wrong.
#[test]
fn errors_that_show_an_unresolved_type_are_kept_without_another_error() {
    let cascades = [
        "Cannot assign `List<{unresolved}>` to `Int`, since they are incompatible",
        "Cannot assign `Int` to `{unresolved}`, since they are incompatible",
    ];

    assert_eq!(
        taken(vec![
            error(incompatible("List<{unresolved}>", "Int")),
            error(incompatible("Int", "{unresolved}")),
        ]),
        cascades
    );

    // A warning isn't an error, and neither is a lint that's reported as one, which isn't why a
    // type couldn't be compiled
    assert_eq!(
        taken(vec![
            error(incompatible("List<{unresolved}>", "Int")),
            lint("Unused binding `x`", DiagnosticSeverity::Warning),
            error(incompatible("Int", "{unresolved}")),
            lint("Unused binding `y`", DiagnosticSeverity::Error),
        ]),
        [
            cascades[0],
            "Unused binding `x`",
            cascades[1],
            "Unused binding `y`"
        ]
    );
}

/// A string is written in quotes in a type, with the quotes in it escaped, so it's told apart
/// from a type that couldn't be compiled whatever it says.
#[test]
fn string_that_says_unresolved_is_shown() {
    let strings = [
        r#""{unresolved}""#,
        r#""a \" {unresolved}""#,
        r#""\\" | "{unresolved}""#,
    ];

    let mut diagnostics = vec![error(undeclared())];
    diagnostics.extend(strings.map(|string| error(incompatible(string, "Int"))));

    // But not a type that couldn't be compiled beside a string
    diagnostics.push(error(incompatible(r#""a" | {unresolved}"#, "Int")));
    diagnostics.push(error(incompatible(r#""\"" | {unresolved}"#, "Int")));

    let mut expected = vec!["Undeclared symbol `missing`".to_string()];
    expected.extend(
        strings.map(|string| {
            format!("Cannot assign `{string}` to `Int`, since they are incompatible")
        }),
    );

    assert_eq!(taken(diagnostics), expected);
}
