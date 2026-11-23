//! A hex literal assigned where an `Int` is expected gets a note that hex literals are bytes, and
//! a suggestion to write the number that was probably meant instead.

use rue_diagnostic::{Applicability, apply_edits};

use crate::test_utils::compile;

/// Applies the suggestions to write a number, which aren't applied automatically, and returns
/// the new source. Other suggestions, such as removing parentheses, are left out.
fn apply_suggestions(source: &str) -> String {
    let compiled = compile(source);

    let edits = compiled
        .diagnostics
        .iter()
        .flat_map(|diagnostic| &diagnostic.suggestions)
        .filter(|suggestion| suggestion.message.starts_with("Write the number"))
        .inspect(|suggestion| {
            assert_eq!(suggestion.applicability, Applicability::MaybeIncorrect);
        })
        .flat_map(|suggestion| &suggestion.edits)
        .collect::<Vec<_>>();

    apply_edits(source, &edits)
}

#[test]
fn suggestion_replaces_the_literal() {
    let source = "fn main() -> Int {\n    let a: Int = 0xff;\n    let b: Int = ( (0x00_7f) );\n    a + b\n}\n";

    assert_eq!(
        apply_suggestions(source),
        "fn main() -> Int {\n    let a: Int = 255;\n    let b: Int = ( (127) );\n    a + b\n}\n"
    );
}

#[test]
fn suggestion_replaces_an_argument() {
    let source =
        "fn takes(value: Int) -> Int {\n    value\n}\n\nfn main() -> Int {\n    takes(0x80)\n}\n";

    assert_eq!(
        apply_suggestions(source),
        "fn takes(value: Int) -> Int {\n    value\n}\n\nfn main() -> Int {\n    takes(128)\n}\n"
    );
}

/// Bytes that aren't a hex literal, such as a variable or a string, get no note.
#[test]
fn only_hex_literals_get_the_note() {
    let source = "fn main() -> Int {\n    let named = 0xff;\n    let a: Int = named;\n    let b: Int = \"x\";\n    a + b\n}\n";

    let compiled = compile(source);
    let unassignable = compiled
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.kind.to_string().contains("without a cast"))
        .collect::<Vec<_>>();

    assert_eq!(unassignable.len(), 2);
    assert!(
        unassignable
            .iter()
            .all(|diagnostic| diagnostic.notes.is_empty() && diagnostic.suggestions.is_empty())
    );
}

/// A function's body is checked against its return type as a whole, so a body that's only the
/// literal gets the suggestion too, and it replaces only the literal.
#[test]
fn suggestion_replaces_a_body() {
    let source = "fn main() -> Int {\n    0x80\n}\n";

    assert_eq!(
        apply_suggestions(source),
        "fn main() -> Int {\n    128\n}\n"
    );
}

/// Comments around the literal are kept, since only the literal is replaced.
#[test]
fn suggestion_keeps_comments() {
    let source = "fn main() -> Int {\n    let a: Int = (/* one */ 0xff // two\n    );\n    a\n}\n";

    assert_eq!(
        apply_suggestions(source),
        "fn main() -> Int {\n    let a: Int = (/* one */ 255 // two\n    );\n    a\n}\n"
    );
}
