use expect_test::expect;
use tower_lsp::lsp_types::{Position, Range};

use super::TestProject;
use crate::document_end;

#[test]
fn test_formatting_edits_whole_document() {
    let project = TestProject::new(&[("main.rue", "fn main()->Int{\n42\n}")]);

    let edits = project.formatting("main.rue").unwrap();
    assert_eq!(edits.len(), 1);
    assert_eq!(
        edits[0].range,
        Range::new(Position::new(0, 0), Position::new(2, 1))
    );
    expect![[r#"
        fn main() -> Int {
            42
        }
    "#]]
    .assert_eq(&edits[0].new_text);
}

#[test]
fn test_formatting_already_formatted() {
    let project = TestProject::new(&[("main.rue", "fn main() -> Int {\n    42\n}\n")]);

    assert_eq!(project.formatting("main.rue").unwrap(), Vec::new());
}

#[test]
fn test_formatting_uses_unsaved_contents() {
    let project = TestProject::new(&[("main.rue", "fn main() -> Int {\n    42\n}\n")]);

    project.change("main.rue", Some("fn main()->Int{\n1\n}\n"));

    let edits = project.formatting("main.rue").unwrap();
    assert_eq!(edits.len(), 1);
    expect![[r#"
        fn main() -> Int {
            1
        }
    "#]]
    .assert_eq(&edits[0].new_text);
}

#[test]
fn test_formatting_respects_manifest() {
    let project = TestProject::with_manifest(
        Some("[compiler]\nentrypoint = \".\"\nstd = false\n\n[formatter]\nindent_width = 2\n"),
        &[("main.rue", "fn main() -> Int {\n    42\n}\n")],
    );

    let edits = project.formatting("main.rue").unwrap();
    assert_eq!(edits.len(), 1);
    expect![[r#"
        fn main() -> Int {
          42
        }
    "#]]
    .assert_eq(&edits[0].new_text);
}

#[test]
fn test_formatting_syntax_error() {
    let project = TestProject::new(&[("main.rue", "fn main() -> Int {\n    42\n")]);

    expect!["source contains syntax errors"]
        .assert_eq(&project.formatting("main.rue").unwrap_err());
}

#[test]
fn test_document_end() {
    assert_eq!(document_end(""), Position::new(0, 0));
    assert_eq!(document_end("abc"), Position::new(0, 3));
    assert_eq!(document_end("abc\n"), Position::new(1, 0));
    assert_eq!(document_end("abc\ndef"), Position::new(1, 3));
    assert_eq!(document_end("abc\r\ndef\r\n"), Position::new(2, 0));
    assert_eq!(document_end("abc\r\nde"), Position::new(1, 2));
    // Characters are counted in UTF-16 code units.
    assert_eq!(document_end("a\n\u{1F600}é"), Position::new(1, 3));
}
