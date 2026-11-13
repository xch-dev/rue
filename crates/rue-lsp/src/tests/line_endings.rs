//! Files with `\r\n` line endings, or a `\r` on its own, which the protocol also counts as a line
//! break. Positions in the snapshots are `line:character`, so they show where each line starts.

use expect_test::expect;
use tower_lsp::lsp_types::{Position, Range};

use super::TestProject;

const SOURCE: &str = "struct Point {\r\n    x: Int,\r\n    y: Int,\r\n}\r\n\r\nfn main(point: Point) -> Int {\r\n    let value = 42;\r\n    value + point.$0x\r\n}\r\n";

#[test]
fn test_crlf_queries() {
    let project = TestProject::new(&[("main.rue", SOURCE)]);

    expect![[r#"
        hover:
        x: Int
        definitions:
        main.rue:1:4-1:5
        references:
        main.rue:1:4-1:5
        main.rue:7:18-7:19
    "#]]
    .assert_eq(&project.queries());
}

#[test]
fn test_crlf_completion() {
    let project = TestProject::new(&[(
        "main.rue",
        "fn main() -> Int {\r\n    let value = 42;\r\n    val$0\r\n}\r\n",
    )]);

    expect![[r#"
        value [Variable] 42
    "#]]
    .assert_eq(&project.completions());
}

#[test]
fn test_crlf_diagnostics() {
    let project = TestProject::new(&[("main.rue", "fn main() -> Int {\r\n    42\r\n}\r\n")]);

    expect![[r#"
        Rue.toml: <none>
        main.rue:1:8-1:14 Warning[unused_binding]: Unused binding `unused`
        main.rue:2:4-2:11 Error: Undeclared symbol `missing`
    "#]]
    .assert_eq(&project.change(
        "main.rue",
        Some("fn main() -> Int {\r\n    let unused = 1;\r\n    missing\r\n}\r\n"),
    ));
}

#[test]
fn test_crlf_formatting() {
    let project = TestProject::new(&[("main.rue", "fn main()->Int{\r\n42\r\n}\r\n")]);

    let edits = project.formatting("main.rue").unwrap();
    assert_eq!(edits.len(), 1);
    assert_eq!(
        edits[0].range,
        Range::new(Position::new(0, 0), Position::new(3, 0))
    );
    expect![[r#"
        fn main() -> Int {
            42
        }
    "#]]
    .assert_eq(&edits[0].new_text);
}

#[test]
fn test_lone_carriage_return_queries() {
    let project = TestProject::new(&[(
        "main.rue",
        "fn main() -> Int {\r    let value = 42;\r    val$0ue\r}\r",
    )]);

    expect![[r#"
        hover:
        let value: 42
        definitions:
        main.rue:1:8-1:13
        references:
        main.rue:1:8-1:13
        main.rue:2:4-2:9
    "#]]
    .assert_eq(&project.queries());
}

#[test]
fn test_lone_carriage_return_diagnostics() {
    let project = TestProject::new(&[("main.rue", "fn main() -> Int {\r    42\r}\r")]);

    expect![[r#"
        Rue.toml: <none>
        main.rue:1:8-1:14 Warning[unused_binding]: Unused binding `unused`
    "#]]
    .assert_eq(&project.change(
        "main.rue",
        Some("fn main() -> Int {\r    let unused = 1;\r    42\r}\r"),
    ));
}

#[test]
fn test_trailing_lone_carriage_return_formatting() {
    // The whole document is replaced, including the `\r` at the end, which starts a new line.
    let project = TestProject::new(&[("main.rue", "fn main()->Int{\n42\n}\r")]);

    let edits = project.formatting("main.rue").unwrap();
    assert_eq!(edits.len(), 1);
    assert_eq!(
        edits[0].range,
        Range::new(Position::new(0, 0), Position::new(3, 0))
    );
    expect![[r#"
        fn main() -> Int {
            42
        }
    "#]]
    .assert_eq(&edits[0].new_text);
}
