use expect_test::expect;

use tower_lsp::lsp_types::Url;

use super::TestProject;

#[test]
fn test_diagnostics_clean() {
    let project = TestProject::new(&[("main.rue", "fn main() -> Int {\n    42\n}\n")]);

    expect![[r#"
        main.rue: <none>
    "#]]
    .assert_eq(&project.change("main.rue", Some("fn main() -> Int {\n    42\n}\n")));
}

#[test]
fn test_diagnostics_update_on_change() {
    let project = TestProject::new(&[("main.rue", "fn main() -> Int {\n    42\n}\n")]);

    expect![[r#"
        main.rue:0:17-2:1 Error: Cannot assign `"hello"` to `Int` without a cast, since they are semantically different
    "#]].assert_eq(&project.change(
        "main.rue",
        Some("fn main() -> Int {\n    \"hello\"\n}\n"),
    ));

    expect![[r#"
        main.rue: <none>
    "#]]
    .assert_eq(&project.change("main.rue", Some("fn main() -> Int {\n    42\n}\n")));
}

#[test]
fn test_diagnostics_warning() {
    let project = TestProject::new(&[("main.rue", "fn main() -> Int {\n    42\n}\n")]);

    expect![[r#"
        main.rue:1:8-1:14 Warning: Unused binding `unused`
    "#]]
    .assert_eq(&project.change(
        "main.rue",
        Some("fn main() -> Int {\n    let unused = 1;\n    42\n}\n"),
    ));
}

#[test]
fn test_diagnostics_inline_if_may_raise() {
    let project = TestProject::new(&[("main.rue", "fn main() -> Int {\n    42\n}\n")]);

    expect![[r#"
        main.rue:1:4-4:4 Warning: A branch of this `inline if` can raise, but both branches are always evaluated regardless of the condition; use a normal `if` instead
    "#]].assert_eq(&project.change(
        "main.rue",
        Some("fn main(x: Int) -> Int {\n    inline if x > 5 {\n        raise;\n    }\n    x + 100\n}\n"),
    ));
}

#[test]
fn test_diagnostics_save_reverts_to_disk() {
    let project = TestProject::new(&[("main.rue", "fn main() -> Int {\n    42\n}\n")]);

    expect![[r#"
        main.rue:1:4-1:5 Error: Undeclared symbol `x`
    "#]]
    .assert_eq(&project.change("main.rue", Some("fn main() -> Int {\n    x\n}\n")));

    // Saving clears the in-memory contents, so the file on disk is used again.
    expect![[r#"
        main.rue: <none>
    "#]]
    .assert_eq(&project.change("main.rue", None));
}

#[test]
fn test_diagnostics_across_files() {
    let project = TestProject::new(&[
        (
            "main.rue",
            "import utils::greet;\n\nfn main() -> String {\n    greet(\"world\")\n}\n",
        ),
        (
            "utils.rue",
            "export fn greet(name: String) -> String {\n    \"Hello, \" + name\n}\n",
        ),
    ]);

    // Changing the signature in one file reports errors in the file that uses it.
    expect![[r#"
        main.rue:3:10-3:17 Error: Cannot assign `"world"` to `Int` without a cast, since they are semantically different
        utils.rue:0:16-0:20 Warning: Unused parameter `name`
    "#]].assert_eq(&project.change(
        "utils.rue",
        Some("export fn greet(name: Int) -> String {\n    \"Hello\"\n}\n"),
    ));
}

#[test]
fn test_diagnostics_outside_project() {
    let project = TestProject::with_manifest(None, &[("notes.txt", "not a rue file")]);

    expect![[""]].assert_eq(&project.change("notes.txt", Some("still not a rue file")));
}

#[test]
fn test_diagnostics_invalid_manifest() {
    let project = TestProject::with_manifest(
        Some("[compiler\n"),
        &[("main.rue", "fn main() -> Int {\n    42\n}\n")],
    );

    expect![[""]].assert_eq(&project.change("main.rue", Some("fn main() -> Int {\n    x\n}\n")));
}

#[test]
fn test_diagnostics_missing_entrypoint() {
    let project = TestProject::with_manifest(
        Some("[compiler]\nentrypoint = \"missing\"\n"),
        &[("main.rue", "fn main() -> Int {\n    42\n}\n")],
    );

    expect![[""]].assert_eq(&project.change("main.rue", Some("fn main() -> Int {\n    x\n}\n")));
}

#[test]
fn test_diagnostics_non_file_uri() {
    let project = TestProject::new(&[]);

    let diagnostics = project.backend().on_change_impl(
        &Url::parse("untitled:Untitled-1").unwrap(),
        Some("fn main() -> Int {\n    x\n}\n".to_string()),
    );

    assert!(diagnostics.is_empty());
}

#[test]
fn test_diagnostics_after_non_ascii() {
    let project = TestProject::new(&[("main.rue", "fn main() -> Int {\n    42\n}\n")]);

    expect![[r#"
        main.rue:1:18-1:19 Error: Undeclared symbol `x`
        main.rue:1:8-1:9 Warning: Unused binding `s`
    "#]]
    .assert_eq(&project.change(
        "main.rue",
        Some("fn main() -> Int {\n    let s = \"\u{1F600}\"; x\n}\n"),
    ));
}
