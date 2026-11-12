use std::fmt::Write;
use std::sync::Arc;

use expect_test::expect;

use rue_diagnostic::{
    Applicability, DiagnosticKind, Edit, Lint, Source, SourceKind, SrcLoc, Suggestion,
};
use tower_lsp::LanguageServer;
use tower_lsp::lsp_types::{
    CodeActionContext, CodeActionOrCommand, CodeActionParams, InitializeParams,
    PartialResultParams, Range, TextDocumentIdentifier, Url, WorkDoneProgressParams,
};

use super::TestProject;
use crate::{code_actions, position::position_to_offset};

#[test]
fn test_diagnostics_clean() {
    let project = TestProject::new(&[("main.rue", "fn main() -> Int {\n    42\n}\n")]);

    expect![[r#"
        Rue.toml: <none>
        main.rue: <none>
    "#]]
    .assert_eq(&project.change("main.rue", Some("fn main() -> Int {\n    42\n}\n")));
}

#[test]
fn test_diagnostics_update_on_change() {
    let project = TestProject::new(&[("main.rue", "fn main() -> Int {\n    42\n}\n")]);

    expect![[r#"
        Rue.toml: <none>
        main.rue:0:17-2:1 Error: Cannot assign `"hello"` to `Int` without a cast, since they are semantically different
    "#]].assert_eq(&project.change(
        "main.rue",
        Some("fn main() -> Int {\n    \"hello\"\n}\n"),
    ));

    expect![[r#"
        Rue.toml: <none>
        main.rue: <none>
    "#]]
    .assert_eq(&project.change("main.rue", Some("fn main() -> Int {\n    42\n}\n")));
}

#[test]
fn test_diagnostics_warning() {
    let project = TestProject::new(&[("main.rue", "fn main() -> Int {\n    42\n}\n")]);

    expect![[r#"
        Rue.toml: <none>
        main.rue:1:8-1:14 Warning[unused_binding]: Unused binding `unused`
    "#]]
    .assert_eq(&project.change(
        "main.rue",
        Some("fn main() -> Int {\n    let unused = 1;\n    42\n}\n"),
    ));
}

#[test]
fn test_diagnostics_manifest_lints() {
    let project = TestProject::with_manifest(
        Some(
            "[compiler]\nentrypoint = \".\"\nstd = false\n\n[lints]\nunused_binding = \"deny\"\ndouble_negation = \"allow\"\n",
        ),
        &[("main.rue", "fn main() -> Int {\n    42\n}\n")],
    );

    expect![[r#"
        Rue.toml: <none>
        main.rue:1:8-1:14 Error[unused_binding]: Unused binding `unused`
    "#]]
    .assert_eq(&project.change(
        "main.rue",
        Some("fn main() -> Int {\n    let unused = 1;\n    --42\n}\n"),
    ));
}

#[test]
fn test_diagnostics_manifest_unknown_lints() {
    let project = TestProject::with_manifest(
        Some(
            "[compiler]\nentrypoint = \".\"\nstd = false\n\n[lints]\nunused_bindings = \"deny\"\n\"typo\" = \"allow\"\n",
        ),
        &[("main.rue", "fn main() -> Int {\n    42\n}\n")],
    );

    expect![[r#"
        Rue.toml:6:0-6:6 Warning[unknown_lint]: Unknown lint `typo`
        Rue.toml:5:0-5:15 Warning[unknown_lint]: Unknown lint `unused_bindings`
        main.rue: <none>
    "#]]
    .assert_eq(&project.change("main.rue", None));
}

#[test]
fn test_diagnostics_manifest_unknown_lint_levels() {
    let project = TestProject::with_manifest(
        Some(
            "[compiler]\nentrypoint = \".\"\nstd = false\n\n[lints]\nunknown_lint = \"deny\"\ntypo = \"allow\"\n",
        ),
        &[("main.rue", "fn main() -> Int {\n    42\n}\n")],
    );

    expect![[r#"
        Rue.toml:6:0-6:4 Error[unknown_lint]: Unknown lint `typo`
        main.rue: <none>
    "#]]
    .assert_eq(&project.change("main.rue", None));

    let project = TestProject::with_manifest(
        Some(
            "[compiler]\nentrypoint = \".\"\nstd = false\n\n[lints]\nunknown_lint = \"allow\"\ntypo = \"allow\"\n",
        ),
        &[("main.rue", "fn main() -> Int {\n    42\n}\n")],
    );

    expect![[r#"
        Rue.toml: <none>
        main.rue: <none>
    "#]]
    .assert_eq(&project.change("main.rue", None));
}

#[test]
fn test_diagnostics_manifest_forbidden_groups() {
    let project = TestProject::with_manifest(
        Some(
            "[compiler]\nentrypoint = \".\"\nstd = false\n\n[lints]\nunused = \"forbid\"\nunused_binding = \"allow\"\n",
        ),
        &[(
            "main.rue",
            "fn main() -> Int {\n    let unused = 1;\n    42\n}\n",
        )],
    );

    expect![[r#"
        Rue.toml:6:0-6:14 Error: Cannot set lint `unused_binding` to `allow`, since it's forbidden by `Rue.toml`
        main.rue:1:8-1:14 Error[unused_binding]: Unused binding `unused`
    "#]]
    .assert_eq(&project.change("main.rue", None));
}

#[test]
fn test_diagnostics_manifest_unknown_lints_in_other_layouts() {
    let project = TestProject::with_manifest(
        Some(
            "lints.dotted_typo = \"allow\"\n\n[compiler] # options\nentrypoint = \".\"\nstd = false\n",
        ),
        &[("main.rue", "fn main() -> Int {\n    42\n}\n")],
    );

    expect![[r#"
        Rue.toml:0:6-0:17 Warning[unknown_lint]: Unknown lint `dotted_typo`
        main.rue: <none>
    "#]]
    .assert_eq(&project.change("main.rue", None));

    let project = TestProject::with_manifest(
        Some(
            "lints = { inline_typo = \"allow\" }\n\n[compiler]\nentrypoint = \".\"\nstd = false\n",
        ),
        &[("main.rue", "fn main() -> Int {\n    42\n}\n")],
    );

    expect![[r#"
        Rue.toml:0:10-0:21 Warning[unknown_lint]: Unknown lint `inline_typo`
        main.rue: <none>
    "#]]
    .assert_eq(&project.change("main.rue", None));
}

#[test]
fn test_diagnostics_manifest_invalid_lint_level() {
    let project = TestProject::with_manifest(
        Some(
            "[compiler]\nentrypoint = \".\"\nstd = false\n\n[lints] # project lints\nunused_binding = \"expect\"\n",
        ),
        &[("main.rue", "fn main() -> Int {\n    42\n}\n")],
    );

    expect![[r#"
        Rue.toml:5:17-5:25 Error: Unknown lint level `expect`, expected `allow`, `warn`, `deny` or `forbid`
    "#]]
    .assert_eq(&project.change("main.rue", None));
}

#[test]
fn test_diagnostics_inline_if_may_raise() {
    let project = TestProject::new(&[("main.rue", "fn main() -> Int {\n    42\n}\n")]);

    expect![[r#"
        Rue.toml: <none>
        main.rue:1:4-3:5 Warning[inline_if_may_raise]: A branch of this `inline if` can raise, but both branches are always evaluated regardless of the condition; use a normal `if` instead
    "#]].assert_eq(&project.change(
        "main.rue",
        Some("fn main(x: Int) -> Int {\n    inline if x > 5 {\n        raise;\n    }\n    x + 100\n}\n"),
    ));
}

#[test]
fn test_diagnostics_save_reverts_to_disk() {
    let project = TestProject::new(&[("main.rue", "fn main() -> Int {\n    42\n}\n")]);

    expect![[r#"
        Rue.toml: <none>
        main.rue:1:4-1:5 Error: Undeclared symbol `x`
    "#]]
    .assert_eq(&project.change("main.rue", Some("fn main() -> Int {\n    x\n}\n")));

    // Saving clears the in-memory contents, so the file on disk is used again.
    expect![[r#"
        Rue.toml: <none>
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
        Rue.toml: <none>
        main.rue:3:10-3:17 Error: Cannot assign `"world"` to `Int` without a cast, since they are semantically different
        utils.rue:0:16-0:20 Warning[unused_parameter]: Unused parameter `name`
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

    expect![[r#"
        Rue.toml:0:9-0:9 Error: unclosed table, expected `]`
    "#]]
    .assert_eq(&project.change("main.rue", Some("fn main() -> Int {\n    x\n}\n")));
}

#[test]
fn test_diagnostics_while_manifest_is_invalid() {
    let project = TestProject::new(&[("main.rue", "fn main() -> Int {\n    42\n}\n")]);

    expect![[r#"
        Rue.toml: <none>
        main.rue:1:4-1:5 Error: Undeclared symbol `x`
    "#]]
    .assert_eq(&project.change("main.rue", Some("fn main() -> Int {\n    x\n}\n")));

    // The file's diagnostics are out of date while the manifest is invalid, so they're cleared
    std::fs::write(project.path("Rue.toml"), "[compiler\n").unwrap();

    expect![[r#"
        Rue.toml:0:9-0:9 Error: unclosed table, expected `]`
        main.rue: <none>
    "#]]
    .assert_eq(&project.change("main.rue", Some("fn main() -> Int {\n    y\n}\n")));

    // The text from while the manifest was invalid is used once it's fixed
    std::fs::write(
        project.path("Rue.toml"),
        "[compiler]\nentrypoint = \".\"\nstd = false\n",
    )
    .unwrap();

    expect![[r#"
        Rue.toml: <none>
        main.rue:1:4-1:5 Error: Undeclared symbol `y`
    "#]]
    .assert_eq(&project.change("Rue.toml", None));
}

#[test]
fn test_diagnostics_missing_entrypoint() {
    let project = TestProject::with_manifest(
        Some("[compiler]\nentrypoint = \"missing\"\n"),
        &[("main.rue", "fn main() -> Int {\n    42\n}\n")],
    );

    // The project can't be compiled, so only the manifest's diagnostics are reported
    expect![[r#"
        Rue.toml: <none>
    "#]]
    .assert_eq(&project.change("main.rue", Some("fn main() -> Int {\n    x\n}\n")));
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
        Rue.toml: <none>
        main.rue:1:8-1:9 Warning[unused_binding]: Unused binding `s`
        main.rue:1:18-1:19 Error: Undeclared symbol `x`
    "#]]
    .assert_eq(&project.change(
        "main.rue",
        Some("fn main() -> Int {\n    let s = \"\u{1F600}\"; x\n}\n"),
    ));
}

#[test]
fn test_code_actions_apply_suggestions() {
    let project = TestProject::new(&[("main.rue", "fn main() -> Int {\n    42\n}\n")]);

    expect![[r#"
        Remove the `+` (preferred: true):
        fn main() -> Int {
            let value = 42;
            return value;
        }

        Replace the `return` statement with its value (preferred: true):
        fn main() -> Int {
            let value = +42;
            value
        }

    "#]]
    .assert_eq(&project.code_actions(
        "main.rue",
        "fn main() -> Int {\n    let value = +42;\n    return value;\n}\n",
    ));
}

#[test]
fn test_diagnostics_unreachable_code() {
    let project = TestProject::new(&[("main.rue", "fn main() -> Int {\n    42\n}\n")]);

    let text =
        "fn main() -> Int {\n    if 1 > 2 {\n        raise;\n        debug 1;\n    }\n    42\n}\n";

    expect![[r#"
        Rue.toml: <none>
        main.rue:3:8-3:16 Warning[unreachable_code]: Unreachable code
            main.rue:2:8-2:14: Any code after this is never evaluated
    "#]]
    .assert_eq(&project.change("main.rue", Some(text)));

    expect![[r#"
        Remove the unreachable code (preferred: false):
        fn main() -> Int {
            if 1 > 2 {
                raise;
            }
            42
        }

    "#]]
    .assert_eq(&project.code_actions("main.rue", text));
}

#[test]
fn test_diagnostics_notes_without_related_information() {
    let project = TestProject::new(&[("main.rue", "fn main() -> Int {\n    42\n}\n")]);

    // Clients that don't say they support related information get the notes in the message
    futures::executor::block_on(project.backend().initialize(InitializeParams::default())).unwrap();

    let text =
        "fn main() -> Int {\n    if 1 > 2 {\n        raise;\n        debug 1;\n    }\n    42\n}\n";

    expect![[r#"
        Rue.toml: <none>
        main.rue:3:8-3:16 Warning[unreachable_code]: Unreachable code
        Any code after this is never evaluated at main.rue:3:9
    "#]]
    .assert_eq(&project.change("main.rue", Some(text)));
}

#[test]
fn test_diagnostics_unnamed_parameters() {
    // Parameters without names used to collapse into one, which panicked when compiling the body
    let project = TestProject::new(&[("main.rue", "fn main() -> Int {\n    42\n}\n")]);

    expect![[r#"
        Rue.toml: <none>
        main.rue:4:3-4:4 Warning[unused_function]: Unused function `f`
        main.rue:4:6-4:9 Warning[unused_parameter]: Unused parameter `Int`
        main.rue:4:9-4:10 Error: Expected `:`, found `,`
        main.rue:4:11-4:14 Error: Duplicate symbol `Int` found in scope
        main.rue:4:14-4:15 Error: Expected `:`, found `)`
    "#]]
    .assert_eq(&project.change(
        "main.rue",
        Some("fn main() -> Int {\n    42\n}\n\nfn f (Int, Int) {}\n"),
    ));
}

/// Renders the code actions for a diagnostic, along with the text of each file after applying the
/// edits of each action to it.
fn render_code_actions(
    project: &TestProject,
    diagnostic: &rue_diagnostic::Diagnostic,
    files: &[(&str, &str)],
) -> String {
    let actions = code_actions(&CodeActionParams {
        text_document: TextDocumentIdentifier::new(project.uri(files[0].0)),
        range: Range::default(),
        context: CodeActionContext {
            diagnostics: vec![crate::diagnostic(
                diagnostic,
                true,
                &project.backend().shared.paths,
            )],
            only: None,
            trigger_kind: None,
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
    })
    .unwrap_or_default();

    let mut output = String::new();

    for action in actions {
        let CodeActionOrCommand::CodeAction(action) = action else {
            panic!("unexpected command: {action:?}");
        };

        writeln!(
            output,
            "{} (preferred: {}):",
            action.title,
            action.is_preferred.unwrap()
        )
        .unwrap();

        let mut changes = action.edit.unwrap().changes.unwrap();

        for (name, text) in files {
            let Some(mut edits) = changes.remove(&project.uri(name)) else {
                continue;
            };

            // The edits of a file don't overlap, so they're applied from the end of the file, which
            // keeps the positions of the earlier ones valid.
            edits.sort_by_key(|edit| edit.range.start);

            let mut fixed = (*text).to_string();

            for edit in edits.into_iter().rev() {
                let start = position_to_offset(text, edit.range.start);
                let end = position_to_offset(text, edit.range.end);
                fixed.replace_range(start..end, &edit.new_text);
            }

            // Carriage returns are shown, since they'd be invisible in the snapshot
            writeln!(output, "{name}:\n{}", fixed.replace('\r', "\\r")).unwrap();
        }

        assert!(changes.is_empty(), "edits to unexpected files: {changes:?}");
    }

    output
}

fn source(project: &TestProject, name: &str, text: &str) -> Source {
    Source::new(
        Arc::from(text),
        SourceKind::File(project.path(name).to_string_lossy().into_owned()),
    )
}

#[test]
fn test_code_actions_multiple_edits_and_files() {
    let main = "fn main() -> Int {\r\n    first() + second()\r\n}\r\n";
    let utils = "export fn first() -> Int { 1 }\n\nexport fn second() -> Int { 2 }\n";
    let files = [("main.rue", main), ("utils.rue", utils)];
    let project = TestProject::new(&files);

    let main_source = source(&project, "main.rue", main);
    let utils_source = source(&project, "utils.rue", utils);
    let std_source = Source::new(Arc::from("fn x() {}"), SourceKind::Std("std".to_string()));

    let mut diagnostic = rue_diagnostic::Diagnostic::new(
        SrcLoc::new(main_source.clone(), 24..31),
        DiagnosticKind::Lint {
            lint: Lint::UnusedFunction,
            message: "Rename the functions".to_string(),
        },
        rue_diagnostic::DiagnosticSeverity::Warning,
    );

    // The edits are out of order, and span both files
    diagnostic.suggestions.push(Suggestion::new(
        "Rename `first` and `second`",
        vec![
            Edit::new(SrcLoc::new(main_source.clone(), 34..40), "two"),
            Edit::new(SrcLoc::new(utils_source.clone(), 10..15), "one"),
            Edit::new(SrcLoc::new(main_source.clone(), 24..29), "one"),
            Edit::new(SrcLoc::new(utils_source.clone(), 42..48), "two"),
        ],
        Applicability::MaybeIncorrect,
    ));

    // A suggestion with an edit that can't be made in the editor is left out entirely, rather
    // than being applied partially
    diagnostic.suggestions.push(Suggestion::new(
        "Edit the standard library",
        vec![
            Edit::new(SrcLoc::new(main_source.clone(), 24..29), "one"),
            Edit::new(SrcLoc::new(std_source, 3..4), "y"),
        ],
        Applicability::MachineApplicable,
    ));

    // Two edits at the same position insert both texts
    diagnostic.suggestions.push(Suggestion::new(
        "Wrap the call",
        vec![
            Edit::new(SrcLoc::new(main_source.clone(), 24..24), "("),
            Edit::new(SrcLoc::new(main_source, 31..31), ")"),
        ],
        Applicability::MachineApplicable,
    ));

    expect![[r#"
        Rename `first` and `second` (preferred: false):
        main.rue:
        fn main() -> Int {\r
            one() + two()\r
        }\r

        utils.rue:
        export fn one() -> Int { 1 }

        export fn two() -> Int { 2 }

        Wrap the call (preferred: true):
        main.rue:
        fn main() -> Int {\r
            (first()) + second()\r
        }\r

    "#]]
    .assert_eq(&render_code_actions(&project, &diagnostic, &files));
}
