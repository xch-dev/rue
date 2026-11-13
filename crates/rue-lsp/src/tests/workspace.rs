//! Two sibling projects in one workspace, each with its own `Rue.toml`. Each is compiled on its
//! own, so names they share don't refer to each other's declarations.

use expect_test::{Expect, expect};

use super::TestProject;

const MANIFEST: &str = "[compiler]\nentrypoint = \".\"\nstd = false\n";

const FIRST: &str = r#"
fn main() -> Int {
    helper(1)
}

fn helper(num: Int) -> Int {
    num * 2
}
"#;

const SECOND: &str = r#"
fn main() -> Bool {
    helper(true)
}

fn helper(flag: Bool) -> Bool {
    !flag
}
"#;

fn project(first: &str, second: &str) -> TestProject {
    TestProject::with_manifest(
        None,
        &[
            ("first/Rue.toml", MANIFEST),
            ("first/main.rue", first),
            ("second/Rue.toml", MANIFEST),
            ("second/main.rue", second),
        ],
    )
}

#[allow(clippy::needless_pass_by_value)]
fn check(first: &str, second: &str, expect: Expect) {
    let project = project(first, second);

    let output = format!(
        "{}completions:\n{}",
        project.queries(),
        project.completions()
    );

    expect.assert_eq(&output);
}

#[test]
fn test_workspace_queries_in_first_project() {
    check(
        &FIRST.replace("helper(1)", "hel$0per(1)"),
        SECOND,
        expect![[r#"
        hover:
        fn helper(num: Int) -> Int
        definitions:
        first/main.rue:5:3-5:9
        references:
        first/main.rue:2:4-2:10
        first/main.rue:5:3-5:9
        completions:
        helper [Function] fn(num: Int) -> Int
    "#]],
    );
}

#[test]
fn test_workspace_queries_in_second_project() {
    check(
        FIRST,
        &SECOND.replace("helper(true)", "hel$0per(true)"),
        expect![[r#"
        hover:
        fn helper(flag: Bool) -> Bool
        definitions:
        second/main.rue:5:3-5:9
        references:
        second/main.rue:2:4-2:10
        second/main.rue:5:3-5:9
        completions:
        helper [Function] fn(flag: Bool) -> Bool
    "#]],
    );
}

#[test]
fn test_workspace_completions_stay_in_project() {
    check(
        &FIRST.replace("helper(1)", "hel$0"),
        &SECOND.replace(
            "fn helper",
            "fn helper_only_in_second() -> Int { 1 }\n\nfn helper",
        ),
        expect![[r#"
            hover:
            <none>
            definitions:
            <none>
            references:
            <none>
            completions:
            helper [Function] fn(num: Int) -> Int
        "#]],
    );
}

#[test]
fn test_workspace_diagnostics_stay_in_project() {
    let project = project(FIRST, SECOND);

    // Only the files of the project that changed are reported
    expect![[r#"
        first/Rue.toml: <none>
        first/main.rue:1:8-1:14 Warning[unused_binding]: Unused binding `unused`
        first/main.rue:2:11-2:15 Error: Cannot assign `true` to `Int` without a cast, since they are semantically different
    "#]].assert_eq(&project.change(
        "first/main.rue",
        Some("fn main() -> Int {\n    let unused = 1;\n    helper(true)\n}\n\nfn helper(num: Int) -> Int {\n    num * 2\n}\n"),
    ));

    expect![[r#"
        second/Rue.toml: <none>
        second/main.rue: <none>
    "#]]
    .assert_eq(&project.change("second/main.rue", Some(SECOND)));
}
