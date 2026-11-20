//! The outline of a file, and the symbols of the workspace that match a search.

use expect_test::{Expect, expect};

use super::TestProject;

#[allow(clippy::needless_pass_by_value)]
fn check(source: &str, expect: Expect) {
    let project = TestProject::new(&[("main.rue", source)]);
    expect.assert_eq(&project.document_symbols("main.rue"));
}

/// Shows the outline of a file that compiles, next to the files it uses.
#[allow(clippy::needless_pass_by_value)]
fn check_compiled(files: &[(&str, &str)], expect: Expect) {
    let project = TestProject::new(files);
    let diagnostics = project.diagnostics();
    assert!(!diagnostics.contains(" Error"), "{diagnostics}");
    expect.assert_eq(&project.document_symbols(files[0].0));
}

const SOURCE: &str = r#"
import utils::helper;

const LIMIT: Int = 10;

inline const NAME: String = "rue";

type Amount = Int;

type Pair<T> = (T, T);

struct Point {
    x: Int,
    y: Int = 0,
    ...rest: List<Int>,
}

struct Empty {}

enum Shape {
    Empty,
    Circle = 1 { radius: Int },
    Rect = 2 {
        width: Int,
        height: Int,
    },
}

export fn main(point: Point, shape: Shape) -> Int {
    let inner = fn(value: Int) => value + 1;
    inner(point.x) + LIMIT
}

inline fn id<T>(value: T) -> T {
    value
}

extern fn run(value: Any) -> Any from "./run.hex";

test fn adds() {
    assert id(1) == 1;
}

mod math {
    export const ONE: Int = 1;

    export fn double(value: Int) -> Int {
        value * 2
    }

    mod deeper {
        struct Inner {
            value: Int,
        }
    }
}
"#;

#[test]
fn test_document_symbols() {
    check_compiled(
        &[
            ("main.rue", SOURCE),
            ("utils.rue", "export fn helper() -> Int {\n    1\n}\n"),
            ("run.hex", "01"),
        ],
        expect![[r#"
            Constant LIMIT `Int` 3:0-3:22 [3:6-3:11]
            Constant NAME `String` 5:0-5:34 [5:13-5:17]
            TypeParameter Amount `Int` 7:0-7:18 [7:5-7:11]
            TypeParameter Pair `(T, T)` 9:0-9:22 [9:5-9:9]
            Struct Point 11:0-15:1 [11:7-11:12]
              Field x `Int` 12:4-12:10 [12:4-12:5]
              Field y `Int` 13:4-13:14 [13:4-13:5]
              Field rest `List<Int>` 14:4-14:22 [14:7-14:11]
            Struct Empty 17:0-17:15 [17:7-17:12]
            Enum Shape 19:0-26:1 [19:5-19:10]
              EnumMember Empty 20:4-20:9 [20:4-20:9]
              EnumMember Circle 21:4-21:30 [21:4-21:10]
                Field radius `Int` 21:17-21:28 [21:17-21:23]
              EnumMember Rect 22:4-25:5 [22:4-22:8]
                Field width `Int` 23:8-23:18 [23:8-23:13]
                Field height `Int` 24:8-24:19 [24:8-24:14]
            Function main `(point: Point, shape: Shape) -> Int` 28:0-31:1 [28:10-28:14]
            Function id `<T>(value: T) -> T` 33:0-35:1 [33:10-33:12]
            Function run `(value: Any) -> Any` 37:0-37:50 [37:10-37:13]
            Function adds `test ()` 39:0-41:1 [39:8-39:12]
            Module math 43:0-55:1 [43:4-43:8]
              Constant ONE `Int` 44:4-44:30 [44:17-44:20]
              Function double `(value: Int) -> Int` 46:4-48:5 [46:14-46:20]
              Module deeper 50:4-54:5 [50:8-50:14]
                Struct Inner 51:8-53:9 [51:15-51:20]
                  Field value `Int` 52:12-52:22 [52:12-52:17]
        "#]],
    );
}

#[test]
fn test_document_symbols_of_an_empty_file() {
    check(
        "",
        expect![[r#"
        <empty>
    "#]],
    );
    check(
        "// Only a comment\n",
        expect![[r#"
        <empty>
    "#]],
    );
    check(
        "import utils::helper;\n",
        expect![[r#"
        <empty>
    "#]],
    );
}

#[test]
fn test_document_symbols_with_long_signature() {
    // A signature is shown on one line, and cut short if it's long
    check(
        "fn long(\n    first: Int,\n    second: Bytes32,\n    third: PublicKey,\n    fourth: List<(Int, Bytes32)>,\n    fifth: fn(value: Int) -> Int,\n) -> (Int, Bytes32) {\n    (first, second)\n}\n",
        expect![[r#"
            Function long `(first: Int, second: Bytes32, third: PublicKey, fourth: List<(Int, Bytes32)>,...` 0:0-8:1 [0:3-0:7]
        "#]],
    );
}

#[test]
fn test_document_symbols_detail_length() {
    // A detail of 80 characters is whole, and a longer one is cut to 80 with what's left of it
    // trimmed
    let detail_of = |ty: &str| {
        let project = TestProject::new(&[("main.rue", &format!("type Long = {ty};\n"))]);
        let symbols = project.document_symbols("main.rue");
        let detail = symbols.split('`').nth(1).unwrap().to_string();
        (detail.chars().count(), detail)
    };

    let exact = format!("({}, Int)", "A".repeat(73));
    assert_eq!(detail_of(&exact), (80, exact.clone()));

    let longer = format!("({}, Int)", "A".repeat(75));
    assert_eq!(detail_of(&longer), (80, format!("({},...", "A".repeat(75))));

    // The cut is after a space, which isn't kept
    let spaced = format!("({}, Int)", "A".repeat(74));
    assert_eq!(detail_of(&spaced), (79, format!("({},...", "A".repeat(74))));

    // A test's detail is cut with `test` in it
    let project = TestProject::new(&[(
        "main.rue",
        &format!(
            "test fn long() -> ({}, Int) {{\n    (1, 2)\n}}\n",
            "A".repeat(74)
        ),
    )]);
    let symbols = project.document_symbols("main.rue");
    assert_eq!(symbols.split('`').nth(1).unwrap().chars().count(), 80);
}

#[test]
fn test_document_symbols_without_comments_in_details() {
    check(
        "fn add(first: Int, // the first\n    second: Int /* the second */) -> Int // the sum\n{\n    first + second\n}\n\nconst ONE: Int /* one */ = 1;\n\nstruct Point {\n    x: Int, // across\n    y: /* up */ Int\n}\n",
        expect![[r#"
            Function add `(first: Int, second: Int) -> Int` 0:0-4:1 [0:3-0:6]
            Constant ONE `Int` 6:0-6:29 [6:6-6:9]
            Struct Point 8:0-11:1 [8:7-8:12]
              Field x `Int` 9:4-9:10 [9:4-9:5]
              Field y `Int` 10:4-10:19 [10:4-10:5]
        "#]],
    );
}

#[test]
fn test_document_symbols_end_before_comments() {
    // The comments after a field or a variant aren't part of it
    check(
        "struct Point {\n    x: Int /* x */, // more\n    y: Int // last\n}\n\nenum Shape {\n    A /* a */,\n    B = 1 // b\n    ,\n    C { x: Int /* x */ } // c\n}\n",
        expect![[r#"
            Struct Point 0:0-3:1 [0:7-0:12]
              Field x `Int` 1:4-1:10 [1:4-1:5]
              Field y `Int` 2:4-2:10 [2:4-2:5]
            Enum Shape 5:0-10:1 [5:5-5:10]
              EnumMember A 6:4-6:5 [6:4-6:5]
              EnumMember B 7:4-7:9 [7:4-7:5]
              EnumMember C 9:4-9:24 [9:4-9:5]
                Field x `Int` 9:8-9:14 [9:8-9:9]
        "#]],
    );
}

#[test]
fn test_document_symbols_with_attributes_and_comments() {
    // An item starts at its attributes, and the comments before it aren't part of it
    check(
        "// A comment\n#[allow(unused_function)]\nfn first() -> Int {\n    1\n}\n\n/* Another */ const SECOND: Int = 2; // After\n",
        expect![[r#"
            Function first `() -> Int` 1:0-4:1 [2:3-2:8]
            Constant SECOND `Int` 6:14-6:36 [6:20-6:26]
        "#]],
    );
}

#[test]
fn test_document_symbols_with_syntax_errors() {
    // What parsed is still in the outline, and what has no name isn't
    check(
        "struct Point {\n    x: Int,\n    : Int,\n    y\n}\n\nfn () -> Int {\n    1\n}\n\nfn broken( -> Int {\n    let value = ;\n\nenum Shape {\n    Circle = { radius: Int },\n    = 2,\n",
        expect![[r#"
            Struct Point 0:0-4:1 [0:7-0:12]
              Field x `Int` 1:4-1:10 [1:4-1:5]
              Field y 3:4-3:5 [3:4-3:5]
            Function broken `(-> Int)` 10:0-11:17 [10:3-10:9]
            Enum Shape 13:0-15:8 [13:5-13:10]
              EnumMember Circle 14:4-14:28 [14:4-14:10]
                Field radius `Int` 14:15-14:26 [14:15-14:21]
        "#]],
    );

    check(
        "mod {\n    fn inner() {}\n}\n\nmod named {\n    fn inner() {\n",
        expect![[r#"
            Module named 4:0-5:16 [4:4-4:9]
              Function inner `()` 5:4-5:16 [5:7-5:12]
        "#]],
    );

    check(
        "fn\n",
        expect![[r#"
        <empty>
    "#]],
    );
}

#[test]
fn test_document_symbols_after_non_ascii() {
    // Positions are in UTF-16 code units
    check(
        "const TEXT: String = \"\u{1F600}\u{e9}\"; const OTHER: Int = 1;\n\n/* \u{1F600} */ fn after() -> String {\n    \"\u{e9}\"\n}\n",
        expect![[r#"
            Constant TEXT `String` 0:0-0:27 [0:6-0:10]
            Constant OTHER `Int` 0:28-0:49 [0:34-0:39]
            Function after `() -> String` 2:9-4:1 [2:12-2:17]
        "#]],
    );
}

#[test]
fn test_document_symbols_of_each_file() {
    let project = TestProject::new(&[
        ("main.rue", "fn main() -> Int {\n    utils::ONE\n}\n"),
        (
            "utils.rue",
            "export const ONE: Int = 1;\n\nexport struct Point {\n    x: Int,\n}\n",
        ),
        ("nested/deep.rue", "export fn deep() -> Int {\n    1\n}\n"),
    ]);

    expect![[r#"
        Function main `() -> Int` 0:0-2:1 [0:3-0:7]
    "#]]
    .assert_eq(&project.document_symbols("main.rue"));
    expect![[r#"
        Constant ONE `Int` 0:0-0:26 [0:13-0:16]
        Struct Point 2:0-4:1 [2:14-2:19]
          Field x `Int` 3:4-3:10 [3:4-3:5]
    "#]]
    .assert_eq(&project.document_symbols("utils.rue"));
    expect![[r#"
        Function deep `() -> Int` 0:0-2:1 [0:10-0:14]
    "#]]
    .assert_eq(&project.document_symbols("nested/deep.rue"));
}

fn workspace() -> TestProject {
    TestProject::new(&[
        (
            "main.rue",
            "fn main() -> Int {\n    utils::double(utils::ONE)\n}\n\nstruct Point {\n    x: Int,\n    double: Int,\n}\n",
        ),
        (
            "utils.rue",
            "export const ONE: Int = 1;\n\nexport fn double(value: Int) -> Int {\n    value * 2\n}\n\nenum Shape {\n    Dot,\n    Circle { radius: Int },\n}\n",
        ),
        (
            "nested/deep.rue",
            "mod inner {\n    export fn doubled() -> Int {\n        2\n    }\n}\n",
        ),
    ])
}

#[test]
fn test_workspace_symbols() {
    let project = workspace();

    // The best matches are first, and each symbol says what it's inside of
    expect![[r#"
        Field double in Point main.rue:6:4-6:10
        Function double utils.rue:2:10-2:16
        Function doubled in inner nested/deep.rue:1:14-1:21
    "#]]
    .assert_eq(&project.workspace_symbols("double"));
    expect![[r#"
        Field double in Point main.rue:6:4-6:10
        Function double utils.rue:2:10-2:16
        Function doubled in inner nested/deep.rue:1:14-1:21
    "#]]
    .assert_eq(&project.workspace_symbols("dbl"));
    expect![[r#"
        Enum Shape utils.rue:6:5-6:10
    "#]]
    .assert_eq(&project.workspace_symbols("SHAPE"));
    expect![[r#"
        Field radius in Circle utils.rue:8:13-8:19
    "#]]
    .assert_eq(&project.workspace_symbols("radius"));
    expect![[r#"
        <empty>
    "#]]
    .assert_eq(&project.workspace_symbols("missing"));
}

#[test]
fn test_workspace_symbols_without_a_query() {
    // Every symbol of the project matches
    expect![[r#"
        EnumMember Circle in Shape utils.rue:8:4-8:10
        EnumMember Dot in Shape utils.rue:7:4-7:7
        Constant ONE utils.rue:0:13-0:16
        Struct Point main.rue:4:7-4:12
        Enum Shape utils.rue:6:5-6:10
        Field double in Point main.rue:6:4-6:10
        Function double utils.rue:2:10-2:16
        Function doubled in inner nested/deep.rue:1:14-1:21
        Module inner nested/deep.rue:0:4-0:9
        Function main main.rue:0:3-0:7
        Field radius in Circle utils.rue:8:13-8:19
        Field x in Point main.rue:5:4-5:5
    "#]]
    .assert_eq(&workspace().workspace_symbols(""));
}

#[test]
fn test_workspace_symbols_of_several_projects() {
    // Each project that has been compiled is searched
    let project = workspace();

    let other = super::TempDir::new("rue-lsp-symbols-other");
    let path = other.path().join("main.rue");
    let text = "fn main() -> Int {\n    double_other(1)\n}\n\nfn double_other(value: Int) -> Int {\n    value\n}\n";
    std::fs::write(other.path().join("Rue.toml"), super::MANIFEST).unwrap();
    std::fs::write(&path, text).unwrap();

    let backend = project.backend();
    let uri = crate::paths::path_to_uri(&path).unwrap();
    backend.shared.paths.open(&uri);
    backend.on_change_impl(&uri, Some(text.to_string()));

    let symbols = project.workspace_symbols("double");
    let other_root = other.path().display().to_string();

    // The other project's file isn't under this one's directory, so it's shown with its whole path
    expect![[r#"
        Field double in Point main.rue:6:4-6:10
        Function double utils.rue:2:10-2:16
        Function double_other <other>/main.rue:4:3-4:15
        Function doubled in inner nested/deep.rue:1:14-1:21
    "#]]
    .assert_eq(&symbols.replace(&other_root, "<other>"));
}

#[test]
fn test_workspace_symbols_with_syntax_errors() {
    let project = TestProject::new(&[
        (
            "main.rue",
            "fn main() -> Int {\n    let value = ;\n\nfn other( {\n",
        ),
        ("utils.rue", "export fn helper() -> Int {\n    1\n"),
    ]);

    expect![[r#"
        Function helper utils.rue:0:10-0:16
        Function main main.rue:0:3-0:7
        Function other main.rue:3:3-3:8
    "#]]
    .assert_eq(&project.workspace_symbols(""));
}

#[test]
fn test_workspace_symbols_best_match_first() {
    // A name that starts with the query is before one that only has its letters, whatever
    // their order by name is
    let project = TestProject::new(&[(
        "main.rue",
        "const amazed: Int = 1;\n\nconst zeta: Int = 2;\n\nconst azure_tide: Int = 3;\n",
    )]);

    expect![[r#"
        Constant zeta main.rue:2:6-2:10
        Constant amazed main.rue:0:6-0:12
        Constant azure_tide main.rue:4:6-4:16
    "#]]
    .assert_eq(&project.workspace_symbols("ze"));
}

#[test]
fn test_workspace_symbols_of_a_file_in_two_projects() {
    // The directory of the second project is inside of the first, so both of them have its
    // file, which is still only listed once
    let inner = "struct First {\n    x: Int,\n}\n\nstruct Second {\n    x: Int,\n}\n";

    let project = TestProject::new(&[
        ("main.rue", "fn main() -> Int {\n    1\n}\n"),
        ("sub/Rue.toml", super::MANIFEST),
        ("sub/inner.rue", inner),
    ]);

    expect![[r#"
        Struct First sub/inner.rue:0:7-0:12
        Struct Second sub/inner.rue:4:7-4:13
        Function main main.rue:0:3-0:7
        Field x in First sub/inner.rue:1:4-1:5
        Field x in Second sub/inner.rue:5:4-5:5
    "#]]
    .assert_eq(&project.workspace_symbols(""));
    expect![[r#"
        Field x in First sub/inner.rue:1:4-1:5
        Field x in Second sub/inner.rue:5:4-5:5
    "#]]
    .assert_eq(&project.workspace_symbols("x"));

    // An edit is only compiled by one of the projects, and the symbols are where they are now
    project.change("sub/inner.rue", Some(&format!("\n\n\n{inner}")));

    expect![[r#"
        Field x in First sub/inner.rue:4:4-4:5
        Field x in Second sub/inner.rue:8:4-8:5
    "#]]
    .assert_eq(&project.workspace_symbols("x"));
}

#[test]
fn test_workspace_symbols_without_the_standard_library() {
    // The standard library has no file to open, so its symbols aren't listed
    let project = TestProject::with_manifest(
        Some("[compiler]\nentrypoint = \".\"\n"),
        &[("main.rue", "fn main() -> Bytes32 {\n    sha256(\"a\")\n}\n")],
    );

    expect![[r#"
        Function main main.rue:0:3-0:7
    "#]]
    .assert_eq(&project.workspace_symbols(""));
    expect![[r#"
        <empty>
    "#]]
    .assert_eq(&project.workspace_symbols("sha"));
}
