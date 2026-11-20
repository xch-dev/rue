//! The outline of a file, and the symbols of the workspace that match a search.

use expect_test::{Expect, expect};

use super::TestProject;

#[allow(clippy::needless_pass_by_value)]
fn check(source: &str, expect: Expect) {
    let project = TestProject::new(&[("main.rue", source)]);
    expect.assert_eq(&project.document_symbols("main.rue"));
}

const SOURCE: &str = r#"
import utils::helper;

const LIMIT: Int = 10;

inline const NAME = "rue";

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
    check(
        SOURCE,
        expect![[r#"
        Constant LIMIT `Int` 3:0-3:22 [3:6-3:11]
        Constant NAME 5:0-5:26 [5:13-5:17]
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
            Field radius `Int` 21:17-21:29 [21:17-21:23]
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
              Field y 3:4-4:0 [3:4-3:5]
            Function broken `(-> Int)` 10:0-13:0 [10:3-10:9]
            Enum Shape 13:0-16:0 [13:5-13:10]
              EnumMember Circle 14:4-14:28 [14:4-14:10]
                Field radius `Int` 14:15-14:27 [14:15-14:21]
        "#]],
    );

    check(
        "mod {\n    fn inner() {}\n}\n\nmod named {\n    fn inner() {\n",
        expect![[r#"
            Module named 4:0-6:0 [4:4-4:9]
              Function inner `()` 5:4-6:0 [5:7-5:12]
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
    // Each project that has been compiled is searched, and a file is only listed once
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
