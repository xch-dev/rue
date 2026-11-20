//! Renaming changes a declaration and everything that refers to it, and refuses when it can't do
//! that without changing what the code means.

use std::sync::atomic::Ordering;

use expect_test::{Expect, expect};

use super::TestProject;

/// A project whose client can rename files.
fn project(files: &[(&str, &str)]) -> TestProject {
    let project = TestProject::new(files);
    project
        .backend()
        .shared
        .rename_files
        .store(true, Ordering::Relaxed);
    project
}

/// Renames what's at the cursor of code that compiles, so that a test is about what it means to
/// be, rather than what's left of code that was rejected.
#[allow(clippy::needless_pass_by_value)]
fn check(files: &[(&str, &str)], new_name: &str, expect: Expect) {
    let project = project(files);
    let diagnostics = project.diagnostics();
    assert!(!diagnostics.contains(" Error"), "{diagnostics}");
    expect.assert_eq(&project.rename(new_name));
}

/// Renames what's at the cursor of code that has errors.
#[allow(clippy::needless_pass_by_value)]
fn check_with_errors(files: &[(&str, &str)], new_name: &str, expect: Expect) {
    let project = project(files);
    let diagnostics = project.diagnostics();
    assert!(diagnostics.contains(" Error"), "{diagnostics}");
    expect.assert_eq(&project.rename(new_name));
}

#[allow(clippy::needless_pass_by_value)]
fn check_prepare(files: &[(&str, &str)], expect: Expect) {
    let project = TestProject::new(files);
    expect.assert_eq(&project.prepare_rename());
}

#[test]
fn test_rename_function() {
    check(
        &[(
            "main.rue",
            r#"
fn main() -> Int {
    double(21) + dou$0ble(1)
}

fn double(num: Int) -> Int {
    num * 2
}
"#,
        )],
        "twice",
        expect![[r#"
            main.rue:

            fn main() -> Int {
                twice(21) + twice(1)
            }

            fn twice(num: Int) -> Int {
                num * 2
            }
        "#]],
    );
}

const POINT: &str = "struct Point {\n    x: Int,\n    y: Int,\n}\n";

fn with_point(source: &str) -> String {
    format!("{POINT}{source}")
}

#[test]
fn test_prepare_rename() {
    check_prepare(
        &[(
            "main.rue",
            "fn main() -> Int {\n    let value = 1;\n    val$0ue\n}\n",
        )],
        expect![[r#"
            main.rue:2:4-2:9 `value`
        "#]],
    );

    // The cursor can be right after the name
    check_prepare(
        &[(
            "main.rue",
            "fn main() -> Int {\n    let value = 1;\n    value$0\n}\n",
        )],
        expect![[r#"
            main.rue:2:4-2:9 `value`
        "#]],
    );

    // Nothing but names can be renamed
    check_prepare(
        &[(
            "main.rue",
            "fn main() -> Int {\n    let value = 1;\n    value $0+ 1\n}\n",
        )],
        expect![[r#"
            <none>
        "#]],
    );
}

#[test]
fn test_rename_binding_that_shadows() {
    // Only the inner binding is renamed
    check(
        &[(
            "main.rue",
            r#"
fn main() -> Int {
    let value = 42;
    let inner = {
        let value = 2;
        val$0ue + 1
    };
    value + inner
}
"#,
        )],
        "other",
        expect![[r#"
            main.rue:

            fn main() -> Int {
                let value = 42;
                let inner = {
                    let other = 2;
                    other + 1
                };
                value + inner
            }
        "#]],
    );
}

#[test]
fn test_rename_parameter() {
    check(
        &[(
            "main.rue",
            "fn double($0num: Int) -> Int {\n    num * 2 + num\n}\n",
        )],
        "value",
        expect![[r#"
            main.rue:
            fn double(value: Int) -> Int {
                value * 2 + value
            }
        "#]],
    );
}

#[test]
fn test_rename_lambda_parameter() {
    check(
        &[(
            "main.rue",
            "fn main() -> Int {\n    let add = fn(num: Int) => n$0um + 1;\n    add(1)\n}\n",
        )],
        "value",
        expect![[r#"
            main.rue:
            fn main() -> Int {
                let add = fn(value: Int) => value + 1;
                add(1)
            }
        "#]],
    );
}

#[test]
fn test_rename_constant() {
    check(
        &[(
            "main.rue",
            "const ON$0E: Int = 1;\n\nfn main() -> Int {\n    ONE + ONE\n}\n",
        )],
        "FIRST",
        expect![[r#"
            main.rue:
            const FIRST: Int = 1;

            fn main() -> Int {
                FIRST + FIRST
            }
        "#]],
    );
}

#[test]
fn test_rename_type_alias() {
    check(
        &[(
            "main.rue",
            "type Amount = Int;\n\nfn main(amount: Amo$0unt) -> Amount {\n    amount\n}\n",
        )],
        "Mojos",
        expect![[r#"
            main.rue:
            type Mojos = Int;

            fn main(amount: Mojos) -> Mojos {
                amount
            }
        "#]],
    );
}

#[test]
fn test_rename_struct() {
    check(
        &[(
            "main.rue",
            &with_point(
                "\nfn main(point: Point) -> Poi$0nt {\n    Point { x: point.x, y: 2 }\n}\n",
            ),
        )],
        "Position",
        expect![[r#"
            main.rue:
            struct Position {
                x: Int,
                y: Int,
            }

            fn main(point: Position) -> Position {
                Position { x: point.x, y: 2 }
            }
        "#]],
    );
}

#[test]
fn test_rename_generic() {
    check(
        &[(
            "main.rue",
            "fn id<T>(value: $0T) -> T {\n    value\n}\n\nstruct Wrap<T> {\n    value: T,\n}\n",
        )],
        "Value",
        expect![[r#"
            main.rue:
            fn id<Value>(value: Value) -> Value {
                value
            }

            struct Wrap<T> {
                value: T,
            }
        "#]],
    );

    check(
        &[(
            "main.rue",
            "struct Wrap<$0T> {\n    value: T,\n}\n\nfn id<T>(value: T) -> T {\n    value\n}\n",
        )],
        "Value",
        expect![[r#"
            main.rue:
            struct Wrap<Value> {
                value: Value,
            }

            fn id<T>(value: T) -> T {
                value
            }
        "#]],
    );
}

#[test]
fn test_rename_field() {
    // Every way to name the field is renamed, and a shorthand keeps the binding's name
    let source = with_point(
        r#"
fn main(point: Point) -> Int {
    let x = point.x;
    let copy = Point { x, y: point.y };
    let other = Point { x: 1, y: 2 };
    let { x: first, y } = copy;
    let { x } = other;
    first + x + y
}
"#,
    );

    check(
        &[("main.rue", &source.replacen("x: Int", "$0x: Int", 1))],
        "left",
        expect![[r#"
            main.rue:
            struct Point {
                left: Int,
                y: Int,
            }

            fn main(point: Point) -> Int {
                let x = point.left;
                let copy = Point { left: x, y: point.y };
                let other = Point { left: 1, y: 2 };
                let { left: first, y } = copy;
                let { left: x } = other;
                first + x + y
            }
        "#]],
    );

    // From a use of the field
    check(
        &[("main.rue", &source.replacen("point.x", "point.$0x", 1))],
        "left",
        expect![[r#"
            main.rue:
            struct Point {
                left: Int,
                y: Int,
            }

            fn main(point: Point) -> Int {
                let x = point.left;
                let copy = Point { left: x, y: point.y };
                let other = Point { left: 1, y: 2 };
                let { left: first, y } = copy;
                let { left: x } = other;
                first + x + y
            }
        "#]],
    );
}

#[test]
fn test_rename_field_of_generic_struct() {
    check(
        &[(
            "main.rue",
            r#"
struct Wrap<T> {
    value: T,
}

fn main(wrap: Wrap<Int>) -> Int {
    let other = Wrap::<String> { value: "a" };
    let { value } = Wrap::<Int> { value: 1 };
    wrap.val$0ue + other.value.length + value
}
"#,
        )],
        "inner",
        expect![[r#"
            main.rue:

            struct Wrap<T> {
                inner: T,
            }

            fn main(wrap: Wrap<Int>) -> Int {
                let other = Wrap::<String> { inner: "a" };
                let { inner: value } = Wrap::<Int> { inner: 1 };
                wrap.inner + other.inner.length + value
            }
        "#]],
    );
}

#[test]
fn test_rename_binding_in_shorthand_field() {
    // The field keeps its name when the binding that a shorthand uses is renamed
    check(
        &[(
            "main.rue",
            &with_point("\nfn main(y: Int) -> Point {\n    let x = 1;\n    Point { $0x, y }\n}\n"),
        )],
        "left",
        expect![[r#"
            main.rue:
            struct Point {
                x: Int,
                y: Int,
            }

            fn main(y: Int) -> Point {
                let left = 1;
                Point { x: left, y }
            }
        "#]],
    );

    // And when the binding that a shorthand declares is
    check(
        &[(
            "main.rue",
            &with_point(
                "\nfn main(point: Point) -> Int {\n    let { x, y } = point;\n    $0x + y\n}\n",
            ),
        )],
        "left",
        expect![[r#"
            main.rue:
            struct Point {
                x: Int,
                y: Int,
            }

            fn main(point: Point) -> Int {
                let { x: left, y } = point;
                left + y
            }
        "#]],
    );

    // The rest of a struct is only a binding
    check(
        &[(
            "main.rue",
            &with_point(
                "\nfn main(point: Point) -> Int {\n    let { x, ...rest } = point;\n    x + re$0st.y\n}\n",
            ),
        )],
        "others",
        expect![[r#"
            main.rue:
            struct Point {
                x: Int,
                y: Int,
            }

            fn main(point: Point) -> Int {
                let { x, ...others } = point;
                x + others.y
            }
        "#]],
    );
}

#[test]
fn test_rename_enum_and_variants() {
    let source = r#"
enum Shape {
    Circle = 1 { radius: Int },
    Empty = 2,
}

fn main(shape: Shape) -> Int {
    let empty: Shape = Shape::Empty;
    let circle = Shape::Circle { radius: 1 };
    if shape is Shape::Circle {
        shape.radius + circle.radius
    } else if empty is Shape::Empty {
        0
    } else {
        1
    }
}
"#;

    check(
        &[(
            "main.rue",
            &source.replacen("enum Shape", "enum Sha$0pe", 1),
        )],
        "Figure",
        expect![[r#"
            main.rue:

            enum Figure {
                Circle = 1 { radius: Int },
                Empty = 2,
            }

            fn main(shape: Figure) -> Int {
                let empty: Figure = Figure::Empty;
                let circle = Figure::Circle { radius: 1 };
                if shape is Figure::Circle {
                    shape.radius + circle.radius
                } else if empty is Figure::Empty {
                    0
                } else {
                    1
                }
            }
        "#]],
    );

    check(
        &[(
            "main.rue",
            &source.replacen("Shape::Empty", "Shape::Emp$0ty", 1),
        )],
        "Nothing",
        expect![[r#"
            main.rue:

            enum Shape {
                Circle = 1 { radius: Int },
                Nothing = 2,
            }

            fn main(shape: Shape) -> Int {
                let empty: Shape = Shape::Nothing;
                let circle = Shape::Circle { radius: 1 };
                if shape is Shape::Circle {
                    shape.radius + circle.radius
                } else if empty is Shape::Nothing {
                    0
                } else {
                    1
                }
            }
        "#]],
    );

    check(
        &[(
            "main.rue",
            &source.replacen("Circle = 1", "Cir$0cle = 1", 1),
        )],
        "Round",
        expect![[r#"
            main.rue:

            enum Shape {
                Round = 1 { radius: Int },
                Empty = 2,
            }

            fn main(shape: Shape) -> Int {
                let empty: Shape = Shape::Empty;
                let circle = Shape::Round { radius: 1 };
                if shape is Shape::Round {
                    shape.radius + circle.radius
                } else if empty is Shape::Empty {
                    0
                } else {
                    1
                }
            }
        "#]],
    );

    check(
        &[(
            "main.rue",
            &source.replacen("shape.radius", "shape.rad$0ius", 1),
        )],
        "size",
        expect![[r#"
            main.rue:

            enum Shape {
                Circle = 1 { size: Int },
                Empty = 2,
            }

            fn main(shape: Shape) -> Int {
                let empty: Shape = Shape::Empty;
                let circle = Shape::Circle { size: 1 };
                if shape is Shape::Circle {
                    shape.size + circle.size
                } else if empty is Shape::Empty {
                    0
                } else {
                    1
                }
            }
        "#]],
    );
}

#[test]
fn test_rename_inline_module() {
    check(
        &[(
            "main.rue",
            r#"
mod math {
    export fn double(num: Int) -> Int {
        num * 2
    }
}

import math::double;

fn main() -> Int {
    ma$0th::double(21) + double(1)
}
"#,
        )],
        "numbers",
        expect![[r#"
            main.rue:

            mod numbers {
                export fn double(num: Int) -> Int {
                    num * 2
                }
            }

            import numbers::double;

            fn main() -> Int {
                numbers::double(21) + double(1)
            }
        "#]],
    );
}

#[test]
fn test_rename_module_named_by_super() {
    // A `super` that refers to the module doesn't name it, so it's left alone
    check(
        &[(
            "main.rue",
            r#"
mod ou$0ter {
    export const ONE: Int = 1;

    export mod inner {
        export fn one() -> Int {
            super::ONE
        }
    }
}

fn main() -> Int {
    outer::inner::one()
}
"#,
        )],
        "wrapper",
        expect![[r#"
            main.rue:

            mod wrapper {
                export const ONE: Int = 1;

                export mod inner {
                    export fn one() -> Int {
                        super::ONE
                    }
                }
            }

            fn main() -> Int {
                wrapper::inner::one()
            }
        "#]],
    );
}

#[test]
fn test_rename_across_files() {
    let files = [
        (
            "main.rue",
            r#"
import utils::{greet, Point};
import utils::inner::{thing};

fn main(point: Point) -> Int {
    greet() + utils::greet() + thing() + project::utils::greet() + point.x
}
"#,
        ),
        (
            "utils.rue",
            r#"
export struct Point {
    x: Int,
}

export fn gre$0et() -> Int {
    1
}

export mod inner {
    export fn thing() -> Int {
        super::greet()
    }
}
"#,
        ),
        (
            "other.rue",
            "export fn greet() -> Int {\n    utils::greet()\n}\n",
        ),
    ];

    check(
        &files,
        "hello",
        expect![[r#"
        main.rue:

        import utils::{hello, Point};
        import utils::inner::{thing};

        fn main(point: Point) -> Int {
            hello() + utils::hello() + thing() + project::utils::hello() + point.x
        }
        utils.rue:

        export struct Point {
            x: Int,
        }

        export fn hello() -> Int {
            1
        }

        export mod inner {
            export fn thing() -> Int {
                super::hello()
            }
        }
        other.rue:
        export fn greet() -> Int {
            utils::hello()
        }
    "#]],
    );
}

#[test]
fn test_rename_from_an_import() {
    check(
        &[
            (
                "main.rue",
                "import utils::{gre$0et};\n\nfn main() -> Int {\n    greet()\n}\n",
            ),
            ("utils.rue", "export fn greet() -> Int {\n    1\n}\n"),
        ],
        "hello",
        expect![[r#"
            main.rue:
            import utils::{hello};

            fn main() -> Int {
                hello()
            }
            utils.rue:
            export fn hello() -> Int {
                1
            }
        "#]],
    );
}

#[test]
fn test_rename_file_module() {
    let files = [
        (
            "main.rue",
            r#"
import utils::greet;

fn main() -> Int {
    greet() + uti$0ls::greet() + project::utils::greet()
}
"#,
        ),
        (
            "utils.rue",
            "export fn greet() -> Int {\n    project::utils::ONE\n}\n\nexport const ONE: Int = 1;\n",
        ),
    ];

    check(
        &files,
        "helpers",
        expect![[r#"
        main.rue:

        import helpers::greet;

        fn main() -> Int {
            greet() + helpers::greet() + project::helpers::greet()
        }
        utils.rue:
        export fn greet() -> Int {
            project::helpers::ONE
        }

        export const ONE: Int = 1;
        renamed: utils.rue -> helpers.rue
    "#]],
    );

    // The editor has to be able to rename the file
    let project = TestProject::new(&files);
    expect![[r#"
        error: `utils.rue` would have to be renamed, which the editor doesn't support
    "#]]
    .assert_eq(&project.rename("helpers"));
}

#[test]
fn test_rename_directory_module() {
    // A `super` that refers to the directory doesn't name it, so it's left alone
    check(
        &[
            (
                "main.rue",
                "fn main() -> Int {\n    mat$0h::ops::double(1) + math::consts::ONE\n}\n",
            ),
            (
                "math/ops.rue",
                "export fn double(num: Int) -> Int {\n    num * super::math::consts::ONE\n}\n",
            ),
            (
                "math/consts.rue",
                "export const ONE: Int = 1;\n\nfn two() -> Int {\n    project::math::ops::double(ONE)\n}\n",
            ),
            (
                "math/deep/two.rue",
                "export fn two() -> Int {\n    super::ops::double(1)\n}\n",
            ),
        ],
        "numbers",
        expect![[r#"
            main.rue:
            fn main() -> Int {
                numbers::ops::double(1) + numbers::consts::ONE
            }
            math/ops.rue:
            export fn double(num: Int) -> Int {
                num * super::numbers::consts::ONE
            }
            math/consts.rue:
            export const ONE: Int = 1;

            fn two() -> Int {
                project::numbers::ops::double(ONE)
            }
            renamed: math -> numbers
        "#]],
    );
}

#[test]
fn test_rename_nested_directory_module() {
    let files = [
        (
            "main.rue",
            "fn main() -> Int {\n    outer::fi$0rst::a::ONE + outer::second::b::TWO\n}\n",
        ),
        ("outer/first/a.rue", "export const ONE: Int = 1;\n"),
        ("outer/second/b.rue", "export const TWO: Int = 2;\n"),
    ];

    check(
        &files,
        "third",
        expect![[r#"
        main.rue:
        fn main() -> Int {
            outer::third::a::ONE + outer::second::b::TWO
        }
        renamed: outer/first -> outer/third
    "#]],
    );

    // The directory it's in already has a module with the name
    check(
        &files,
        "second",
        expect![[r#"
        error: `second` is already used in a scope that has `first`
    "#]],
    );
}

#[test]
fn test_rename_entrypoint() {
    // The project is built from its `main` file
    check(
        &[
            ("main.rue", "export fn helper() -> Int {\n    1\n}\n"),
            (
                "other.rue",
                "export fn go() -> Int {\n    ma$0in::helper()\n}\n",
            ),
        ],
        "start",
        expect![[r#"
            error: `main` is the project's entrypoint, so it can't be renamed
        "#]],
    );

    // A `main` file in another directory is like any other
    check(
        &[
            (
                "main.rue",
                "fn main() -> Int {\n    inner::ma$0in::ONE\n}\n",
            ),
            ("inner/main.rue", "export const ONE: Int = 1;\n"),
        ],
        "start",
        expect![[r#"
            main.rue:
            fn main() -> Int {
                inner::start::ONE
            }
            renamed: inner/main.rue -> inner/start.rue
        "#]],
    );
}

#[test]
fn test_rename_module_that_only_differs_in_case() {
    // The new name is the same file on a file system that ignores case
    check(
        &[
            ("main.rue", "fn main() -> Int {\n    uti$0ls::ONE\n}\n"),
            ("utils.rue", "export const ONE: Int = 1;\n"),
        ],
        "Utils",
        expect![[r#"
            main.rue:
            fn main() -> Int {
                Utils::ONE
            }
            renamed: utils.rue -> Utils.rue
        "#]],
    );
}

#[test]
fn test_rename_directory_with_external_function() {
    let files = |main: &'static str, a: &'static str| {
        [
            ("main.rue", main),
            ("lib/a.rue", a),
            ("lib/run.hex", "01"),
            ("run.hex", "01"),
        ]
    };

    let external = "extern fn run(value: Any) -> Any from \"./lib/run.hex\";\n\n";
    let main = "fn main() -> Any {\n    li$0b::a::go()\n}\n";
    let go = "export fn go() -> Any {\n    1\n}\n";

    // The path of an external function isn't rewritten, so the rename is refused if it would
    // stop leading to its file
    let outside = format!("{external}{main}");
    check(
        &files(outside.leak(), go),
        "core",
        expect![[r#"
        error: `lib` can't be renamed, since the path of the external function at main.rue:1:39 would no longer lead to its file
    "#]],
    );

    let inside = format!("extern fn run(value: Any) -> Any from \"../run.hex\";\n\n{go}");
    check(
        &files(main, inside.leak()),
        "core",
        expect![[r#"
        error: `lib` can't be renamed, since the path of the external function at a.rue:1:39 would no longer lead to its file
    "#]],
    );

    // A file that's renamed along with the function still leads to it
    let together = format!("extern fn run(value: Any) -> Any from \"./run.hex\";\n\n{go}");
    check(
        &files(main, together.leak()),
        "core",
        expect![[r#"
        main.rue:
        fn main() -> Any {
            core::a::go()
        }
        renamed: lib -> core
    "#]],
    );
}

#[cfg(unix)]
#[test]
fn test_rename_file_module_open_through_a_symlink() {
    // The file itself is renamed, rather than the symlink the editor opened it through
    let utils = "export const ONE: Int = 1;\n";
    let project = project(&[
        ("main.rue", "fn main() -> Int {\n    uti$0ls::ONE\n}\n"),
        ("utils.rue", utils),
    ]);

    let links = super::TempDir::new("rue-lsp-rename-links");
    let link = links.path().join("link.rue");
    std::os::unix::fs::symlink(project.path("utils.rue"), &link).unwrap();

    let backend = project.backend();
    let uri = crate::paths::path_to_uri(&link).unwrap();
    backend.shared.paths.open(&uri);
    backend.on_change_impl(&uri, Some(utils.to_string()));

    expect![[r#"
        main.rue:
        fn main() -> Int {
            helpers::ONE
        }
        renamed: utils.rue -> helpers.rue
    "#]]
    .assert_eq(&project.rename("helpers"));
}

#[test]
fn test_rename_file_module_conflicts() {
    let files = [
        (
            "main.rue",
            "fn main() -> Int {\n    uti$0ls::ONE + other::TWO\n}\n",
        ),
        ("utils.rue", "export const ONE: Int = 1;\n"),
        ("other.rue", "export const TWO: Int = 2;\n"),
    ];

    // Another file is already the module
    check(
        &files,
        "other",
        expect![[r#"
        error: `other` is already used in a scope that has `utils`
    "#]],
    );

    // A file that isn't part of the project is in the way
    let project = TestProject::new(&files);
    project
        .backend()
        .shared
        .rename_files
        .store(true, Ordering::Relaxed);
    std::fs::write(project.path("helpers.rue"), "").unwrap();
    expect![[r#"
        error: `helpers.rue` already exists
    "#]]
    .assert_eq(&project.rename("helpers"));
}

#[test]
fn test_rename_keyword_modules() {
    // `super` and `project` don't name the module they refer to
    check(
        &[(
            "main.rue",
            "mod inner {\n    export fn one() -> Int {\n        sup$0er::ONE\n    }\n}\n\nconst ONE: Int = 1;\n",
        )],
        "outer",
        expect![[r#"
            error: `super` is a keyword, so it can't be renamed
        "#]],
    );

    check(
        &[
            (
                "main.rue",
                "fn main() -> Int {\n    proj$0ect::utils::ONE\n}\n",
            ),
            ("utils.rue", "export const ONE: Int = 1;\n"),
        ],
        "root",
        expect![[r#"
            error: `project` is a keyword, so it can't be renamed
        "#]],
    );
}

#[test]
fn test_rename_builtins() {
    let files = |source: &'static str| [("main.rue", source)];

    check(
        &files("fn main() -> I$0nt {\n    1\n}\n"),
        "Number",
        expect![[r#"
        error: `Int` isn't declared in the project, so it can't be renamed
    "#]],
    );
    check(
        &files("fn main() -> Bytes32 {\n    sha$0256(\"a\")\n}\n"),
        "hash",
        expect![[r#"
            error: `sha256` isn't declared in the project, so it can't be renamed
        "#]],
    );
    check(
        &files("fn main(pair: (Int, Int)) -> Int {\n    pair.fir$0st\n}\n"),
        "left",
        expect![[r#"
            error: `first` isn't declared in the project, so it can't be renamed
        "#]],
    );
    check(
        &files("fn main(text: String) -> Int {\n    text.len$0gth\n}\n"),
        "size",
        expect![[r#"
            error: `length` isn't declared in the project, so it can't be renamed
        "#]],
    );
}

#[test]
fn test_rename_standard_library() {
    let check = |source: &str, expect: Expect| {
        let project = TestProject::with_manifest(
            Some("[compiler]\nentrypoint = \".\"\n"),
            &[("main.rue", source)],
        );

        expect.assert_eq(&format!(
            "{}{}",
            project.prepare_rename(),
            project.rename("other")
        ));
    };

    check(
        "fn main() -> List<Int> {\n    merge_li$0st([1, 2], [3])\n}\n",
        expect![[r#"
            error: `merge_list` is part of the standard library, so it can't be renamed
            error: `merge_list` is part of the standard library, so it can't be renamed
        "#]],
    );
    check(
        "fn main(conditions: Li$0st<Condition>) -> List<Condition> {\n    conditions\n}\n",
        expect![[r#"
            error: `List` is part of the standard library, so it can't be renamed
            error: `List` is part of the standard library, so it can't be renamed
        "#]],
    );
    check(
        "fn main(proof: MerkleProof) -> Int {\n    proof.pa$0th\n}\n",
        expect![[r#"
            error: `path` is part of the standard library, so it can't be renamed
            error: `path` is part of the standard library, so it can't be renamed
        "#]],
    );

    // A name of the project's own can be renamed to one that the standard library has, as long
    // as nothing uses that one where it would be hidden
    check(
        "fn main() -> Int {\n    let val$0ue = 1;\n    value\n}\n",
        expect![[r#"
            main.rue:1:8-1:13 `value`
            main.rue:
            fn main() -> Int {
                let other = 1;
                other
            }
        "#]],
    );
    check(
        "fn main() -> Int {\n    let val$0ue = 1;\n    value + other(2)\n}\n\nfn other(num: Int) -> Int {\n    num\n}\n",
        expect![[r#"
            main.rue:1:8-1:13 `value`
            error: Renaming `value` to `other` would change what `other` refers to at main.rue:3:13
        "#]],
    );
}

#[test]
fn test_rename_invalid_names() {
    let files = [(
        "main.rue",
        "fn main() -> Int {\n    let value = 1;\n    val$0ue\n}\n",
    )];

    for name in [
        "", "1abc", "a b", "a-b", "fn", "super", "nil", "_", "a::b", " a",
    ] {
        let project = TestProject::new(&files);
        assert_eq!(
            project.rename(name),
            format!("error: `{name}` isn't a valid name\n")
        );
    }

    // The same name changes nothing
    check(
        &files,
        "value",
        expect![[r#"
        <no changes>
    "#]],
    );
    check(
        &files,
        "_value",
        expect![[r#"
        main.rue:
        fn main() -> Int {
            let _value = 1;
            _value
        }
    "#]],
    );
}

#[test]
fn test_rename_conflicts_in_the_same_scope() {
    check(
        &[(
            "main.rue",
            "fn fi$0rst() -> Int {\n    1\n}\n\nfn second() -> Int {\n    first()\n}\n",
        )],
        "second",
        expect![[r#"
            error: `second` is already used in a scope that has `first`
        "#]],
    );

    // A value and a type can have the same name
    check(
        &[(
            "main.rue",
            "fn fi$0rst() -> Int {\n    1\n}\n\ntype second = Int;\n\nfn main(num: second) -> Int {\n    num + first()\n}\n",
        )],
        "second",
        expect![[r#"
            main.rue:
            fn second() -> Int {
                1
            }

            type second = Int;

            fn main(num: second) -> Int {
                num + second()
            }
        "#]],
    );

    check(
        &[(
            "main.rue",
            "fn add(fi$0rst: Int, second: Int) -> Int {\n    first + second\n}\n",
        )],
        "second",
        expect![[r#"
            error: `second` is already used in a scope that has `first`
        "#]],
    );

    check(
        &[(
            "main.rue",
            "fn main(pair: (Int, Int)) -> Int {\n    let (fi$0rst, second) = pair;\n    first + second\n}\n",
        )],
        "second",
        expect![[r#"
            error: `second` is already used in a scope that has `first`
        "#]],
    );

    check(
        &[(
            "main.rue",
            "type Fi$0rst = Int;\n\nstruct Second {\n    value: First,\n}\n",
        )],
        "Second",
        expect![[r#"
            error: `Second` is already used in a scope that has `First`
        "#]],
    );

    check(
        &[("main.rue", "mod fi$0rst {}\n\nmod second {}\n")],
        "second",
        expect![[r#"
            error: `second` is already used in a scope that has `first`
        "#]],
    );
}

#[test]
fn test_rename_conflicts_with_members() {
    check(
        &[(
            "main.rue",
            &with_point("\nfn main(point: Point) -> Int {\n    point.$0x\n}\n"),
        )],
        "y",
        expect![[r#"
            error: There's already a field named `y`
        "#]],
    );

    check(
        &[(
            "main.rue",
            "enum Shape {\n    Cir$0cle = 1,\n    Square = 2 { size: Int },\n}\n",
        )],
        "Square",
        expect![[r#"
            error: There's already a variant named `Square`
        "#]],
    );

    check(
        &[(
            "main.rue",
            "fn first<$0A, B>(a: A, b: B) -> A {\n    a\n}\n",
        )],
        "B",
        expect![[r#"
            error: There's already a generic parameter named `B`
        "#]],
    );

    // An enum is also the namespace of its variants, so it can't have a module's name
    check(
        &[(
            "main.rue",
            "enum Sha$0pe {\n    Circle = 1,\n}\n\nmod shapes {}\n",
        )],
        "shapes",
        expect![[r#"
            error: `shapes` is already used in a scope that has `Shape`
        "#]],
    );

    // And a variant without fields is also a constant
    check(
        &[(
            "main.rue",
            "enum Shape {\n    Cir$0cle = 1,\n}\n\nimport Shape::Circle;\n\nconst ROUND: Int = 1;\n",
        )],
        "ROUND",
        expect![[r#"
            error: `ROUND` is already used in a scope that has `Circle`
        "#]],
    );
}

#[test]
fn test_rename_that_would_be_shadowed() {
    // The reference would find the inner binding instead
    check(
        &[(
            "main.rue",
            r#"
fn main() -> Int {
    let fi$0rst = 1;
    let second = 2;
    first + second
}
"#,
        )],
        "second",
        expect![[r#"
            error: Renaming `first` to `second` would change what `second` refers to at main.rue:5:5
        "#]],
    );

    check(
        &[(
            "main.rue",
            "const FI$0RST: Int = 1;\n\nfn main(second: Int) -> Int {\n    FIRST + second\n}\n",
        )],
        "second",
        expect![[r#"
            error: Renaming `FIRST` to `second` would change what `second` refers to at main.rue:4:5
        "#]],
    );
}

#[test]
fn test_rename_that_would_shadow() {
    // The reference to the outer binding would find the renamed one instead
    check(
        &[(
            "main.rue",
            r#"
fn main() -> Int {
    let first = 1;
    let sec$0ond = 2;
    first + second
}
"#,
        )],
        "first",
        expect![[r#"
            error: Renaming `second` to `first` would change what `first` refers to at main.rue:5:5
        "#]],
    );

    check(
        &[(
            "main.rue",
            "const FIRST: Int = 1;\n\nfn main(sec$0ond: Int) -> Int {\n    FIRST + second\n}\n",
        )],
        "FIRST",
        expect![[r#"
            error: Renaming `second` to `FIRST` would change what `FIRST` refers to at main.rue:4:5
        "#]],
    );

    // A shorthand field refers to the binding with its name
    check(
        &[(
            "main.rue",
            &with_point(
                "\nfn main(x: Int) -> Point {\n    let oth$0er = 2;\n    Point { x, y: other }\n}\n",
            ),
        )],
        "x",
        expect![[r#"
            error: Renaming `other` to `x` would change what `x` refers to at main.rue:8:13
        "#]],
    );
}

#[test]
fn test_rename_to_a_shadowed_name_that_is_not_used() {
    // Nothing refers to the outer name where the renamed one hides it
    check(
        &[(
            "main.rue",
            r#"
const FIRST: Int = 1;

fn main(sec$0ond: Int) -> Int {
    second + second
}

fn other() -> Int {
    FIRST
}
"#,
        )],
        "FIRST",
        expect![[r#"
            main.rue:

            const FIRST: Int = 1;

            fn main(FIRST: Int) -> Int {
                FIRST + FIRST
            }

            fn other() -> Int {
                FIRST
            }
        "#]],
    );

    check(
        &[(
            "main.rue",
            r#"
fn main() -> Int {
    let first = 1;
    let result = first + 1;
    let sec$0ond = 2;
    result + second
}
"#,
        )],
        "first",
        expect![[r#"
            main.rue:

            fn main() -> Int {
                let first = 1;
                let result = first + 1;
                let first = 2;
                result + first
            }
        "#]],
    );
}

#[test]
fn test_rename_to_a_name_that_only_other_scopes_have() {
    // The scope that has the new name isn't the one that the renamed binding is in, though it
    // has another binding with the old name
    check(
        &[(
            "main.rue",
            r#"
fn main(value: Int, other: Int) -> Int {
    let inner = {
        let value = 2;
        val$0ue + 1
    };
    value + inner + other
}
"#,
        )],
        "other",
        expect![[r#"
            main.rue:

            fn main(value: Int, other: Int) -> Int {
                let inner = {
                    let other = 2;
                    other + 1
                };
                value + inner + other
            }
        "#]],
    );
}

#[test]
fn test_rename_conflicts_with_an_import() {
    // The name is imported into a file that already has the new one
    let files = |main: &'static str| {
        [
            ("main.rue", main),
            ("utils.rue", "export fn gre$0et() -> Int {\n    1\n}\n"),
        ]
    };

    check(
        &files("import utils::greet;\n\nfn hello() -> Int {\n    greet()\n}\n"),
        "hello",
        expect![[r#"
            error: `hello` is already used in a scope that has `greet`
        "#]],
    );

    check(
        &files("import utils::*;\n\nfn hello() -> Int {\n    greet()\n}\n"),
        "hello",
        expect![[r#"
            error: `hello` is already used in a scope that has `greet`
        "#]],
    );

    // A file that only names it through its module isn't affected
    check(
        &files("fn hello() -> Int {\n    utils::greet()\n}\n"),
        "hello",
        expect![[r#"
            main.rue:
            fn hello() -> Int {
                utils::hello()
            }
            utils.rue:
            export fn hello() -> Int {
                1
            }
        "#]],
    );
}

#[test]
fn test_rename_with_syntax_errors() {
    // The parts that still parse are renamed
    check_with_errors(
        &[(
            "main.rue",
            "fn main() -> Int {\n    let value = 42;\n    let other = value +;\n    val$0ue\n",
        )],
        "number",
        expect![[r#"
            main.rue:
            fn main() -> Int {
                let number = 42;
                let other = number +;
                number
        "#]],
    );

    check_with_errors(
        &[
            (
                "main.rue",
                "import utils::greet;\n\nfn main() -> Int {\n    gre$0et() +\n}\n",
            ),
            (
                "utils.rue",
                "export fn greet() -> Int {\n    let = ;\n    1\n}\n\nfn other( {\n    greet()\n",
            ),
        ],
        "hello",
        expect![[r#"
            main.rue:
            import utils::hello;

            fn main() -> Int {
                hello() +
            }
            utils.rue:
            export fn hello() -> Int {
                let = ;
                1
            }

            fn other( {
                hello()
        "#]],
    );

    // Nothing is renamed where there's no name
    check_with_errors(
        &[("main.rue", "fn main() -> Int {\n    let = $0;\n")],
        "number",
        expect![[r#"
            <none>
        "#]],
    );
}

#[test]
fn test_rename_after_non_ascii() {
    // Positions are in UTF-16 code units
    check(
        &[(
            "main.rue",
            "fn main() -> String {\n    let value = \"a\";\n    \"\u{1F600}\u{e9}\" + value + \"\u{1F600}\" + val$0ue\n}\n",
        )],
        "text",
        expect![[r#"
            main.rue:
            fn main() -> String {
                let text = "a";
                "😀é" + text + "😀" + text
            }
        "#]],
    );
}

#[test]
fn test_rename_recursive_and_test_functions() {
    check(
        &[(
            "main.rue",
            "fn count(num: Int) -> Int {\n    if num == 0 { 0 } else { cou$0nt(num - 1) + 1 }\n}\n\ntest fn counts() {\n    assert count(2) == 2;\n}\n",
        )],
        "total",
        expect![[r#"
            main.rue:
            fn total(num: Int) -> Int {
                if num == 0 { 0 } else { total(num - 1) + 1 }
            }

            test fn counts() {
                assert total(2) == 2;
            }
        "#]],
    );
}
