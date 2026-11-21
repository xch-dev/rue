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

    // A directory that's named `main` isn't what the project is built from
    check(
        &[
            (
                "other.rue",
                "export fn go() -> Int {\n    ma$0in::a::ONE\n}\n",
            ),
            ("main/a.rue", "export const ONE: Int = 1;\n"),
        ],
        "start",
        expect![[r#"
            other.rue:
            export fn go() -> Int {
                start::a::ONE
            }
            renamed: main -> start
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
    // The new name is the same file on a file system that ignores case, which is the only kind
    // of file system that this would be refused on
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
    // The path of an external function isn't rewritten, so the rename is refused if a part of
    // the path names the directory, and allowed if the path leads to the same file afterwards
    let renamed = |path: &str, inside: bool| {
        let external = format!("extern fn run(value: Any) -> Any from \"{path}\";\n\n");
        let main = "fn main() -> Any {\n    li$0b::a::go()\n}\n";
        let go = "export fn go() -> Any {\n    1\n}\n";

        let (main, a) = if inside {
            (main.to_string(), format!("{external}{go}"))
        } else {
            (format!("{external}{main}"), go.to_string())
        };

        let project = project(&[
            ("main.rue", &main),
            ("lib/a.rue", &a),
            ("lib/sub/b.rue", "export const ONE: Int = 1;\n"),
            ("lib/run.hex", "01"),
            ("run.hex", "01"),
        ]);

        let diagnostics = project.diagnostics();
        assert!(!diagnostics.contains(" Error"), "{diagnostics}");

        project.rename("core").lines().last().unwrap().to_string()
    };

    let refused = |place: &str| {
        format!(
            "error: `lib` can't be renamed, since the path of the external function at {place} \
            leads into it"
        )
    };

    // From outside of the directory, into it
    assert_eq!(renamed("./lib/run.hex", false), refused("main.rue:1:39"));
    assert_eq!(renamed("lib/../run.hex", false), refused("main.rue:1:39"));

    // From inside of it, out and back in
    assert_eq!(renamed("../lib/run.hex", true), refused("lib/a.rue:1:39"));

    // A path that stays inside of the directory, or only leaves it, still leads to its file
    assert_eq!(renamed("./run.hex", true), "renamed: lib -> core");
    assert_eq!(renamed("sub/../run.hex", true), "renamed: lib -> core");
    assert_eq!(renamed("../run.hex", true), "renamed: lib -> core");
    assert_eq!(renamed("./run.hex", false), "renamed: lib -> core");
}

#[cfg(unix)]
#[test]
fn test_rename_module_that_is_a_symlink() {
    // The file or directory that a symbolic link leads to isn't where the project has the
    // module, so neither it nor the link is renamed, whatever the link is named
    let elsewhere = super::TempDir::new("rue-lsp-rename-elsewhere");
    let real = elsewhere.path().canonicalize().unwrap();

    for (path, text) in [
        ("utils.rue", "export const ONE: Int = 1;\n"),
        ("impl.rue", "export const TWO: Int = 2;\n"),
        ("only.rue", "export const THREE: Int = 3;\n"),
        ("shared/a.rue", "export const FOUR: Int = 4;\n"),
    ] {
        std::fs::create_dir_all(real.join(path).parent().unwrap()).unwrap();
        std::fs::write(real.join(path), text).unwrap();
    }

    let main = "fn main() -> Int {\n    utils::ONE + other::TWO + real::only::THREE + shared::a::FOUR + plain::FIVE\n}\n";

    let renamed = |cursor: &str, new_name: &str| {
        let main = main.replacen(cursor, &format!("$0{cursor}"), 1);
        let files = [
            ("main.rue", main.as_str()),
            ("plain.rue", "export const FIVE: Int = 5;\n"),
        ];

        // The links have to be there before the project is compiled: a file and a directory
        // that are named like what they lead to, a file that isn't, and a file that's the only
        // one in a directory of the project
        let dir = super::TempDir::new("rue-lsp-rename-links");
        let link = |from: &str, to: &str| {
            std::os::unix::fs::symlink(real.join(from), dir.path().join(to)).unwrap();
        };

        std::fs::write(dir.path().join("Rue.toml"), super::MANIFEST).unwrap();
        std::fs::create_dir(dir.path().join("real")).unwrap();
        link("utils.rue", "utils.rue");
        link("impl.rue", "other.rue");
        link("only.rue", "real/only.rue");
        link("shared", "shared");

        let project = TestProject::in_dir(dir, &files);
        project
            .backend()
            .shared
            .rename_files
            .store(true, Ordering::Relaxed);

        let diagnostics = project.diagnostics();
        assert!(!diagnostics.contains(" Error"), "{diagnostics}");

        project.rename(new_name)
    };

    for (cursor, name) in [
        ("utils", "utils"),
        ("other", "other"),
        ("only", "only"),
        ("shared", "shared"),
        ("a::FOUR", "a"),
    ] {
        assert_eq!(
            renamed(cursor, "renamed"),
            format!("error: `{name}` is reached through a symbolic link, so it can't be renamed\n")
        );
    }

    // A file and a directory that are where they seem to be are still renamed
    expect![[r#"
        main.rue:
        fn main() -> Int {
            utils::ONE + other::TWO + real::only::THREE + shared::a::FOUR + simple::FIVE
        }
        renamed: plain.rue -> simple.rue
    "#]]
    .assert_eq(&renamed("plain", "simple"));
    expect![[r#"
        main.rue:
        fn main() -> Int {
            utils::ONE + other::TWO + actual::only::THREE + shared::a::FOUR + plain::FIVE
        }
        renamed: real -> actual
    "#]]
    .assert_eq(&renamed("real", "actual"));
}

#[test]
fn test_rename_refused_from_a_file_in_a_directory() {
    // The place that's in the way is shown in the project, wherever the rename is asked for
    let files = [
        (
            "main.rue",
            "extern fn run(value: Any) -> Any from \"lib/run.hex\";\n\nfn main(other: Int) -> Int {\n    lib::a::go(other)\n}\n",
        ),
        (
            "lib/a.rue",
            "export fn go(value: Int) -> Int {\n    let other = super::l$0ib::b::ONE;\n    value + other\n}\n",
        ),
        ("lib/b.rue", "export const ONE: Int = 1;\n"),
        ("lib/run.hex", "01"),
    ];

    check(
        &files,
        "core",
        expect![[r#"
            error: `lib` can't be renamed, since the path of the external function at main.rue:1:39 leads into it
        "#]],
    );

    let files = [
        (
            "main.rue",
            "fn main(other: Int) -> Int {\n    lib::a::go(other) + lib::b::go(other)\n}\n",
        ),
        (
            "lib/a.rue",
            "export fn go(value: Int) -> Int {\n    let oth$0er = 1;\n    value + other\n}\n",
        ),
        (
            "lib/b.rue",
            "export fn go(value: Int) -> Int {\n    value\n}\n",
        ),
    ];

    check(
        &files,
        "value",
        expect![[r#"
        error: Renaming `other` to `value` would change what `value` refers to at lib/a.rue:3:5
    "#]],
    );
}

#[cfg(unix)]
#[test]
fn test_rename_with_symlinks_inside_the_project() {
    // A file that's linked to from the project it's in is a module for each of its names, and a
    // link that leads into a directory is left leading nowhere if the directory is renamed
    let main = "extern fn run(value: Any) -> Any from \"hexes/run.hex\";\n\nfn main() -> Int {\n    utils::ONE + alias::ONE + lib::a::TWO + nested::THREE + plain::data::b::FOUR\n}\n";

    let renamed = |cursor: &str, new_name: &str| {
        let main = main.replacen(cursor, &format!("$0{cursor}"), 1);

        let dir = super::TempDir::new("rue-lsp-rename-inner-links");
        let real = dir.path().canonicalize().unwrap();
        std::fs::write(real.join("Rue.toml"), super::MANIFEST).unwrap();

        for (path, text) in [
            ("utils.rue", "export const ONE: Int = 1;\n"),
            ("lib/a.rue", "export const TWO: Int = 2;\n"),
            ("lib/run.hex", "01"),
            ("plain/data/b.rue", "export const FOUR: Int = 4;\n"),
            ("plain/data/run.hex", "01"),
        ] {
            std::fs::create_dir_all(real.join(path).parent().unwrap()).unwrap();
            std::fs::write(real.join(path), text).unwrap();
        }

        let link = |from: &str, to: &str| {
            std::os::unix::fs::symlink(real.join(from), real.join(to)).unwrap();
        };

        link("utils.rue", "alias.rue");
        link("lib/a.rue", "nested.rue");
        link("plain/data", "hexes");

        // `nested` is the same file as `lib::a`, whose constant it names differently
        let main = main.replace("nested::THREE", "nested::TWO");

        let project = TestProject::in_dir(dir, &[("main.rue", main.as_str())]);
        project
            .backend()
            .shared
            .rename_files
            .store(true, Ordering::Relaxed);

        let diagnostics = project.diagnostics();
        assert!(!diagnostics.contains(" Error"), "{diagnostics}");

        project.rename(new_name)
    };

    expect![[r#"
        error: `ONE` is used in a file that's in the project more than once, through a symbolic link, so it can't be renamed
    "#]].assert_eq(&renamed("ONE", "FIRST"));
    expect![[r#"
        error: `utils` can't be renamed, since the symbolic link at `alias.rue` leads to it
    "#]]
    .assert_eq(&renamed("utils", "helpers"));
    expect![[r#"
        error: `lib` can't be renamed, since the symbolic link at `nested.rue` leads to it
    "#]]
    .assert_eq(&renamed("lib", "core"));
    expect![[r#"
        error: `plain` can't be renamed, since the symbolic link at `hexes/b.rue` leads to it
    "#]]
    .assert_eq(&renamed("plain", "simple"));
}

#[test]
fn test_rename_module_with_a_doubled_extension() {
    // The file isn't the one that the module's name gives, so there's nothing to rename
    check(
        &[
            ("main.rue", "fn main() -> Int {\n    uti$0ls::ONE\n}\n"),
            ("utils.rue.rue", "export const ONE: Int = 1;\n"),
        ],
        "helpers",
        expect![[r#"
            error: `utils` isn't where it was when the project was compiled
        "#]],
    );
}

#[test]
fn test_rename_directory_without_source_files() {
    // A directory is a module even if none of the files in it are
    check(
        &[
            (
                "main.rue",
                "import hex$0es::*;\n\nfn main() -> Int {\n    1\n}\n",
            ),
            ("hexes/run.hex", "01"),
        ],
        "programs",
        expect![[r#"
            main.rue:
            import programs::*;

            fn main() -> Int {
                1
            }
            renamed: hexes -> programs
        "#]],
    );
}

#[test]
fn test_rename_directory_with_external_function_spelled_differently() {
    // On a file system that ignores case, a path can name the directory under another spelling
    let project = project(&[
        (
            "main.rue",
            "extern fn run(value: Any) -> Any from \"Lib/run.hex\";\n\nfn main() -> Any {\n    li$0b::a::ONE\n}\n",
        ),
        ("lib/a.rue", "export const ONE: Int = 1;\n"),
        ("lib/run.hex", "01"),
    ]);

    if !project.path("Lib").exists() {
        return;
    }

    assert_eq!(
        project.rename("core"),
        "error: `lib` can't be renamed, since the path of the external function at main.rue:1:39 \
        leads into it\n"
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

    // A name that's found through another module isn't looked up where the renamed one is
    check(
        &[
            (
                "main.rue",
                "import utils::greet;\n\nfn main() -> Int {\n    greet() + other::hello()\n}\n",
            ),
            ("utils.rue", "export fn gre$0et() -> Int {\n    1\n}\n"),
            ("other.rue", "export fn hello() -> Int {\n    2\n}\n"),
        ],
        "hello",
        expect![[r#"
            main.rue:
            import utils::hello;

            fn main() -> Int {
                hello() + other::hello()
            }
            utils.rue:
            export fn hello() -> Int {
                1
            }
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

#[test]
fn test_rename_in_assert_message() {
    // A name in the message of an `assert` is renamed like any other
    check(
        &[(
            "main.rue",
            "fn main(value: Int) -> Int {\n    let text = \"too small\";\n    assert value > 1, te$0xt;\n    value\n}\n",
        )],
        "message",
        expect![[r#"
            main.rue:
            fn main(value: Int) -> Int {
                let message = "too small";
                assert value > 1, message;
                value
            }
        "#]],
    );

    check(
        &[(
            "main.rue",
            "fn main(value: Int) -> Int {\n    let text = \"too small\";\n    assert val$0ue > 1, text;\n    value\n}\n",
        )],
        "number",
        expect![[r#"
            main.rue:
            fn main(number: Int) -> Int {
                let text = "too small";
                assert number > 1, text;
                number
            }
        "#]],
    );
}

#[test]
fn test_rename_in_index_and_slice() {
    // A name that's indexed or sliced, or is a bound of one, is renamed like any other
    check(
        &[(
            "main.rue",
            "fn main(bytes: Bytes, start: Int) -> Bytes {\n    let first = bytes[sta$0rt];\n    bytes[start..] + bytes[..start] + first\n}\n",
        )],
        "offset",
        expect![[r#"
            main.rue:
            fn main(bytes: Bytes, offset: Int) -> Bytes {
                let first = bytes[offset];
                bytes[offset..] + bytes[..offset] + first
            }
        "#]],
    );

    check(
        &[(
            "main.rue",
            "fn main(bytes: Bytes, start: Int) -> Bytes {\n    let first = by$0tes[start];\n    bytes[start..] + bytes[..start] + first\n}\n",
        )],
        "data",
        expect![[r#"
            main.rue:
            fn main(data: Bytes, start: Int) -> Bytes {
                let first = data[start];
                data[start..] + data[..start] + first
            }
        "#]],
    );
}

const SHAPE: &str = r#"
enum Shape {
    Empty,
    Circle { radius: Int },
    Rect { width: Int, height: Int },
}
"#;

fn with_shape(source: &str) -> String {
    format!("{SHAPE}{source}")
}

#[test]
fn test_rename_field_in_match_arm_binding() {
    // The arm's shorthand keeps the binding's name, so the body is untouched, and a field that's
    // named for its own binding is only renamed
    let source = with_shape(
        r#"
fn main(shape: Shape) -> Int {
    match shape {
        Shape::Empty => 0,
        Shape::Circle { radius } => radius * radius,
        Shape::Rect { width: w, height } => w * height + shape.width,
    }
}
"#,
    );

    check(
        &[(
            "main.rue",
            &source.replacen("radius: Int", "rad$0ius: Int", 1),
        )],
        "size",
        expect![[r#"
            main.rue:

            enum Shape {
                Empty,
                Circle { size: Int },
                Rect { width: Int, height: Int },
            }

            fn main(shape: Shape) -> Int {
                match shape {
                    Shape::Empty => 0,
                    Shape::Circle { size: radius } => radius * radius,
                    Shape::Rect { width: w, height } => w * height + shape.width,
                }
            }
        "#]],
    );

    check(
        &[(
            "main.rue",
            &source.replacen("width: Int", "wid$0th: Int", 1),
        )],
        "across",
        expect![[r#"
            main.rue:

            enum Shape {
                Empty,
                Circle { radius: Int },
                Rect { across: Int, height: Int },
            }

            fn main(shape: Shape) -> Int {
                match shape {
                    Shape::Empty => 0,
                    Shape::Circle { radius } => radius * radius,
                    Shape::Rect { across: w, height } => w * height + shape.across,
                }
            }
        "#]],
    );

    // From the field's name in the arm
    check(
        &[("main.rue", &source.replacen("width: w", "wid$0th: w", 1))],
        "across",
        expect![[r#"
            main.rue:

            enum Shape {
                Empty,
                Circle { radius: Int },
                Rect { across: Int, height: Int },
            }

            fn main(shape: Shape) -> Int {
                match shape {
                    Shape::Empty => 0,
                    Shape::Circle { radius } => radius * radius,
                    Shape::Rect { across: w, height } => w * height + shape.across,
                }
            }
        "#]],
    );
}

#[test]
fn test_rename_binding_in_match_arm() {
    let source = with_shape(
        r#"
fn main(shape: Shape) -> Int {
    match shape {
        Shape::Empty => 0,
        Shape::Circle { radius } => radius * radius,
        Shape::Rect { width: w, height } => w * height,
    }
}
"#,
    );

    // From a use in the arm's value, the shorthand is expanded to keep the field's name
    check(
        &[(
            "main.rue",
            &source.replacen("radius * radius", "rad$0ius * radius", 1),
        )],
        "size",
        expect![[r#"
            main.rue:

            enum Shape {
                Empty,
                Circle { radius: Int },
                Rect { width: Int, height: Int },
            }

            fn main(shape: Shape) -> Int {
                match shape {
                    Shape::Empty => 0,
                    Shape::Circle { radius: size } => size * size,
                    Shape::Rect { width: w, height } => w * height,
                }
            }
        "#]],
    );

    // A binding with a name of its own is only renamed, from its use or where it's declared
    check(
        &[(
            "main.rue",
            &source.replacen("w * height", "$0w * height", 1),
        )],
        "across",
        expect![[r#"
            main.rue:

            enum Shape {
                Empty,
                Circle { radius: Int },
                Rect { width: Int, height: Int },
            }

            fn main(shape: Shape) -> Int {
                match shape {
                    Shape::Empty => 0,
                    Shape::Circle { radius } => radius * radius,
                    Shape::Rect { width: across, height } => across * height,
                }
            }
        "#]],
    );

    check(
        &[("main.rue", &source.replacen("width: w", "width: $0w", 1))],
        "across",
        expect![[r#"
            main.rue:

            enum Shape {
                Empty,
                Circle { radius: Int },
                Rect { width: Int, height: Int },
            }

            fn main(shape: Shape) -> Int {
                match shape {
                    Shape::Empty => 0,
                    Shape::Circle { radius } => radius * radius,
                    Shape::Rect { width: across, height } => across * height,
                }
            }
        "#]],
    );
}

#[test]
fn test_rename_pair_and_list_bindings_in_match_arms() {
    let source = r#"
fn main(value: (Int, Int) | List<Bytes>) -> Int {
    match value {
        (Int, Int) as (first, rest) => first + rest,
        nil => 0,
        (Bytes, List<Bytes>) as [head, ...tail] => head.length + main(tail),
    }
}
"#;

    check(
        &[(
            "main.rue",
            &source.replacen("first + rest", "first + re$0st", 1),
        )],
        "second",
        expect![[r#"
            main.rue:

            fn main(value: (Int, Int) | List<Bytes>) -> Int {
                match value {
                    (Int, Int) as (first, second) => first + second,
                    nil => 0,
                    (Bytes, List<Bytes>) as [head, ...tail] => head.length + main(tail),
                }
            }
        "#]],
    );

    check(
        &[("main.rue", &source.replacen("...tail", "...ta$0il", 1))],
        "others",
        expect![[r#"
            main.rue:

            fn main(value: (Int, Int) | List<Bytes>) -> Int {
                match value {
                    (Int, Int) as (first, rest) => first + rest,
                    nil => 0,
                    (Bytes, List<Bytes>) as [head, ...others] => head.length + main(others),
                }
            }
        "#]],
    );
}

#[test]
fn test_rename_match_arm_binding_conflicts() {
    let source = with_shape(
        r#"
fn main(shape: Shape, scale: Int) -> Int {
    match shape {
        Shape::Empty => scale,
        Shape::Circle { radius } => radius * scale,
        Shape::Rect { width, height } => width * height * scale,
    }
}
"#,
    );

    // A name that the arm's value uses for something else
    check(
        &[(
            "main.rue",
            &source.replacen("radius * scale", "rad$0ius * scale", 1),
        )],
        "scale",
        expect![[r#"
            error: Renaming `radius` to `scale` would change what `scale` refers to at main.rue:11:46
        "#]],
    );

    // A name that's bound in the same arm
    check(
        &[(
            "main.rue",
            &source.replacen("width * height", "wid$0th * height", 1),
        )],
        "height",
        expect![[r#"
            error: `height` is already used in a scope that has `width`
        "#]],
    );

    // A name that's only bound in another arm is free
    check(
        &[(
            "main.rue",
            &source.replacen("radius * scale", "rad$0ius * scale", 1),
        )],
        "width",
        expect![[r#"
            main.rue:

            enum Shape {
                Empty,
                Circle { radius: Int },
                Rect { width: Int, height: Int },
            }

            fn main(shape: Shape, scale: Int) -> Int {
                match shape {
                    Shape::Empty => scale,
                    Shape::Circle { radius: width } => width * scale,
                    Shape::Rect { width, height } => width * height * scale,
                }
            }
        "#]],
    );

    // The value being matched on can be shadowed if the arm doesn't use it
    check(
        &[(
            "main.rue",
            &source.replacen("radius * scale", "rad$0ius * scale", 1),
        )],
        "shape",
        expect![[r#"
            main.rue:

            enum Shape {
                Empty,
                Circle { radius: Int },
                Rect { width: Int, height: Int },
            }

            fn main(shape: Shape, scale: Int) -> Int {
                match shape {
                    Shape::Empty => scale,
                    Shape::Circle { radius: shape } => shape * scale,
                    Shape::Rect { width, height } => width * height * scale,
                }
            }
        "#]],
    );
}

#[test]
fn test_rename_with_unfinished_match_arm_binding() {
    check_with_errors(
        &[(
            "main.rue",
            &with_shape(
                "\nfn main(shape: Shape, scale: Int) -> Int {\n    match shape {\n        Shape::Circle { radius } => radius * sca$0le,\n        Shape::Rect { width =>\n",
            ),
        )],
        "factor",
        expect![[r#"
            main.rue:

            enum Shape {
                Empty,
                Circle { radius: Int },
                Rect { width: Int, height: Int },
            }

            fn main(shape: Shape, factor: Int) -> Int {
                match shape {
                    Shape::Circle { radius } => radius * factor,
                    Shape::Rect { width =>
        "#]],
    );

    check_with_errors(
        &[(
            "main.rue",
            &with_shape(
                "\nfn main(shape: Shape) -> Int {\n    match shape {\n        Shape::Rect { wid$0th =>\n",
            ),
        )],
        "across",
        expect![[r#"
            main.rue:

            enum Shape {
                Empty,
                Circle { radius: Int },
                Rect { width: Int, height: Int },
            }

            fn main(shape: Shape) -> Int {
                match shape {
                    Shape::Rect { width: across =>
        "#]],
    );
}

#[test]
fn test_rename_named_match_arm_binding() {
    let source = with_shape(
        r#"
fn main(shape: Shape) -> Int {
    match shape {
        Shape::Circle as circle => circle.radius,
        Shape::Rect as rect => rect.width * rect.height,
        _ => 0,
    }
}
"#,
    );

    // From where it's declared, and from a use
    check(
        &[(
            "main.rue",
            &source.replacen("Shape::Circle as circle", "Shape::Circle as cir$0cle", 1),
        )],
        "round",
        expect![[r#"
            main.rue:

            enum Shape {
                Empty,
                Circle { radius: Int },
                Rect { width: Int, height: Int },
            }

            fn main(shape: Shape) -> Int {
                match shape {
                    Shape::Circle as round => round.radius,
                    Shape::Rect as rect => rect.width * rect.height,
                    _ => 0,
                }
            }
        "#]],
    );

    check(
        &[(
            "main.rue",
            &source.replacen("rect.width", "re$0ct.width", 1),
        )],
        "sides",
        expect![[r#"
            main.rue:

            enum Shape {
                Empty,
                Circle { radius: Int },
                Rect { width: Int, height: Int },
            }

            fn main(shape: Shape) -> Int {
                match shape {
                    Shape::Circle as circle => circle.radius,
                    Shape::Rect as sides => sides.width * sides.height,
                    _ => 0,
                }
            }
        "#]],
    );

    // The type before the name is renamed on its own
    check(
        &[(
            "main.rue",
            &source.replacen("Shape::Rect as rect", "Shape::Re$0ct as rect", 1),
        )],
        "Box",
        expect![[r#"
            main.rue:

            enum Shape {
                Empty,
                Circle { radius: Int },
                Box { width: Int, height: Int },
            }

            fn main(shape: Shape) -> Int {
                match shape {
                    Shape::Circle as circle => circle.radius,
                    Shape::Box as rect => rect.width * rect.height,
                    _ => 0,
                }
            }
        "#]],
    );
}

#[test]
fn test_rename_match_arm_binding_in_condition() {
    let source = with_shape(
        r#"
fn main(shape: Shape, limit: Int) -> Int {
    match shape {
        Shape::Circle { radius } if radius > limit => radius,
        Shape::Circle { radius } => radius + limit,
        Shape::Rect as rect if rect.width > rect.height => rect.width,
        _ => 0,
    }
}
"#,
    );

    // A binding that's renamed from the condition is renamed in its own arm only, and its
    // shorthand is expanded
    check(
        &[(
            "main.rue",
            &source.replacen("if radius > limit", "if rad$0ius > limit", 1),
        )],
        "size",
        expect![[r#"
            main.rue:

            enum Shape {
                Empty,
                Circle { radius: Int },
                Rect { width: Int, height: Int },
            }

            fn main(shape: Shape, limit: Int) -> Int {
                match shape {
                    Shape::Circle { radius: size } if size > limit => size,
                    Shape::Circle { radius } => radius + limit,
                    Shape::Rect as rect if rect.width > rect.height => rect.width,
                    _ => 0,
                }
            }
        "#]],
    );

    // A field that's renamed from the condition keeps the bindings' names
    check(
        &[(
            "main.rue",
            &source.replacen("rect.width > rect.height", "rect.wid$0th > rect.height", 1),
        )],
        "span",
        expect![[r#"
            main.rue:

            enum Shape {
                Empty,
                Circle { radius: Int },
                Rect { span: Int, height: Int },
            }

            fn main(shape: Shape, limit: Int) -> Int {
                match shape {
                    Shape::Circle { radius } if radius > limit => radius,
                    Shape::Circle { radius } => radius + limit,
                    Shape::Rect as rect if rect.span > rect.height => rect.span,
                    _ => 0,
                }
            }
        "#]],
    );

    // A name that's used in the condition is taken
    check(
        &[(
            "main.rue",
            &source.replacen(
                "Shape::Circle { radius } if",
                "Shape::Circle { rad$0ius } if",
                1,
            ),
        )],
        "limit",
        expect![[r#"
            error: Renaming `radius` to `limit` would change what `limit` refers to at main.rue:10:46
        "#]],
    );
}
