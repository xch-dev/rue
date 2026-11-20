use expect_test::{Expect, expect};

use super::TestProject;

#[allow(clippy::needless_pass_by_value)]
fn check_definitions(files: &[(&str, &str)], expect: Expect) {
    let project = TestProject::new(files);
    expect.assert_eq(&project.definitions());
}

#[allow(clippy::needless_pass_by_value)]
fn check_references(files: &[(&str, &str)], expect: Expect) {
    let project = TestProject::new(files);
    expect.assert_eq(&project.references());
}

#[allow(clippy::needless_pass_by_value)]
fn check_references_without_declaration(files: &[(&str, &str)], expect: Expect) {
    let project = TestProject::new(files);
    expect.assert_eq(&project.references_with(false));
}

#[test]
fn test_definition_function() {
    check_definitions(
        &[(
            "main.rue",
            r#"
fn main() -> Int {
    dou$0ble(21)
}

fn double(num: Int) -> Int {
    num * 2
}
"#,
        )],
        expect![[r#"
            main.rue:5:3-5:9
        "#]],
    );
}

#[test]
fn test_definition_binding() {
    check_definitions(
        &[(
            "main.rue",
            r#"
fn main() -> Int {
    let value = 42;
    value + val$0ue
}
"#,
        )],
        expect![[r#"
            main.rue:2:8-2:13
        "#]],
    );
}

#[test]
fn test_definition_type() {
    check_definitions(
        &[(
            "main.rue",
            r#"
type Amount = Int;

fn main(amount: Amo$0unt) -> Int {
    amount
}
"#,
        )],
        expect![[r#"
            main.rue:1:5-1:11
        "#]],
    );
}

#[test]
fn test_definition_field() {
    check_definitions(
        &[(
            "main.rue",
            r#"
struct Point {
    x: Int,
    y: Int,
}

fn main(point: Point) -> Int {
    point.$0y
}
"#,
        )],
        expect![[r#"
            main.rue:3:4-3:5
        "#]],
    );
}

#[test]
fn test_definition_across_files() {
    check_definitions(
        &[
            (
                "main.rue",
                r#"
import utils::greet;

fn main() -> String {
    gre$0et("world")
}
"#,
            ),
            (
                "utils.rue",
                r#"
export fn greet(name: String) -> String {
    "Hello, " + name + "!"
}
"#,
            ),
        ],
        expect![[r#"
            utils.rue:1:10-1:15
        "#]],
    );
}

#[test]
fn test_definition_through_project() {
    // A path through `project` reaches the same items as one through the files' names
    check_definitions(
        &[
            (
                "main.rue",
                r#"
mod inner {
    export fn value() -> Int {
        project::utils::ON$0E
    }
}
"#,
            ),
            ("utils.rue", "export const ONE: Int = 1;\n"),
        ],
        expect![[r#"
            utils.rue:0:13-0:16
        "#]],
    );
    check_definitions(
        &[
            ("main.rue", "import project::util$0s::ONE;\n"),
            ("utils.rue", "export const ONE: Int = 1;\n"),
        ],
        expect![[r#"
            utils.rue:0:0-1:0
        "#]],
    );
}

#[test]
fn test_definition_file_module() {
    check_definitions(
        &[
            (
                "main.rue",
                r#"
import uti$0ls::greet;

fn main() -> String {
    greet("world")
}
"#,
            ),
            (
                "utils.rue",
                r#"
export fn greet(name: String) -> String {
    "Hello, " + name + "!"
}
"#,
            ),
        ],
        expect![[r#"
            utils.rue:0:0-4:0
        "#]],
    );
}

#[test]
fn test_definition_on_declaration() {
    // Going to the definition of a declaration lists its references instead.
    check_definitions(
        &[(
            "main.rue",
            r#"
fn main() -> Int {
    double(double(1))
}

fn dou$0ble(num: Int) -> Int {
    num * 2
}
"#,
        )],
        expect![[r#"
            main.rue:2:4-2:10
            main.rue:2:11-2:17
        "#]],
    );
}

#[test]
fn test_definition_nothing() {
    check_definitions(
        &[(
            "main.rue",
            r#"
fn main() -> Int {
    4$02
}
"#,
        )],
        expect![[r#"
            <none>
        "#]],
    );
}

#[test]
fn test_references_function() {
    check_references(
        &[(
            "main.rue",
            r#"
fn main() -> Int {
    double(double(1))
}

fn dou$0ble(num: Int) -> Int {
    num * 2
}
"#,
        )],
        expect![[r#"
            main.rue:2:4-2:10
            main.rue:2:11-2:17
            main.rue:5:3-5:9
        "#]],
    );
}

#[test]
fn test_references_type() {
    check_references(
        &[(
            "main.rue",
            r#"
type Amount = Int;

fn main(a: Amo$0unt, b: Amount) -> Amount {
    a + b
}
"#,
        )],
        expect![[r#"
            main.rue:1:5-1:11
            main.rue:3:11-3:17
            main.rue:3:22-3:28
            main.rue:3:33-3:39
        "#]],
    );
}

#[test]
fn test_references_field() {
    check_references(
        &[(
            "main.rue",
            r#"
struct Point {
    $0x: Int,
    y: Int,
}

fn main(point: Point) -> Int {
    point.x + point.y + point.x
}
"#,
        )],
        expect![[r#"
            main.rue:2:4-2:5
            main.rue:7:10-7:11
            main.rue:7:30-7:31
        "#]],
    );
}

#[test]
fn test_references_across_files() {
    check_references(
        &[
            (
                "main.rue",
                r#"
import utils::greet;

fn main() -> String {
    greet("world")
}
"#,
            ),
            (
                "utils.rue",
                r#"
export fn gre$0et(name: String) -> String {
    "Hello, " + name + "!"
}
"#,
            ),
        ],
        expect![[r#"
            main.rue:1:14-1:19
            main.rue:4:4-4:9
            utils.rue:1:10-1:15
        "#]],
    );
}

#[test]
fn test_definition_after_non_ascii() {
    check_definitions(
        &[(
            "main.rue",
            "fn main() -> Int {\n    let s = \"\u{1F600}\"; let value = 42; val$0ue\n}\n",
        )],
        expect![[r#"
            main.rue:1:22-1:27
        "#]],
    );
}

#[test]
fn test_references_without_declaration() {
    check_references_without_declaration(
        &[(
            "main.rue",
            r#"
fn main() -> Int {
    double(double(1))
}

fn dou$0ble(num: Int) -> Int {
    num * 2
}
"#,
        )],
        expect![[r#"
            main.rue:2:4-2:10
            main.rue:2:11-2:17
        "#]],
    );
}

#[test]
fn test_references_without_declaration_from_reference() {
    check_references_without_declaration(
        &[(
            "main.rue",
            r#"
type Amount = Int;

fn main(a: Amount, b: Amount) -> Amo$0unt {
    a + b
}
"#,
        )],
        expect![[r#"
            main.rue:3:11-3:17
            main.rue:3:22-3:28
            main.rue:3:33-3:39
        "#]],
    );
}

#[test]
fn test_references_without_declaration_field() {
    check_references_without_declaration(
        &[(
            "main.rue",
            r#"
struct Point {
    x: Int,
    y: Int,
}

fn main(point: Point) -> Int {
    point.$0x + point.y
}
"#,
        )],
        expect![[r#"
            main.rue:7:10-7:11
        "#]],
    );
}

#[test]
fn test_references_declaration_in_other_file() {
    check_references(
        &[
            (
                "main.rue",
                r#"
import utils::greet;

fn main() -> String {
    gre$0et("world")
}
"#,
            ),
            (
                "utils.rue",
                r#"
export fn greet(name: String) -> String {
    "Hello, " + name + "!"
}
"#,
            ),
        ],
        expect![[r#"
            main.rue:1:14-1:19
            main.rue:4:4-4:9
            utils.rue:1:10-1:15
        "#]],
    );
}

#[test]
fn test_references_file_module() {
    // A file module's declaration is the whole file, so it isn't included as a reference.
    check_references(
        &[
            (
                "main.rue",
                r#"
import uti$0ls::greet;

fn main() -> String {
    utils::greet("world")
}
"#,
            ),
            (
                "utils.rue",
                r#"
export fn greet(name: String) -> String {
    "Hello, " + name + "!"
}
"#,
            ),
        ],
        expect![[r#"
            main.rue:1:7-1:12
            main.rue:4:4-4:9
        "#]],
    );
}

#[test]
fn test_references_nothing() {
    check_references(
        &[(
            "main.rue",
            r#"
fn main() -> Int {
    4$02
}
"#,
        )],
        expect![[r#"
            <none>
        "#]],
    );
}

#[test]
fn test_references_without_declaration_in_other_file() {
    check_references_without_declaration(
        &[
            (
                "main.rue",
                r#"
import utils::greet;

fn main() -> String {
    gre$0et("world")
}
"#,
            ),
            (
                "utils.rue",
                r#"
export fn greet(name: String) -> String {
    "Hello, " + name + "!"
}
"#,
            ),
        ],
        expect![[r#"
            main.rue:1:14-1:19
            main.rue:4:4-4:9
        "#]],
    );
}

#[test]
fn test_definition_through_an_import_from_a_parent_module() {
    // `deeper` imports `V` from `inner`, which imports it from `a`, rather than the top level
    check_definitions(
        &[(
            "main.rue",
            r"
mod a {
    export const V: Int = 1;
}

const V: Int = 7;

mod inner {
    import super::a::V;

    export mod deeper {
        import super::V;

        export fn f() -> Int {
            V$0
        }
    }
}

fn main() -> Int {
    inner::deeper::f() + V
}
",
        )],
        expect![[r#"
            main.rue:2:17-2:18
        "#]],
    );
}

#[test]
fn test_definition_of_a_local_function_named_like_an_imported_module() {
    check_definitions(
        &[(
            "main.rue",
            r"
mod m {
    export mod helper {}
}

import m::helper;

fn helper() -> Int {
    5
}

fn main() -> Int {
    helper$0()
}
",
        )],
        expect![[r#"
            main.rue:7:3-7:9
        "#]],
    );
}

#[test]
fn test_definition_of_a_module_with_the_same_name_as_a_function() {
    // Modules have their own namespace, so the path goes to the module
    check_definitions(
        &[(
            "main.rue",
            r"
mod helper {
    export const VALUE: Int = 1;
}

fn helper() -> Int {
    2
}

fn main() -> Int {
    helper() + help$0er::VALUE
}
",
        )],
        expect![[r#"
            main.rue:1:4-1:10
        "#]],
    );
}

#[test]
fn test_definition_of_a_function_with_the_same_name_as_a_module() {
    check_definitions(
        &[(
            "main.rue",
            r"
mod helper {
    export const VALUE: Int = 1;
}

fn helper() -> Int {
    2
}

fn main() -> Int {
    help$0er() + helper::VALUE
}
",
        )],
        expect![[r#"
            main.rue:5:3-5:9
        "#]],
    );
}

#[test]
fn test_definition_of_a_middle_segment_of_an_import_path() {
    // Each segment of an import's path goes to the module it names, not the last one
    check_definitions(
        &[(
            "main.rue",
            r"
mod outer {
    export mod inner {
        export const VALUE: Int = 1;
    }
}

import outer::inn$0er::VALUE;

fn main() -> Int {
    VALUE
}
",
        )],
        expect![[r#"
            main.rue:2:15-2:20
        "#]],
    );
}

const MODE: &str = r#"
enum Mode { Off, On { level: Int } }

fn flip(mode: Mode) -> Mode {
    if mode is Mode::Off { Mode::On { level: 1 } } else { Mode::Off }
}
"#;

fn mode(cursor: &str, replacement: &str) -> String {
    MODE.replacen(cursor, replacement, 1)
}

#[test]
fn test_definition_enum_through_its_variant() {
    check_definitions(
        &[("main.rue", &mode("mode is Mode", "mode is Mo$0de"))],
        expect![[r#"
            main.rue:1:5-1:9
        "#]],
    );
}

#[test]
fn test_references_enum() {
    check_references(
        &[("main.rue", &mode("enum Mode", "enum Mo$0de"))],
        expect![[r#"
            main.rue:1:5-1:9
            main.rue:3:14-3:18
            main.rue:3:23-3:27
            main.rue:4:15-4:19
            main.rue:4:27-4:31
            main.rue:4:58-4:62
        "#]],
    );
}

#[test]
fn test_references_enum_from_its_variant() {
    check_references(
        &[("main.rue", &mode("{ Mode::On", "{ Mo$0de::On"))],
        expect![[r#"
            main.rue:1:5-1:9
            main.rue:3:14-3:18
            main.rue:3:23-3:27
            main.rue:4:15-4:19
            main.rue:4:27-4:31
            main.rue:4:58-4:62
        "#]],
    );
}

#[test]
fn test_definition_variant_without_fields() {
    check_definitions(
        &[("main.rue", &mode("else { Mode::Off", "else { Mode::Of$0f"))],
        expect![[r#"
            main.rue:1:12-1:15
        "#]],
    );
}

#[test]
fn test_references_variant_without_fields() {
    check_references(
        &[("main.rue", &mode("{ Off", "{ Of$0f"))],
        expect![[r#"
            main.rue:1:12-1:15
            main.rue:4:21-4:24
            main.rue:4:64-4:67
        "#]],
    );
}

#[test]
fn test_references_variant_with_fields() {
    check_references(
        &[("main.rue", &mode("On {", "O$0n {"))],
        expect![[r#"
            main.rue:1:17-1:19
            main.rue:4:33-4:35
        "#]],
    );
}

#[test]
fn test_references_imported_variant() {
    check_references(
        &[(
            "main.rue",
            r#"
enum Mode { Off, On }

import Mode::Off;

fn main() -> Mode {
    O$0ff
}
"#,
        )],
        expect![[r#"
            main.rue:1:12-1:15
            main.rue:3:13-3:16
            main.rue:6:4-6:7
        "#]],
    );
}

const SHAPES: &str = r#"
export enum Shape {
    Empty,
    Circle { radius: Int },
}
"#;

#[test]
fn test_references_enum_across_files() {
    check_references(
        &[
            (
                "main.rue",
                r#"
import shapes::Shape;
import shapes::Shape::Empty;

fn main() -> Shape {
    if Empty is Shape::Circle { Shape::Empty } else { Empty }
}
"#,
            ),
            (
                "shapes.rue",
                &SHAPES.replacen("enum Shape", "enum Sha$0pe", 1),
            ),
        ],
        expect![[r#"
            main.rue:1:15-1:20
            main.rue:2:15-2:20
            main.rue:4:13-4:18
            main.rue:5:16-5:21
            main.rue:5:32-5:37
            shapes.rue:1:12-1:17
        "#]],
    );
}

#[test]
fn test_references_variant_across_files() {
    check_references(
        &[
            (
                "main.rue",
                r#"
import shapes::Shape;
import shapes::Shape::Empty;

fn main() -> Shape {
    if Empty is Shape::Circle { Shape::Empty } else { Empty }
}
"#,
            ),
            ("shapes.rue", &SHAPES.replacen("Empty,", "Emp$0ty,", 1)),
        ],
        expect![[r#"
            main.rue:2:22-2:27
            main.rue:5:7-5:12
            main.rue:5:39-5:44
            main.rue:5:54-5:59
            shapes.rue:2:4-2:9
        "#]],
    );
}

#[test]
fn test_references_glob_imported_variant() {
    check_references(
        &[(
            "main.rue",
            r#"
enum Shape { Empty, Circle { radius: Int } }

import Shape::*;

fn main() -> Shape {
    if Empty is Circle { Circle { radius: 1 } } else { Emp$0ty }
}
"#,
        )],
        expect![[r#"
            main.rue:1:13-1:18
            main.rue:6:7-6:12
            main.rue:6:55-6:60
        "#]],
    );
}

#[test]
fn test_definition_enum_in_an_import_path() {
    check_definitions(
        &[
            (
                "main.rue",
                "import shapes::Sha$0pe::Empty;\n\nfn main() -> Int {\n    0\n}\n",
            ),
            ("shapes.rue", SHAPES),
        ],
        expect![[r#"
            shapes.rue:1:12-1:17
        "#]],
    );
}

#[test]
fn test_references_field_of_generic_struct() {
    // Every use of the field is found, whichever instance of the struct it's used through, along
    // with the fields of struct initializers and bindings
    check_references(
        &[(
            "main.rue",
            r#"
struct Wrap<T> {
    val$0ue: T,
}

fn main(wrap: Wrap<Int>) -> Int {
    let other = Wrap::<String> { value: "a" };
    let { value } = Wrap::<Int> { value: 1 };
    wrap.value + other.value.length + value
}
"#,
        )],
        expect![[r#"
            main.rue:2:4-2:9
            main.rue:6:33-6:38
            main.rue:7:10-7:15
            main.rue:7:34-7:39
            main.rue:8:9-8:14
            main.rue:8:23-8:28
        "#]],
    );
}

#[test]
fn test_definition_field_in_initializer() {
    check_definitions(
        &[(
            "main.rue",
            r#"
struct Point {
    x: Int,
}

fn main() -> Point {
    Point { $0x: 1 }
}
"#,
        )],
        expect![[r#"
            main.rue:2:4-2:5
        "#]],
    );
}
