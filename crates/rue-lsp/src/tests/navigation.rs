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
            main.rue:3:33-3:39
            main.rue:3:11-3:17
            main.rue:3:22-3:28
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
