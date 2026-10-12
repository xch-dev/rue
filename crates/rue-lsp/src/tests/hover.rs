use expect_test::{Expect, expect};

use super::TestProject;

#[allow(clippy::needless_pass_by_value)]
fn check(source: &str, expect: Expect) {
    let project = TestProject::new(&[("main.rue", source)]);
    expect.assert_eq(&project.hover());
}

#[test]
fn test_hover_binding() {
    check(
        r#"
fn main() -> Int {
    let value = 42;
    val$0ue
}
"#,
        expect![[r#"
            let value: 42
        "#]],
    );
}

#[test]
fn test_hover_binding_declaration() {
    check(
        r#"
fn main() -> Int {
    let val$0ue = 42;
    value
}
"#,
        expect![[r#"
            let value: 42
        "#]],
    );
}

#[test]
fn test_hover_parameter() {
    check(
        r#"
fn main(num: Int) -> Int {
    nu$0m + 1
}
"#,
        expect![[r#"
            let num: Int
        "#]],
    );
}

#[test]
fn test_hover_function() {
    check(
        r#"
fn main() -> Int {
    dou$0ble(21)
}

fn double(num: Int) -> Int {
    num * 2
}
"#,
        expect![[r#"
            let double: fn(num: Int) -> Int
        "#]],
    );
}

#[test]
fn test_hover_constant() {
    check(
        r#"
const VALUE: Int = 42;

fn main() -> Int {
    VAL$0UE
}
"#,
        expect![[r#"
            let VALUE: Int
        "#]],
    );
}

#[test]
fn test_hover_module() {
    check(
        r#"
mod inner {
    export const NUM: Int = 42;
}

fn main() -> Int {
    inn$0er::NUM
}
"#,
        expect![[r#"
            mod inner
        "#]],
    );
}

#[test]
fn test_hover_type_alias() {
    check(
        r#"
type Amount = Int;

fn main(amount: Amo$0unt) -> Int {
    amount
}
"#,
        expect![[r#"
            type Amount = Int
        "#]],
    );
}

#[test]
fn test_hover_builtin_type() {
    check(
        r#"
fn main(num: In$0t) -> Int {
    num
}
"#,
        expect![[r#"
            type Int
        "#]],
    );
}

#[test]
fn test_hover_struct() {
    check(
        r#"
struct Point {
    x: Int,
    y: Int,
}

fn main() -> Po$0int {
    Point { x: 1, y: 2 }
}
"#,
        expect![[r#"
            struct Point {
                x: Int,
                y: Int,
            }
        "#]],
    );
}

#[test]
fn test_hover_struct_spread() {
    check(
        r#"
struct Curried {
    first: Int,
    ...rest: List<Int>,
}

fn main(value: Cur$0ried) -> Int {
    value.first
}
"#,
        expect![[r#"
            struct Curried {
                first: Int,
                ...rest: List,
            }
        "#]],
    );
}

#[test]
fn test_hover_field_reference() {
    check(
        r#"
struct Point {
    x: Int,
    y: Int,
}

fn main(point: Point) -> Int {
    point.$0y
}
"#,
        expect![[r#"
            y: Int
        "#]],
    );
}

#[test]
fn test_hover_field_initializer() {
    check(
        r#"
struct Point {
    x: Int,
    y: Int,
}

fn main() -> Point {
    Point { $0x: 1, y: 2 }
}
"#,
        expect![[r#"
            x: Int
        "#]],
    );
}

#[test]
fn test_hover_nothing() {
    check(
        r#"
fn main() -> Int {
    42$0
}
"#,
        expect![[r#"
            <none>
        "#]],
    );
}
