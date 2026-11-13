//! Cursors next to punctuation between two names (`a.b`, `f(x)`, `x+y`, `List<T>`, `a::b`), and on
//! names that more than one item is recorded for, resolve to one item, which hover, definitions
//! and references agree on. Names are always separated by at least one character, so the end of one
//! name is never the start of another; the rule for choosing between different spans is tested on
//! its own in `cache.rs`.

use expect_test::{Expect, expect};

use super::TestProject;

#[allow(clippy::needless_pass_by_value)]
fn check(source: &str, expect: Expect) {
    let project = TestProject::new(&[("main.rue", source)]);
    expect.assert_eq(&project.queries());
}

const POINT: &str = r#"
struct Point {
    x: Int,
    y: Int,
}
"#;

fn with_point(source: &str) -> String {
    format!("{POINT}{source}")
}

#[test]
fn test_field_access_on_object() {
    check(
        &with_point(
            r#"
fn main(point: Point) -> Int {
    poi$0nt.x
}
"#,
        ),
        expect![[r#"
            hover:
            point: Point
            definitions:
            main.rue:6:8-6:13
            references:
            main.rue:6:8-6:13
            main.rue:7:4-7:9
        "#]],
    );
}

#[test]
fn test_field_access_before_dot() {
    check(
        &with_point(
            r#"
fn main(point: Point) -> Int {
    point$0.x
}
"#,
        ),
        expect![[r#"
            hover:
            point: Point
            definitions:
            main.rue:6:8-6:13
            references:
            main.rue:6:8-6:13
            main.rue:7:4-7:9
        "#]],
    );
}

#[test]
fn test_field_access_after_dot() {
    check(
        &with_point(
            r#"
fn main(point: Point) -> Int {
    point.$0x
}
"#,
        ),
        expect![[r#"
            hover:
            x: Int
            definitions:
            main.rue:2:4-2:5
            references:
            main.rue:2:4-2:5
            main.rue:7:10-7:11
        "#]],
    );
}

#[test]
fn test_field_access_after_field() {
    check(
        &with_point(
            r#"
fn main(point: Point) -> Int {
    point.x$0
}
"#,
        ),
        expect![[r#"
            hover:
            x: Int
            definitions:
            main.rue:2:4-2:5
            references:
            main.rue:2:4-2:5
            main.rue:7:10-7:11
        "#]],
    );
}

#[test]
fn test_call_before_parenthesis() {
    check(
        r#"
fn main(value: Int) -> Int {
    double$0(value)
}

fn double(num: Int) -> Int {
    num * 2
}
"#,
        expect![[r#"
            hover:
            fn double(num: Int) -> Int
            definitions:
            main.rue:5:3-5:9
            references:
            main.rue:2:4-2:10
            main.rue:5:3-5:9
        "#]],
    );
}

#[test]
fn test_call_after_parenthesis() {
    check(
        r#"
fn main(value: Int) -> Int {
    double($0value)
}

fn double(num: Int) -> Int {
    num * 2
}
"#,
        expect![[r#"
            hover:
            value: Int
            definitions:
            main.rue:1:8-1:13
            references:
            main.rue:1:8-1:13
            main.rue:2:11-2:16
        "#]],
    );
}

#[test]
fn test_binary_before_operator() {
    // The cursor is at the end of `x` and the start of `+`, which isn't an item.
    check(
        r#"
fn main(x: Int, y: Int) -> Int {
    x$0+y
}
"#,
        expect![[r#"
            hover:
            x: Int
            definitions:
            main.rue:1:8-1:9
            references:
            main.rue:1:8-1:9
            main.rue:2:4-2:5
        "#]],
    );
}

#[test]
fn test_binary_after_operator() {
    check(
        r#"
fn main(x: Int, y: Int) -> Int {
    x+$0y
}
"#,
        expect![[r#"
            hover:
            y: Int
            definitions:
            main.rue:1:16-1:17
            references:
            main.rue:1:16-1:17
            main.rue:2:6-2:7
        "#]],
    );
}

#[test]
fn test_generic_before_angle_bracket() {
    check(
        r#"
struct Wrapper<T> {
    value: T,
}

type Amount = Int;

fn main(wrapper: Wrapper$0<Amount>) -> Amount {
    wrapper.value
}
"#,
        expect![[r#"
            hover:
            struct Wrapper<T> {
                value: T,
            }
            definitions:
            main.rue:1:7-1:14
            references:
            main.rue:1:7-1:14
            main.rue:7:17-7:24
        "#]],
    );
}

#[test]
fn test_generic_after_angle_bracket() {
    check(
        r#"
struct Wrapper<T> {
    value: T,
}

type Amount = Int;

fn main(wrapper: Wrapper<$0Amount>) -> Amount {
    wrapper.value
}
"#,
        expect![[r#"
            hover:
            type Amount = Int
            definitions:
            main.rue:5:5-5:11
            references:
            main.rue:5:5-5:11
            main.rue:7:25-7:31
            main.rue:7:37-7:43
        "#]],
    );
}

#[test]
fn test_generic_before_closing_angle_bracket() {
    check(
        r#"
struct Wrapper<T> {
    value: T,
}

type Amount = Int;

fn main(wrapper: Wrapper<Amount$0>) -> Amount {
    wrapper.value
}
"#,
        expect![[r#"
            hover:
            type Amount = Int
            definitions:
            main.rue:5:5-5:11
            references:
            main.rue:5:5-5:11
            main.rue:7:25-7:31
            main.rue:7:37-7:43
        "#]],
    );
}

#[test]
fn test_struct_initializer_field() {
    check(
        &with_point(
            r#"
fn main() -> Point {
    let x = 1;
    Point { $0x, y: 2 }
}
"#,
        ),
        expect![[r#"
            hover:
            let x: 1
            ---
            x: Int
            definitions:
            main.rue:7:8-7:9
            references:
            main.rue:7:8-7:9
            main.rue:8:12-8:13
        "#]],
    );
}

#[test]
fn test_module_path() {
    check(
        r#"
mod math {
    export fn double(num: Int) -> Int {
        num * 2
    }
}

fn main() -> Int {
    math::$0double(21)
}
"#,
        expect![[r#"
            hover:
            fn double(num: Int) -> Int
            definitions:
            main.rue:2:14-2:20
            references:
            main.rue:2:14-2:20
            main.rue:8:10-8:16
        "#]],
    );
}

#[test]
fn test_module_path_module() {
    check(
        r#"
mod math {
    export fn double(num: Int) -> Int {
        num * 2
    }
}

fn main() -> Int {
    math$0::double(21)
}
"#,
        expect![[r#"
            hover:
            mod math
            definitions:
            main.rue:1:4-1:8
            references:
            main.rue:1:4-1:8
            main.rue:8:4-8:8
        "#]],
    );
}

#[test]
fn test_struct_destructuring_field() {
    check(
        &with_point(
            r#"
fn main(point: Point) -> Int {
    let { $0x, y } = point;
    x + y
}
"#,
        ),
        expect![[r#"
            hover:
            let x: Int
            definitions:
            main.rue:8:4-8:5
            references:
            main.rue:7:10-7:11
            main.rue:8:4-8:5
        "#]],
    );
}

#[test]
fn test_struct_destructuring_renamed_field() {
    check(
        &with_point(
            r#"
fn main(point: Point) -> Int {
    let { x$0: px, y } = point;
    px + y
}
"#,
        ),
        expect![[r#"
            hover:
            x: Int
            definitions:
            main.rue:2:4-2:5
            references:
            main.rue:2:4-2:5
            main.rue:7:10-7:11
        "#]],
    );
}

#[test]
fn test_struct_destructuring_renamed_binding() {
    check(
        &with_point(
            r#"
fn main(point: Point) -> Int {
    let { x: $0px, y } = point;
    px + y
}
"#,
        ),
        expect![[r#"
            hover:
            let px: Int
            definitions:
            main.rue:8:4-8:6
            references:
            main.rue:7:13-7:15
            main.rue:8:4-8:6
        "#]],
    );
}

#[test]
fn test_generic_function_body() {
    check(
        r#"
fn main() -> Int {
    identity(1) + identity(2)
}

fn identity<T>(value: T) -> T {
    val$0ue
}
"#,
        expect![[r#"
            hover:
            value: T
            definitions:
            main.rue:5:15-5:20
            references:
            main.rue:5:15-5:20
            main.rue:6:4-6:9
        "#]],
    );
}

#[test]
fn test_inline_function_body() {
    check(
        r#"
fn main() -> Int {
    double(1) + double(2)
}

inline fn double(num: Int) -> Int {
    nu$0m * 2
}
"#,
        expect![[r#"
            hover:
            num: Int
            definitions:
            main.rue:5:17-5:20
            references:
            main.rue:5:17-5:20
            main.rue:6:4-6:7
        "#]],
    );
}

#[test]
fn test_lambda_body() {
    check(
        r#"
fn main() -> Int {
    let f = fn(num: Int) => nu$0m * 2;
    f(1) + f(2)
}
"#,
        expect![[r#"
            hover:
            num: Int
            definitions:
            main.rue:2:15-2:18
            references:
            main.rue:2:15-2:18
            main.rue:2:28-2:31
        "#]],
    );
}

#[test]
fn test_imported_name() {
    check(
        r#"
mod math {
    export fn double(num: Int) -> Int {
        num * 2
    }
}

import math::dou$0ble;

fn main() -> Int {
    double(21)
}
"#,
        expect![[r#"
            hover:
            fn double(num: Int) -> Int
            definitions:
            main.rue:2:14-2:20
            references:
            main.rue:2:14-2:20
            main.rue:7:13-7:19
            main.rue:10:4-10:10
        "#]],
    );
}

#[test]
fn test_narrowed_binding() {
    check(
        r#"
fn main(value: Int | nil) -> Int {
    if value is Int {
        return val$0ue;
    }
    0
}
"#,
        expect![[r#"
            hover:
            value: Int
            definitions:
            main.rue:1:8-1:13
            references:
            main.rue:1:8-1:13
            main.rue:2:7-2:12
            main.rue:3:15-3:20
        "#]],
    );
}
