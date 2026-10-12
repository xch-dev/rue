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
            num: Int
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
            fn double(num: Int) -> Int
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
            const VALUE: Int
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
                ...rest: List<Int>,
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

#[test]
fn test_hover_after_non_ascii() {
    check(
        "fn main() -> Int {\n    let s = \"\u{e9}\u{1F600}\u{1F600}\"; let value = 42; val$0ue\n}\n",
        expect![[r#"
            let value: 42
        "#]],
    );
}

#[test]
fn test_hover_inline_constant() {
    check(
        r#"
inline const VALUE: Int = 42;

fn main() -> Int {
    VAL$0UE
}
"#,
        expect![[r#"
            inline const VALUE: Int
        "#]],
    );
}

#[test]
fn test_hover_inline_function() {
    check(
        r#"
fn main() -> Int {
    dou$0ble(21)
}

inline fn double(num: Int) -> Int {
    num * 2
}
"#,
        expect![[r#"
            inline fn double(num: Int) -> Int
        "#]],
    );
}

#[test]
fn test_hover_generic_function() {
    check(
        r#"
fn main() -> Int {
    iden$0tity(42)
}

fn identity<T>(value: T) -> T {
    value
}
"#,
        expect![[r#"
            fn identity<T>(value: T) -> T
        "#]],
    );
}

#[test]
fn test_hover_spread_parameter() {
    check(
        r#"
fn main() -> Int {
    tot$0al(1, 2, 3)
}

fn total(first: Int, ...rest: List<Int>) -> Int {
    first
}
"#,
        expect![[r#"
            fn total(first: Int, ...rest: List<Int>) -> Int
        "#]],
    );
}

#[test]
fn test_hover_builtin() {
    check(
        r#"
fn main() -> Bytes32 {
    sha$0256("hello")
}
"#,
        expect![[r#"
            fn sha256(value: Bytes) -> Bytes32
            fn sha256(...values: List<Bytes>) -> Bytes32
        "#]],
    );
}

#[test]
fn test_hover_builtin_generic() {
    check(
        r#"
fn main() -> Int {
    unchecked_c$0ast(42)
}
"#,
        expect![[r#"
            inline fn unchecked_cast<T>(value: Any) -> T
        "#]],
    );
}

#[test]
fn test_hover_generic_alias_reference() {
    check(
        r#"
type Both<A, B> = (A, B);

fn main(value: Bo$0th<Int, Bytes>) -> Int {
    value.first
}
"#,
        expect![[r#"
            type Both<A, B> = (A, B)
        "#]],
    );
}

#[test]
fn test_hover_generic_alias_binding() {
    check(
        r#"
type Both<A, B> = (A, B);

fn main(val$0ue: Both<Int, Bytes>) -> Int {
    value.first
}
"#,
        expect![[r#"
            value: Both<Int, Bytes>
        "#]],
    );
}

#[test]
fn test_hover_generic_struct() {
    check(
        r#"
struct Wrapper<T> {
    value: T,
}

fn main(wrapper: Wrapper<Int>) -> Int {
    wrap$0per.value
}
"#,
        expect![[r#"
            wrapper: Wrapper<Int>
        "#]],
    );
}

#[test]
fn test_hover_generic_struct_declaration() {
    check(
        r#"
struct Wra$0pper<T> {
    value: T,
}

fn main(wrapper: Wrapper<Int>) -> Int {
    wrapper.value
}
"#,
        expect![[r#"
            struct Wrapper<T> {
                value: T,
            }
        "#]],
    );
}

#[test]
fn test_hover_list() {
    check(
        r#"
fn main(values: Li$0st<Int>) -> Int {
    values.first
}
"#,
        expect![[r#"
            type List<T> = nil | (T, List<T>)
        "#]],
    );
}
