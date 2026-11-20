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
fn test_hover_indexed_value() {
    check(
        r#"
fn main(value: String, index: Int) -> Bytes {
    val$0ue[index]
}
"#,
        expect![[r#"
            value: String
        "#]],
    );
}

#[test]
fn test_hover_index() {
    check(
        r#"
fn main(value: String, index: Int) -> Bytes {
    value[ind$0ex]
}
"#,
        expect![[r#"
            index: Int
        "#]],
    );
}

#[test]
fn test_hover_slice_bound() {
    check(
        r#"
fn main(value: String, index: Int) -> Bytes {
    value[1..ind$0ex]
}
"#,
        expect![[r#"
            index: Int
        "#]],
    );
}

#[test]
fn test_hover_index_result() {
    check(
        r#"
fn main(value: String) -> Bytes {
    let first = value[0];
    let rest = value[1..];
    fir$0st + rest
}
"#,
        expect![[r#"
            let first: String
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
fn test_hover_generic_function_with_placeholder() {
    check(
        r#"
fn main() -> Int {
    wr$0ap::<_, Bytes>(42).first
}

fn wrap<A, B>(value: A) -> (A, B | nil) {
    (value, nil)
}
"#,
        expect![[r#"
            fn wrap<A, B>(value: A) -> (A, B | nil)
        "#]],
    );
}

#[test]
fn test_hover_generic_argument_placeholder() {
    check(
        r#"
fn main() -> Int {
    let pair = wrap::<$0_, Bytes>(42);
    pair.first
}

fn wrap<A, B>(value: A) -> (A, B | nil) {
    (value, nil)
}
"#,
        expect![[r#"
            <none>
        "#]],
    );
}

#[test]
fn test_hover_binding_with_placeholder() {
    check(
        r#"
fn main() -> Int {
    let pa$0ir = wrap::<_, Bytes>(42);
    pair.first
}

fn wrap<A, B>(value: A) -> (A, B | nil) {
    (value, nil)
}
"#,
        expect![[r#"
            let pair: (Int, Bytes | nil)
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

#[test]
fn test_hover_narrowed_in_match_arm() {
    check(
        r#"
struct Point {
    x: Int,
}

fn main(value: Point | Int) -> Int {
    match value {
        Point => val$0ue.x,
        Int => value,
    }
}
"#,
        expect![[r#"
            value: Point
        "#]],
    );
}

#[test]
fn test_hover_narrowed_in_assert_message() {
    // The message is only evaluated if the condition is false
    check(
        r#"
fn main(value: Int | nil) -> Int {
    assert !(value is nil), val$0ue;
    value
}
"#,
        expect![[r#"
            value: nil
        "#]],
    );
}

#[test]
fn test_hover_narrowed_after_assert_with_message() {
    check(
        r#"
fn main(value: Int | nil) -> Int {
    assert !(value is nil), value;
    val$0ue
}
"#,
        expect![[r#"
            value: Int
        "#]],
    );
}

#[test]
fn test_hover_narrowed_in_wildcard_match_arm() {
    check(
        r#"
struct Point {
    x: Int,
}

fn main(value: Point | Int | nil) -> Int {
    match value {
        nil => 0,
        _ => val$0ue as Int,
    }
}
"#,
        expect![[r#"
            value: Point | Int
        "#]],
    );
}

#[test]
fn test_hover_match_arm_pattern() {
    check(
        r#"
struct Point {
    x: Int,
}

fn main(value: Point | Int) -> Int {
    match value {
        Poi$0nt => value.x,
        Int => value,
    }
}
"#,
        expect![[r#"
            struct Point {
                x: Int,
            }
        "#]],
    );
}

const SHAPE: &str = r#"
enum Shape {
    Empty,
    Circle { radius: Int },
    Rect = 10 { width: Int, ...rest: Any },
}
"#;

#[test]
fn test_hover_enum() {
    check(
        &format!("{SHAPE}\nfn main(shape: Sh$0ape) -> Shape {{\n    shape\n}}\n"),
        expect![[r#"
            enum Shape {
                Empty = 0,
                Circle = 1 { radius: Int },
                Rect = 10 { width: Int, ...rest: Any },
            }
        "#]],
    );
}

#[test]
fn test_hover_enum_declaration() {
    check(
        r#"
enum Mo$0de {
    Off,
    On = 5,
}

fn main() -> Mode {
    Mode::On
}
"#,
        expect![[r#"
            enum Mode {
                Off = 0,
                On = 5,
            }
        "#]],
    );
}

#[test]
fn test_hover_empty_enum() {
    check(
        "enum Nev$0er {}\n",
        expect![[r#"
            enum Never {}
        "#]],
    );
}

#[test]
fn test_hover_variant_with_braces() {
    check(
        r#"
enum Shape {
    Empty {},
    Point,
    Circle { radius: Int },
}

fn main() -> Shape {
    Shape::Emp$0ty {}
}
"#,
        expect![[r#"
            Shape::Empty = 0 {}
        "#]],
    );
}

#[test]
fn test_hover_enum_with_braces() {
    check(
        r#"
enum Sha$0pe {
    Empty {},
    Point,
    Circle { radius: Int },
}
"#,
        expect![[r#"
            enum Shape {
                Empty = 0 {},
                Point = 1,
                Circle = 2 { radius: Int },
            }
        "#]],
    );
}

#[test]
fn test_hover_generic_enum() {
    check(
        r#"
enum Opt$0ion<T> {
    None,
    Some { value: T },
}
"#,
        expect![[r#"
            enum Option<T> {
                None = 0,
                Some = 1 { value: T },
            }
        "#]],
    );
}

#[test]
fn test_hover_enum_through_its_variant() {
    check(
        &format!("{SHAPE}\nfn main() -> Shape {{\n    Sha$0pe::Empty\n}}\n"),
        expect![[r#"
            enum Shape {
                Empty = 0,
                Circle = 1 { radius: Int },
                Rect = 10 { width: Int, ...rest: Any },
            }
        "#]],
    );
}

#[test]
fn test_hover_variant() {
    check(
        &format!("{SHAPE}\nfn main() -> Shape {{\n    Shape::Cir$0cle {{ radius: 1 }}\n}}\n"),
        expect![[r#"
            Shape::Circle = 1 { radius: Int }
        "#]],
    );
}

#[test]
fn test_hover_variant_with_spread() {
    check(
        &format!("{SHAPE}\nfn main(rect: Shape::Re$0ct) -> Any {{\n    rect.rest\n}}\n"),
        expect![[r#"
            Shape::Rect = 10 { width: Int, ...rest: Any }
        "#]],
    );
}

#[test]
fn test_hover_variant_without_fields() {
    check(
        &format!("{SHAPE}\nfn main() -> Shape {{\n    Shape::Emp$0ty\n}}\n"),
        expect![[r#"
            Shape::Empty = 0
        "#]],
    );
}

#[test]
fn test_hover_generic_variant() {
    check(
        r#"
enum Option<T> {
    None,
    Some { value: T },
}

fn main(option: Option::So$0me<Int>) -> Int {
    option.value
}
"#,
        expect![[r#"
            Option::Some<T> = 1 { value: T }
        "#]],
    );
}

#[test]
fn test_hover_generic_variant_without_fields_declaration() {
    check(
        r#"
enum Option<T> {
    No$0ne,
    Some { value: T },
}

fn main() -> Option<Int> {
    Option::None
}
"#,
        expect![[r#"
            Option::None<T> = 0
        "#]],
    );
}

#[test]
fn test_hover_enum_in_an_import_path() {
    check(
        &format!("{SHAPE}\nimport Sha$0pe::Empty;\n\nfn main() -> Shape {{\n    Empty\n}}\n"),
        expect![[r#"
            enum Shape {
                Empty = 0,
                Circle = 1 { radius: Int },
                Rect = 10 { width: Int, ...rest: Any },
            }
        "#]],
    );
}

// An imported variant is named the way it's in scope, like any other type
#[test]
fn test_hover_imported_variant() {
    check(
        &format!(
            "{SHAPE}\nimport Shape::{{Circle, Empty}};\n\nfn main(circle: Cir$0cle) -> Shape {{\n    Empty\n}}\n"
        ),
        expect![[r#"
            Circle = 1 { radius: Int }
        "#]],
    );
}

#[test]
fn test_hover_imported_variant_without_fields() {
    check(
        &format!("{SHAPE}\nimport Shape::Empty;\n\nfn main() -> Shape {{\n    Emp$0ty\n}}\n"),
        expect![[r#"
            Empty = 0
        "#]],
    );
}

#[test]
fn test_hover_for_item() {
    check(
        r#"
fn main(xs: List<Int>) -> List<Int> {
    for x in xs { x$0 + 1 }
}
"#,
        expect![[r#"
            let x: Int
        "#]],
    );
}

#[test]
fn test_hover_for_fold() {
    check(
        r#"
fn main(xs: List<Int>) -> Int {
    for x in xs fold sum = 0 { su$0m + x }
}
"#,
        expect![[r#"
            let sum: Int
        "#]],
    );
}

#[test]
fn test_hover_for_narrowed_item() {
    check(
        r#"
fn main(xs: List<Int | (Int, Int)>) -> List<Int> {
    for x in xs if x is Int { x$0 }
}
"#,
        expect![[r#"
            let x: Int
        "#]],
    );
}
