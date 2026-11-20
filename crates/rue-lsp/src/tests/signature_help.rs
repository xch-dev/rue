//! Signature help shows the function that's being called at the cursor, and the parameter that
//! the argument at the cursor is for.

use expect_test::{Expect, expect};

use super::TestProject;

#[allow(clippy::needless_pass_by_value)]
fn check(source: &str, expect: Expect) {
    check_files(&[("main.rue", source)], expect);
}

#[allow(clippy::needless_pass_by_value)]
fn check_files(files: &[(&str, &str)], expect: Expect) {
    let project = TestProject::new(files);
    expect.assert_eq(&project.signature_help());
}

const ADD: &str = "fn add(first: Int, second: Int) -> Int {\n    first + second\n}\n\n";

fn with_add(source: &str) -> String {
    format!("{ADD}{source}")
}

#[test]
fn test_signature_of_function() {
    check(
        &with_add("fn main() -> Int {\n    add($0)\n}\n"),
        expect![[r#"
            > fn add([first: Int], second: Int) -> Int
        "#]],
    );
    check(
        &with_add("fn main() -> Int {\n    add(1$0, 2)\n}\n"),
        expect![[r#"
            > fn add([first: Int], second: Int) -> Int
        "#]],
    );
    check(
        &with_add("fn main() -> Int {\n    add(1,$0 2)\n}\n"),
        expect![[r#"
            > fn add(first: Int, [second: Int]) -> Int
        "#]],
    );
    check(
        &with_add("fn main() -> Int {\n    add(1, 2$0)\n}\n"),
        expect![[r#"
            > fn add(first: Int, [second: Int]) -> Int
        "#]],
    );
}

#[test]
fn test_signature_outside_of_arguments() {
    // The cursor has to be between the parentheses
    check(
        &with_add("fn main() -> Int {\n    add$0(1, 2)\n}\n"),
        expect![[r#"
            <none>
        "#]],
    );
    check(
        &with_add("fn main() -> Int {\n    add(1, 2)$0\n}\n"),
        expect![[r#"
            <none>
        "#]],
    );
    check(
        &with_add("fn main() -> Int {\n    $0add(1, 2)\n}\n"),
        expect![[r#"
            <none>
        "#]],
    );
    check(
        "fn main() -> Int {\n    1 + $02\n}\n",
        expect![[r#"
        <none>
    "#]],
    );
}

#[test]
fn test_signature_of_nested_calls() {
    // The innermost call that the cursor is in the arguments of
    check(
        &with_add("fn main() -> Int {\n    add(add(1, $02), 3)\n}\n"),
        expect![[r#"
            > fn add(first: Int, [second: Int]) -> Int
        "#]],
    );
    check(
        &with_add("fn main() -> Int {\n    add(add(1, 2)$0, 3)\n}\n"),
        expect![[r#"
            > fn add([first: Int], second: Int) -> Int
        "#]],
    );
    check(
        &with_add("fn main() -> Int {\n    add(add(1, 2), $03)\n}\n"),
        expect![[r#"
            > fn add(first: Int, [second: Int]) -> Int
        "#]],
    );

    // Inside of a lambda that's an argument, it's still the call that the lambda is passed to
    check(
        "fn apply(value: Int, function: fn(item: Int) -> Int) -> Int {\n    function(value)\n}\n\nfn main() -> Int {\n    apply(1, fn(item) => item + $01)\n}\n",
        expect![[r#"
            > fn apply(value: Int, [function: fn(item: Int) -> Int]) -> Int
        "#]],
    );
}

#[test]
fn test_signature_with_too_many_arguments() {
    check(
        &with_add("fn main() -> Int {\n    add(1, 2, $03)\n}\n"),
        expect![[r#"
            > fn add(first: Int, [second: Int]) -> Int
        "#]],
    );
}

#[test]
fn test_signature_without_parameters() {
    check(
        "fn one() -> Int {\n    1\n}\n\nfn main() -> Int {\n    one($0)\n}\n",
        expect![[r#"
            > fn one() -> Int
        "#]],
    );
}

#[test]
fn test_signature_of_spread_parameter() {
    // The last parameter is the rest of the arguments as one list, which is spread into it
    let source = "fn total(first: Int, ...rest: List<Int>) -> Int {\n    first\n}\n\n";

    check(
        &format!("{source}fn main() -> Int {{\n    total(1, $0...[2, 3])\n}}\n"),
        expect![[r#"
            > fn total(first: Int, [...rest: List<Int>]) -> Int
        "#]],
    );
    check(
        "fn main(total: fn(first: Int, ...rest: List<Int>) -> Int) -> Int {\n    total(1, $0...[2])\n}\n",
        expect![[r#"
            > fn total(first: Int, [...rest: List<Int>]) -> Int
        "#]],
    );
}

#[test]
fn test_signature_of_lambda_in_binding() {
    check(
        "fn main() -> Int {\n    let add = fn(first: Int, second: Int) => first + second;\n    add(1, $02)\n}\n",
        expect![[r#"
            > fn add(first: Int, [second: Int]) -> Int
        "#]],
    );

    check(
        "fn main(combine: fn(first: Int, second: Int) -> Int) -> Int {\n    combine(1, $02)\n}\n",
        expect![[r#"
            > fn combine(first: Int, [second: Int]) -> Int
        "#]],
    );
}

#[test]
fn test_signature_of_expression_that_is_called() {
    // A function that isn't named by a path has no name to show
    check(
        "fn make() -> fn(value: Int) -> Int {\n    fn(value) => value\n}\n\nfn main() -> Int {\n    make()($01)\n}\n",
        expect![[r#"
            > fn([value: Int]) -> Int
        "#]],
    );

    check(
        "struct Ops {\n    double: fn(value: Int) -> Int,\n}\n\nfn main(ops: Ops) -> Int {\n    ops.double($01)\n}\n",
        expect![[r#"
            > fn([value: Int]) -> Int
        "#]],
    );
}

#[test]
fn test_signature_of_generic_function() {
    let source = "fn pick<T, U>(first: T, second: U, third: T) -> T {\n    first\n}\n\n";

    // Nothing is known about the generics yet
    check(
        &format!("{source}fn main() -> Int {{\n    let value = pick($0);\n    1\n}}\n"),
        expect![[r#"
            > fn pick<T, U>([first: T], second: U, third: T) -> T
        "#]],
    );

    // The generics that the arguments so far give are shown as what they were inferred as
    check(
        &format!("{source}fn main() -> Int {{\n    let value = pick(1, $0);\n    1\n}}\n"),
        expect![[r#"
            > fn pick<T = Int, U>(first: T, [second: U], third: T) -> T
        "#]],
    );
    check(
        &format!("{source}fn main() -> Int {{\n    pick(1, \"a\", $02)\n}}\n"),
        expect![[r#"
            > fn pick<T = Int, U = String>(first: T, second: U, [third: T]) -> T
        "#]],
    );

    // And the type that's expected of the call gives them too
    check(
        &format!("{source}fn main() -> Int {{\n    pick($0)\n}}\n"),
        expect![[r#"
            > fn pick<T = Int, U>([first: T], second: U, third: T) -> T
        "#]],
    );
}

#[test]
fn test_signature_of_recursive_generic_call() {
    // The generics are the function's own, which a call to itself can infer as each other
    check(
        "fn first<T>(list: List<T>, fallback: T) -> T {\n    first(list, $0fallback)\n}\n",
        expect![[r#"
            > fn first<T>(list: List<T>, [fallback: T]) -> T
        "#]],
    );
    check(
        "fn rec<T>(value: T, other: Int) -> Int {\n    rec(1, $02)\n}\n",
        expect![[r#"
            > fn rec<T = Int>(value: T, [other: Int]) -> Int
        "#]],
    );
    check(
        "fn swap<T, U>(a: T, b: U, n: Int) -> Int {\n    swap(b, a, $0n - 1)\n}\n",
        expect![[r#"
            > fn swap<T = U, U = T>(a: T, b: U, [n: Int]) -> Int
        "#]],
    );
}

#[test]
fn test_signature_of_ambiguous_call() {
    // The call is rejected, since it could be to either function, so neither is shown
    check(
        "fn main(f: (fn(a: Int) -> Int) | (fn(b: Bytes) -> Int)) -> Int {\n    f($01)\n}\n",
        expect![[r#"
            <none>
        "#]],
    );
}

#[test]
fn test_signature_with_large_union() {
    // A union that's written in full doesn't say that anything was left out of it
    let members = (1..=20)
        .map(|i| i.to_string())
        .collect::<Vec<_>>()
        .join(" | ");

    check(
        &format!(
            "fn big(value: {members}, other: Int) -> Int {{\n    other\n}}\n\nfn main() -> Int {{\n    big(1, $02)\n}}\n"
        ),
        expect![[r#"
            > fn big(value: 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20, [other: Int]) -> Int
        "#]],
    );

    // And one that isn't says so once, at its end
    let members = (1..=100)
        .map(|i| i.to_string())
        .collect::<Vec<_>>()
        .join(" | ");

    check(
        &format!(
            "fn big(value: {members}, other: Int) -> Int {{\n    other\n}}\n\nfn main() -> Int {{\n    big(1, $02)\n}}\n"
        ),
        expect![[r#"
            > fn big(value: 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39 | 40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 | 48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 59 | 60 | 61 | 62 | 63 | ..., [other: Int]) -> Int
        "#]],
    );
}

#[test]
fn test_signature_of_generic_function_with_lambda() {
    check(
        "fn map<T, U>(list: List<T>, function: fn(item: T) -> U) -> List<U> {\n    []\n}\n\nfn main(list: List<Int>) -> List<Bool> {\n    map(list, $0fn(item) => item > 1)\n}\n",
        expect![[r#"
            > fn map<T = Int, U = true | false>(list: List<T>, [function: fn(item: T) -> U]) -> List<U>
        "#]],
    );
}

#[test]
fn test_signature_of_builtin() {
    check(
        "fn main() -> Bytes32 {\n    sha256($0\"a\")\n}\n",
        expect![[r#"
            > fn sha256([value: Bytes]) -> Bytes32
              fn sha256([...values: List<Bytes>]) -> Bytes32
        "#]],
    );
    check(
        "fn main() -> PublicKey {\n    g1_map(\"abc\", $0\"dst\")\n}\n",
        expect![[r#"
              fn g1_map([data: Bytes]) -> PublicKey
            > fn g1_map(data: Bytes, [dst: Bytes]) -> PublicKey
        "#]],
    );
    check(
        "fn main() -> PublicKey {\n    g1_map($0\"abc\")\n}\n",
        expect![[r#"
            > fn g1_map([data: Bytes]) -> PublicKey
              fn g1_map([data: Bytes], dst: Bytes) -> PublicKey
        "#]],
    );
    // A signature that takes the rest of the arguments is for all of them
    check(
        "fn main() -> Bytes32 {\n    sha256(\"a\", $0\"b\")\n}\n",
        expect![[r#"
              fn sha256([value: Bytes]) -> Bytes32
            > fn sha256([...values: List<Bytes>]) -> Bytes32
        "#]],
    );

    // With more arguments than any signature takes, it's the last one
    check(
        "fn main() -> PublicKey {\n    g1_map(\"abc\", \"dst\", $0\"more\")\n}\n",
        expect![[r#"
              fn g1_map([data: Bytes]) -> PublicKey
            > fn g1_map(data: Bytes, [dst: Bytes]) -> PublicKey
        "#]],
    );

    check(
        "fn main(key: PublicKey, signature: Signature) -> nil {\n    bls_verify(signature, key, \"a\", key, $0\"b\")\n}\n",
        expect![[r#"
            > fn bls_verify(signature: Signature, public_key: PublicKey, message: Bytes, [...]) -> nil
              fn bls_verify(signature: Signature, [...pairs: AlternatingList<PublicKey, Bytes>]) -> nil
        "#]],
    );
}

#[test]
fn test_signature_of_imported_function() {
    check_files(
        &[
            (
                "main.rue",
                "import utils::double;\n\nfn main() -> Int {\n    double($01) + utils::double(2)\n}\n",
            ),
            (
                "utils.rue",
                "export fn double(value: Int) -> Int {\n    value * 2\n}\n",
            ),
        ],
        expect![[r#"
            > fn double([value: Int]) -> Int
        "#]],
    );

    check_files(
        &[
            (
                "main.rue",
                "fn main() -> Int {\n    utils::double($01)\n}\n",
            ),
            (
                "utils.rue",
                "export fn double(value: Int) -> Int {\n    value * 2\n}\n",
            ),
        ],
        expect![[r#"
            > fn double([value: Int]) -> Int
        "#]],
    );
}

#[test]
fn test_signature_is_of_the_file_with_the_cursor() {
    // Each file has a call and a struct initializer in the same place
    let file = |name: &str, cursor: &str| {
        format!(
            "struct Point {{\n    {name}: Int,\n}}\n\nfn make({name}: Int) -> Point {{\n    Point {{ {name} }}\n}}\n\nfn call() -> Point {{\n    let point = Point {{ {cursor} }};\n    make(1)\n}}\n"
        )
    };

    check_files(
        &[
            ("main.rue", &file("aaa", "$0")),
            ("other.rue", &file("bbb", "")),
            ("zzz.rue", &file("ccc", "")),
        ],
        expect![[r#"
            > Point { [aaa: Int] }
        "#]],
    );

    check_files(
        &[
            ("main.rue", &file("aaa", "").replace("make(1)", "make($01)")),
            ("other.rue", &file("bbb", "")),
            ("zzz.rue", &file("ccc", "")),
        ],
        expect![[r#"
            > fn make([aaa: Int]) -> Point
        "#]],
    );
}

#[test]
fn test_signature_of_standard_library_function() {
    let project = TestProject::with_manifest(
        Some("[compiler]\nentrypoint = \".\"\n"),
        &[(
            "main.rue",
            "fn main() -> Bytes32 {\n    tree_hash_pair(tree_hash_atom(\"a\"), $0)\n}\n",
        )],
    );

    expect![[r#"
        > fn tree_hash_pair(first: Bytes32, [rest: Bytes32]) -> Bytes32
    "#]]
    .assert_eq(&project.signature_help());
}

const POINT: &str = "struct Point {\n    x: Int,\n    y: Int,\n}\n\n";

#[test]
fn test_signature_of_struct_initializer() {
    // The field that the cursor is in, or else the first one that's missing
    for (source, expect) in [
        (
            "Point { $0 }",
            expect![[r#"
            > Point { [x: Int], y: Int }
        "#]],
        ),
        (
            "Point { x: $01 }",
            expect![[r#"
            > Point { [x: Int], y: Int }
        "#]],
        ),
        (
            "Point { x: 1, $0 }",
            expect![[r#"
            > Point { x: Int, [y: Int] }
        "#]],
        ),
        (
            "Point { y: 1, $0 }",
            expect![[r#"
            > Point { [x: Int], y: Int }
        "#]],
        ),
        (
            "Point { y: $01, x: 2 }",
            expect![[r#"
            > Point { x: Int, [y: Int] }
        "#]],
        ),
        (
            "Point { y: 1, x: 2$0 }",
            expect![[r#"
            > Point { [x: Int], y: Int }
        "#]],
        ),
        (
            "Point$0 { x: 1, y: 2 }",
            expect![[r#"
            <none>
        "#]],
        ),
        (
            "Point { x: 1, y: 2 }$0",
            expect![[r#"
            <none>
        "#]],
        ),
    ] {
        check(
            &format!("{POINT}fn main() -> Point {{\n    {source}\n}}\n"),
            expect,
        );
    }
}

#[test]
fn test_signature_of_generic_struct_and_variant() {
    check(
        "struct Wrap<T> {\n    value: T,\n    ...rest: List<T>,\n}\n\nfn main() -> Wrap<Int> {\n    Wrap::<Int> { value: 1, $0 }\n}\n",
        expect![[r#"
            > Wrap::<Int> { value: Int, [...rest: List<Int>] }
        "#]],
    );

    check(
        "enum Shape {\n    Circle = 1 { radius: Int },\n    Empty = 2,\n}\n\nfn main() -> Shape {\n    Shape::Circle { $0 }\n}\n",
        expect![[r#"
            > Shape::Circle { [radius: Int] }
        "#]],
    );

    // A struct without fields has nothing to show
    check(
        "struct Empty {}\n\nfn main() -> Empty {\n    Empty { $0 }\n}\n",
        expect![[r#"
            <none>
        "#]],
    );
}

#[test]
fn test_signature_of_call_in_struct_initializer() {
    check(
        &format!("{POINT}{ADD}fn main() -> Point {{\n    Point {{ x: add(1, $02), y: 2 }}\n}}\n"),
        expect![[r#"
            > fn add(first: Int, [second: Int]) -> Int
        "#]],
    );
    check(
        &format!("{POINT}{ADD}fn main() -> Int {{\n    add(Point {{ x: 1, $0 }}.x, 2)\n}}\n"),
        expect![[r#"
            > Point { x: Int, [y: Int] }
        "#]],
    );
}

#[test]
fn test_signature_with_syntax_errors() {
    // The call is being typed, so it isn't closed yet
    check(
        &with_add("fn main() -> Int {\n    add($0\n}\n"),
        expect![[r#"
            > fn add([first: Int], second: Int) -> Int
        "#]],
    );
    check(
        &with_add("fn main() -> Int {\n    add(1, $0\n}\n"),
        expect![[r#"
            > fn add(first: Int, [second: Int]) -> Int
        "#]],
    );
    check(
        &with_add("fn main() -> Int {\n    add(1,$0"),
        expect![[r#"
        > fn add(first: Int, [second: Int]) -> Int
    "#]],
    );
    check(
        &with_add("fn main() -> Int {\n    let value = add(add(1, $0\n    value\n}\n"),
        expect![[r#"
            > fn add(first: Int, [second: Int]) -> Int
        "#]],
    );
    check(
        &format!("{POINT}fn main() -> Point {{\n    Point {{ x: 1, $0\n}}\n"),
        expect![[r#"
            > Point { x: Int, [y: Int] }
        "#]],
    );

    // A generic struct that's missing its generic arguments shows its fields as they're declared
    check(
        "struct Wrap<T> {\n    value: T,\n    list: List<T>,\n}\n\nfn main() -> Int {\n    let wrap = Wrap { value: 1, $0 };\n    1\n}\n",
        expect![[r#"
            > Wrap { value: T, [list: List<T>] }
        "#]],
    );

    // Something that isn't a function has no signature, and the call that it's an argument of
    // isn't shown in its place
    check(
        &with_add("fn main() -> Int {\n    add(1, unknown($0))\n}\n"),
        expect![[r#"
            <none>
        "#]],
    );
    check(
        &with_add("fn main() -> Int {\n    add(1, Unknown { $0 }.x)\n}\n"),
        expect![[r#"
            <none>
        "#]],
    );
    check(
        &with_add(
            "struct Empty {}\n\nfn main() -> Int {\n    let empty = Empty { $0 };\n    add(1, 2)\n}\n",
        ),
        expect![[r#"
            <none>
        "#]],
    );

    // Something that isn't a function has no signature
    check(
        "fn main() -> Int {\n    unknown($0)\n}\n",
        expect![[r#"
        <none>
    "#]],
    );
    check(
        "fn main() -> Int {\n    let value = 1;\n    value($0)\n}\n",
        expect![[r#"
            <none>
        "#]],
    );
    check(
        "fn main() -> Int {\n    Unknown { $0 }\n}\n",
        expect![[r#"
        <none>
    "#]],
    );
}

#[test]
fn test_signature_after_non_ascii() {
    // The parameters are parts of the label in UTF-16 code units
    check(
        "fn pick(first: \"\u{1F600}\", second: \"\u{e9}\") -> Int {\n    1\n}\n\nfn main() -> Int {\n    pick(\"\u{1F600}\", $0\"\u{e9}\")\n}\n",
        expect![[r#"
            > fn pick(first: "😀", [second: "é"]) -> Int
        "#]],
    );
}

#[test]
fn test_signature_with_huge_inferred_type() {
    // Each call doubles the type, which is over a million times as long written out as it is in
    // memory, so only the start of it is shown
    let nested = format!("{}1{}", "pair(".repeat(20), ")".repeat(20));

    check(
        &format!(
            "fn pair<T>(value: T) -> (T, T) {{\n    (value, value)\n}}\n\nfn id<T>(value: T) -> T {{\n    value\n}}\n\nfn main() -> Int {{\n    let big = {nested};\n    let same = id($0big);\n    1\n}}\n"
        ),
        expect![[r#"
            > fn id<T = ((((((((((((((((((((Int, Int), (Int, Int)), ((Int, Int), (Int, Int))), (((Int, Int), (Int, Int)), ((Int, Int), (Int, Int)))), ((((Int, Int), (Int, Int)), ((Int, Int), (Int, Int))), (..., ...))), ...), ...), ...), ...), ...), ...), ...), ...), ...), ...), ...), ...), ...), ...), ...)>([value: T]) -> T
        "#]],
    );
}

#[test]
fn test_signature_in_assert_message() {
    // The comma before the message of an `assert` isn't one of a call's
    check(
        &with_add(
            "fn main(value: Int) -> Int {\n    assert add(value, $01) > 1, add(1, 2);\n    value\n}\n",
        ),
        expect![[r#"
            > fn add(first: Int, [second: Int]) -> Int
        "#]],
    );
    check(
        &with_add(
            "fn main(value: Int) -> Int {\n    assert value > 1, add($01, 2);\n    value\n}\n",
        ),
        expect![[r#"
            > fn add([first: Int], second: Int) -> Int
        "#]],
    );
    check(
        &with_add(
            "fn main(value: Int) -> Int {\n    assert value > 1,$0 add(1, 2);\n    value\n}\n",
        ),
        expect![[r#"
            <none>
        "#]],
    );
}

#[test]
fn test_signature_with_huge_return_type() {
    let nested = format!("{}1{}", "pair(".repeat(20), ")".repeat(20));

    check(
        &format!(
            "fn pair<T>(value: T) -> (T, T) {{\n    (value, value)\n}}\n\nfn main() -> Int {{\n    let big = {nested};\n    let function = fn(value: Int) => big;\n    let result = function($01);\n    1\n}}\n"
        ),
        expect![[r#"
            > fn function([value: Int]) -> ((((((((((((((((((((Int, Int), (Int, Int)), ((Int, Int), (Int, Int))), (((Int, Int), (Int, Int)), ((Int, Int), (Int, Int)))), ((((Int, Int), (Int, Int)), ((Int, Int), (Int, Int))), (..., ...))), ...), ...), ...), ...), ...), ...), ...), ...), ...), ...), ...), ...), ...), ...), ...)
        "#]],
    );
}

#[test]
fn test_signature_in_index_and_slice() {
    // A call that's indexed or sliced, or is a bound of one, is like any other
    check(
        &with_add("fn main(bytes: Bytes) -> Bytes {\n    bytes[add(1, $02)..]\n}\n"),
        expect![[r#"
            > fn add(first: Int, [second: Int]) -> Int
        "#]],
    );
    check(
        "fn make(length: Int, fill: Bytes) -> Bytes {\n    fill\n}\n\nfn main() -> Bytes {\n    make(1, $0\"a\")[0..1]\n}\n",
        expect![[r#"
            > fn make(length: Int, [fill: Bytes]) -> Bytes
        "#]],
    );
    check(
        &with_add("fn main(bytes: Bytes) -> Bytes {\n    bytes[$01..add(1, 2)]\n}\n"),
        expect![[r#"
            <none>
        "#]],
    );
}
