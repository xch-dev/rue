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
    // The last parameter takes the rest of the arguments
    let source = "fn total(first: Int, ...rest: List<Int>) -> Int {\n    first\n}\n\n";

    check(
        &format!("{source}fn main() -> Int {{\n    total(1, $0...[2, 3])\n}}\n"),
        expect![[r#"
            > fn total(first: Int, [...rest: List<Int>]) -> Int
        "#]],
    );
    check(
        &format!("{source}fn main(list: List<Int>) -> Int {{\n    total(1, 2, $0...list)\n}}\n"),
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
        "fn main() -> Bytes {\n    substr(\"abc\", $01)\n}\n",
        expect![[r#"
            > fn substr(value: Bytes, [start: Int]) -> Bytes
              fn substr(value: Bytes, [start: Int], end: Int) -> Bytes
        "#]],
    );
    check(
        "fn main() -> Bytes {\n    substr(\"abc\", 1, $02)\n}\n",
        expect![[r#"
              fn substr(value: Bytes, [start: Int]) -> Bytes
            > fn substr(value: Bytes, start: Int, [end: Int]) -> Bytes
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
