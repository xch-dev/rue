//! Tests for when a value that's only referenced once is bound rather than substituted, since the
//! reference can be evaluated more than once, and for the order that inline values are captured in.

use crate::test_utils::compile;

const HELPERS: &str = "
fn total(xs: List<Int>) -> Int {
    if xs is nil { 0 } else { xs.first + total(xs.rest) }
}

fn map(xs: List<Int>, f: fn(x: Int) -> Int) -> List<Int> {
    if xs is nil { nil } else { [f(xs.first), ...map(xs.rest, f)] }
}

fn apply(f: fn(x: Int) -> Int, x: Int) -> Int {
    f(x)
}

fn apply_twice(f: fn(x: Int) -> Int, x: Int) -> Int {
    f(f(x))
}

fn apply_to_each(f: fn(x: Int) -> Int, xs: List<Int>) -> List<Int> {
    for x in xs { f(x) }
}

fn apply_through(f: fn(x: Int) -> Int, x: Int) -> Int {
    apply(f, x)
}

fn f(n: Int) -> Int {
    if n > 10 { n } else { f(n + 3) }
}

fn g(n: Int) -> Int {
    if n > 20 { n } else { g(n + 5) }
}

fn h(n: Int) -> Int {
    if n > 30 { n } else { h(n + 7) }
}

struct Plain {
    _scale: Int,
    value: Int,
}
";

/// A constant whose value goes through a list, so that it isn't cheap to evaluate again.
const K: &str = "const K: Int = total([1, 2, 3]);";

/// Whether the constant `K` is bound rather than substituted, since its only reference can be
/// evaluated more than once each time the program is run, in a program with the helpers and the
/// items.
fn is_repeated(items: &str) -> bool {
    let compiled = compile(&format!("{HELPERS}\n{K}\n{items}"));
    compiled.assert_no_errors();
    compiled
        .dependency_graph()
        .is_repeated(compiled.symbol("K"))
}

/// Whether the constant `K` is bound when it's only referenced in a lambda that's called for
/// each item of a list, given the items that declare it.
fn is_value_repeated(items: &str) -> bool {
    let compiled = compile(&format!(
        "{HELPERS}\n{items}\nfn main(xs: List<Int>) -> List<Int> {{ map(xs, fn(x) => x + K) }}"
    ));
    compiled.assert_no_errors();
    compiled
        .dependency_graph()
        .is_repeated(compiled.symbol("K"))
}

#[test]
fn cheap_value_is_evaluated_again() {
    for items in [
        "const K: Int = 5;",
        "const K: Int = 100 * 2;",
        "const K: Int = (1 + 2) * (3 + 4) - 5;",
        "fn seconds(days: Int) -> Int { days * 86400 }\nconst K: Int = seconds(14);",
        "fn positive(value: Int) -> Int { assert value > 0; value }\nconst K: Int = positive(2);",
        "const K: Int = { let a = 2 * 3; a + a };",
        "const K: Int = if 1 > 2 { 3 } else { 4 + 5 };",
        "const K: Int = (fn(y: Int) => y + 1)(2);",
        "const K: Int = { let add = fn(y: Int) => y + 1; add(2) };",
        "const ONE: Int = 2 - 1;\nconst K: Int = ONE + 1;",
        "const K: Bytes32 = sha256(0x0102 + 0x03);",
        // A value that's bound costs nothing to read
        "const BOUND: Int = total([1, 2, 3]);\nconst K: Int = BOUND + BOUND;",
    ] {
        let items = items.replace(
            "Bytes32 = sha256(0x0102 + 0x03)",
            "Int = sha256(0x0102 + 0x03) as Int",
        );
        assert!(!is_value_repeated(&items), "{items}");
    }
}

#[test]
fn value_that_can_take_long_is_bound() {
    for items in [
        // A function that calls itself, directly or through another one
        "const K: Int = total([1, 2, 3]);",
        "fn even(n: Int) -> Int { if n > 0 { odd(n - 1) } else { 1 } }\nfn odd(n: Int) -> Int { if n > 0 { even(n - 1) } else { 0 } }\nconst K: Int = even(4);",
        // Through a function that's called, and through a value that's substituted
        "fn sum() -> Int { total([1, 2, 3]) }\nconst K: Int = sum() + 1;",
        "const SUM: Int = total([1, 2, 3]);\nconst K: Int = SUM + 1;",
        "inline const SUM: Int = total([1, 2, 3]);\nconst K: Int = SUM + 1;",
        // A loop
        "const K: Int = for x in [1, 2, 3] fold sum = 0 { sum + x };",
        // A function value that isn't known where it's called
        "const K: Int = apply(fn(y) => y + 1, 2);",
        // A type check
        "fn checked(value: Any) -> Int { if value is Int { value } else { 0 } }\nconst K: Int = checked(5);",
        // More operators than are worth evaluating again
        "fn mix(a: Int, b: Int) -> Int { (a + b) * (a - b) + a * b - (a + 1) * (b + 2) }\nconst K: Int = mix(mix(1, 2), 3);",
        // An operator that's expensive on its own
        "const K: PublicKey = pubkey_for_exp(0x0000000000000000000000000000000000000000000000000000000000000001);",
    ] {
        let items = items.replace(
            "const K: PublicKey = pubkey_for_exp(",
            "const KEY: PublicKey = pubkey_for_exp(",
        );
        let items = if items.contains("const KEY") {
            format!("{items}\nconst K: Int = (KEY as Bytes) as Int;")
        } else {
            items
        };
        assert!(is_value_repeated(&items), "{items}");
    }
}

#[test]
fn constant_in_the_entrypoint_is_evaluated_once() {
    assert!(!is_repeated("fn main(x: Int) -> Int { x + K }"));
}

#[test]
fn constant_in_an_entrypoint_that_calls_itself_is_repeated() {
    assert!(is_repeated(
        "fn main(x: Int) -> Int { if x > 5 { x + K } else { main(x + 1) } }"
    ));
}

#[test]
fn constant_in_a_function_is_repeated_unless_it_is_called_once() {
    let once = "fn add(x: Int) -> Int { x + K }\nfn main(x: Int) -> Int { add(x) }";
    let twice = "fn add(x: Int) -> Int { x + K }\nfn main(x: Int) -> Int { add(add(x)) }";
    let recursive = "
fn add(xs: List<Int>) -> Int { if xs is nil { 0 } else { xs.first + K + add(xs.rest) } }
fn main(xs: List<Int>) -> Int { add(xs) }";
    let nested = "
fn inner(x: Int) -> Int { x + K }
fn outer(x: Int) -> Int { inner(x) * 2 }
fn main(x: Int) -> Int { outer(x) + outer(x + 1) }";
    let value = "fn add(x: Int) -> Int { x + K }\nfn main(x: Int) -> Int { apply(add, x) }";
    let passed_on =
        "fn add(x: Int) -> Int { x + K }\nfn main(xs: List<Int>) -> List<Int> { map(xs, add) }";

    assert!(!is_repeated(once));
    assert!(is_repeated(twice));
    assert!(is_repeated(recursive));
    assert!(is_repeated(nested));
    assert!(!is_repeated(value));
    assert!(is_repeated(passed_on));
}

#[test]
fn constant_in_a_loop_is_repeated() {
    assert!(is_repeated(
        "fn main(xs: List<Int>) -> List<Int> { for x in xs { x + K } }"
    ));
}

#[test]
fn constant_in_a_lambda_is_repeated_unless_the_lambda_is_called_once() {
    let called = "fn main(x: Int) -> Int { (fn(y: Int) => y + K)(x) }";
    let bound = "fn main(x: Int) -> Int { let g = fn(y: Int) => y + K; g(x) }";
    let rebound = "fn main(x: Int) -> Int { let g = fn(y: Int) => y + K; let h = g; h(x) }";
    let inline_bound = "fn main(x: Int) -> Int { inline let g = fn(y: Int) => y + K; g(x) }";
    let passed = "fn main(x: Int) -> Int { apply(fn(y) => y + K, x) }";
    let passed_through = "fn main(x: Int) -> Int { apply_through(fn(y) => y + K, x) }";
    let bound_and_passed = "fn main(x: Int) -> Int { let g = fn(y: Int) => y + K; apply(g, x) }";
    let before_a_spread = "
fn call(f: fn(x: Int) -> Int, ...xs: List<Int>) -> Int { if xs is nil { 0 } else { f(xs.first) } }
fn main(xs: List<Int>) -> Int { call(fn(y) => y + K, ...xs) }";

    for source in [
        called,
        bound,
        rebound,
        inline_bound,
        passed,
        passed_through,
        bound_and_passed,
        before_a_spread,
    ] {
        assert!(!is_repeated(source), "{source}");
    }

    let called_twice = "fn main(x: Int) -> Int { let g = fn(y: Int) => y + K; g(x) + g(1) }";
    let passed_to_map = "fn main(xs: List<Int>) -> List<Int> { map(xs, fn(y) => y + K) }";
    let passed_and_called_twice = "fn main(x: Int) -> Int { apply_twice(fn(y) => y + K, x) }";
    let passed_to_a_loop =
        "fn main(xs: List<Int>) -> List<Int> { apply_to_each(fn(y) => y + K, xs) }";
    let called_in_a_loop =
        "fn main(xs: List<Int>) -> List<Int> { let g = fn(y: Int) => y + K; for x in xs { g(x) } }";
    let called_in_a_lambda = "fn main(xs: List<Int>) -> List<Int> { let g = fn(y: Int) => y + K; map(xs, fn(y) => g(y)) }";
    let in_a_nested_lambda =
        "fn main(xs: List<Int>) -> List<Int> { map(xs, fn(y) => (fn(z: Int) => z + K)(y)) }";
    let returned = "fn main(x: Int) -> fn(y: Int) -> Int { fn(y) => x + y + K }";
    let in_a_pair = "fn main(x: Int) -> Int { let pair = (fn(y: Int) => y + K, 1); pair.first(x) }";
    let rest = "
fn call(x: Int, ...fs: List<fn(x: Int) -> Int>) -> Int { if fs is nil { x } else { fs.first(x) } }
fn main(x: Int) -> Int { call(x, ...[fn(y: Int) => y + K]) }";

    for source in [
        called_twice,
        passed_to_map,
        passed_and_called_twice,
        passed_to_a_loop,
        called_in_a_loop,
        called_in_a_lambda,
        in_a_nested_lambda,
        returned,
        in_a_pair,
        rest,
    ] {
        assert!(is_repeated(source), "{source}");
    }
}

#[test]
fn constant_in_an_inline_constant_is_repeated_where_that_is() {
    let once = "inline const NEXT: Int = K + 1;\nfn main(x: Int) -> Int { x + NEXT }";
    let in_a_loop = "inline const NEXT: Int = K + 1;\nfn main(xs: List<Int>) -> List<Int> { for x in xs { x + NEXT } }";
    let in_a_default = "
struct Scaled { _scale: Int, value: Int = K + 1 }
fn main(xs: List<Int>) -> List<Int> { map(xs, fn(x) => Scaled { _scale: x }.value) }";

    assert!(!is_repeated(once));
    assert!(is_repeated(in_a_loop));
    assert!(is_repeated(in_a_default));
}

#[test]
fn constant_in_the_value_of_a_bound_value_is_evaluated_once() {
    let constant = "
const NEXT: Int = K + 1;
fn main(xs: List<Int>) -> List<Int> { for x in xs { x + NEXT } }";
    let binding =
        "fn main(xs: List<Int>) -> List<Int> { let next = K + 1; map(xs, fn(x) => x + next) }";

    assert!(!is_repeated(constant));
    assert!(!is_repeated(binding));
}

/// The program of `main`, in a program with the helpers and the items.
fn program(items: &str) -> String {
    compile(&format!("{HELPERS}\n{K}\n{items}")).main_program()
}

/// A lambda that's only called once evaluates the value once, so the value is substituted, and
/// the program is the same as if it were written where it's used.
#[test]
fn value_in_a_lambda_called_once_is_substituted() {
    let written = program("fn main(xs: List<Int>) -> Int { 1 + total(xs) }");

    for body in [
        "let k = total(xs); let g = fn(y: Int) => y + k; g(1)",
        "let k = total(xs); (fn(y: Int) => y + k)(1)",
        "let k = total(xs); let g = fn(y: Int) => y + k; let h = g; h(1)",
        "let k = total(xs); apply(fn(y) => y + k, 1)",
    ] {
        assert_eq!(
            program(&format!("fn main(xs: List<Int>) -> Int {{ {body} }}")),
            written,
            "{body}"
        );
    }
}

/// A value that isn't written as something cheap, but is compiled to a constant, is substituted
/// and folded with what uses it, so the program is the same as with the constant written out.
#[test]
fn value_that_folds_is_substituted() {
    let large = "((1 + 2) * (3 + 4) + (5 + 6) * (7 + 8)) * ((9 + 10) * (11 + 12) + 13)";
    let written = program("fn main(xs: List<Int>) -> List<Int> { map(xs, fn(x) => x + 83700) }");

    let binding = format!(
        "fn main(xs: List<Int>) -> List<Int> {{ let k = {large}; map(xs, fn(x) => x + k) }}"
    );
    assert_eq!(program(&binding), written);

    let constant = format!(
        "const LARGE: Int = {large};\nfn main(xs: List<Int>) -> List<Int> {{ map(xs, fn(x) => x + LARGE) }}"
    );
    assert_eq!(program(&constant), written);

    // In a lambda that's called twice, which is folded with what's added to it
    let twice = format!(
        "const LARGE: Int = {large};\nfn main(x: Int) -> Int {{ apply_twice(fn(y) => y + LARGE, x) }}"
    );
    let written = program("fn main(x: Int) -> Int { apply_twice(fn(y) => y + 83700, x) }");
    assert_eq!(program(&twice), written);
}

/// A lambda that's called for each item only captures the value that's bound before it, rather
/// than what the value is found from, whether the value is referenced once or twice.
#[test]
fn lambda_only_captures_a_bound_value() {
    let once = program(
        "fn main(xs: List<Int>, ys: List<Int>) -> List<Int> { let k = total(ys); map(xs, fn(x) => x + k) }",
    );
    let passed = program("fn main(xs: List<Int>, k: Int) -> List<Int> { map(xs, fn(x) => x + k) }");

    // The closure is the same as one that captures a parameter: `(c (q . 2) (c (q . body) ...`
    let closure = |program: &str| {
        let start = program
            .find("(c (q . 2) (c (q 1 ")
            .expect("the program builds a closure");
        program[start..].to_string()
    };

    assert_eq!(closure(&once).matches("(q . 4)").count(), 1, "{once}");
    assert_eq!(closure(&passed).matches("(q . 4)").count(), 1, "{passed}");

    let twice = program(
        "fn main(xs: List<Int>, ys: List<Int>) -> List<Int> { let k = total(ys); map(xs, fn(x) => x + k + k) }",
    );

    assert_eq!(closure(&twice).matches("(q . 4)").count(), 1, "{twice}");
}

/// The default value of a struct's field, or the value of an `inline let`, is lowered where it's
/// referenced, so what it refers to is captured in the same order as if it were written there.
#[test]
fn inline_value_is_captured_like_the_value_in_place() {
    let values = [
        "f(3)",
        "f(3) + g(4)",
        "h(f(3))",
        "{ let a = f(3); a + a }",
        "K + f(3)",
        "for z in [1, 2] fold t = 0 { t + f(z) }",
    ];

    let bodies = [
        "g(x) + VALUE + h(x)",
        "VALUE + g(x) + h(x)",
        "g(x) + h(x) + VALUE",
        "n + VALUE + g(x) + m",
        "g(x) + h(for y in xs fold s = 0 { s + VALUE + y + n })",
        "h(for y in xs fold s = 0 { s + g(y) + VALUE + n }) + g(x)",
        "h(total(for y in xs { g(y) + VALUE + x })) + f(x)",
        "h(total(map(xs, fn(y) => g(y) + VALUE + x))) + f(x)",
    ];

    let contexts = [
        "for x in xs { BODY + m + n }",
        "map(xs, fn(x) => BODY + m + n)",
        "[for x in xs fold acc = 0 { acc + BODY + m + n }]",
    ];

    for value in values {
        for body in bodies {
            for context in contexts {
                let main = |prelude: &str, value: &str| {
                    let body = context.replace("BODY", &body.replace("VALUE", value));
                    format!(
                        "fn main(xs: List<Int>) -> List<Int> {{ let n = f(total(xs)); let m = g(n); {prelude}{body} }}"
                    )
                };

                let in_place = format!("(Plain {{ _scale: 0, value: {value} }}).value");
                let expected = program(&main("", &in_place));

                let default = format!(
                    "struct Default {{ _scale: Int, value: Int = {value} }}\n{}",
                    main("", "(Default { _scale: 0 }).value")
                );
                assert_eq!(program(&default), expected, "{default}");

                let inline = main(&format!("inline let v = {in_place}; "), "v");
                assert_eq!(program(&inline), expected, "{inline}");
            }
        }
    }
}
