//! Const evaluation: the values `const { ... }` expressions evaluate to at compile time, and the
//! errors for ones that can't be evaluated.

use crate::test_utils::{compile, compile_with};

/// Compiles a function `main` whose body is the given const expression, and returns its value.
fn evaluate(return_type: &str, expr: &str) -> String {
    let compiled = compile(&format!("fn main() -> {return_type} {{\n    {expr}\n}}\n"));
    compiled.assert_no_errors();

    let mut results = compiled.const_results();
    assert_eq!(results.len(), 1, "{results:?}");

    results.remove(0).1
}

#[test]
fn arithmetic_follows_precedence() {
    assert_eq!(evaluate("Int", "const { 1 + 2 * 3 - 4 }"), "3");
    assert_eq!(evaluate("Int", "const { (1 + 2) * 3 }"), "9");
    assert_eq!(evaluate("Int", "const { -(5 - 8) }"), "3");
}

#[test]
fn integers_do_not_overflow() {
    assert_eq!(
        evaluate(
            "Int",
            "const { 18446744073709551616 * 18446744073709551616 }"
        ),
        "340282366920938463463374607431768211456"
    );
    assert_eq!(
        evaluate("Int", "const { 0 - 18446744073709551616 - 1 }"),
        "-18446744073709551617"
    );
}

#[test]
fn division_rounds_toward_negative_infinity() {
    assert_eq!(evaluate("Int", "const { 7 / 2 }"), "3");
    assert_eq!(evaluate("Int", "const { -7 / 2 }"), "-4");
    assert_eq!(evaluate("Int", "const { -7 % 2 }"), "1");
    assert_eq!(evaluate("Int", "const { 7 % -2 }"), "-1");
}

#[test]
fn comparisons_and_conditionals_are_evaluated() {
    assert_eq!(
        evaluate("Int", "const { if 3 > 2 { 10 } else { 20 } }"),
        "10"
    );
    assert_eq!(evaluate("Bool", "const { 1 == 2 }"), "()");
    assert_eq!(evaluate("Bool", "const { 2 >= 2 }"), "1");
}

#[test]
fn pairs_and_lists_are_evaluated() {
    assert_eq!(evaluate("(Int, Int)", "const { (1, 2) }"), "(1 . 2)");
    assert_eq!(
        evaluate("List<Int>", "const { [1, 2, 3] }"),
        "(1 . (2 . (3 . ())))"
    );
}

#[test]
fn constants_and_functions_can_be_used() {
    let compiled = compile(
        "
        const BASE: Int = 5;

        fn factorial(value: Int) -> Int {
            if value <= 1 {
                1
            } else {
                value * factorial(value - 1)
            }
        }

        fn main() -> Int {
            const { factorial(BASE) + BASE }
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(
        compiled.const_values(),
        ["`const { factorial(BASE) + BASE }` = 125"]
    );
}

#[test]
fn const_expressions_in_called_functions_and_nested_const_expressions_are_evaluated() {
    let compiled = compile(
        "
        fn inner() -> Int {
            const { 41 }
        }

        fn main() -> Int {
            const { inner() + 1 } + const { const { 2 } * 3 }
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(
        compiled.const_values(),
        [
            "`const { 41 }` = 41",
            "`const { inner() + 1 }` = 42",
            "`const { const { 2 } * 3 }` = 6",
            "`const { 2 }` = 2",
        ]
    );
}

#[test]
fn const_expressions_reached_through_calls_are_evaluated_once() {
    // Counting down from 100 costs about 133,000, so the limit allows it once but not twice;
    // `inner()` must use the cached value of its const expression rather than run it again
    let compiled = compile_with(
        "
        fn count(value: Int) -> Int {
            if value == 0 {
                0
            } else {
                count(value - 1)
            }
        }

        fn inner() -> Int {
            const { count(100) }
        }

        fn main() -> Int {
            const { inner() + inner() }
        }
        ",
        |options| options.const_eval_max_cost = 200_000,
    );

    compiled.assert_no_errors();
    assert_eq!(
        compiled.const_values(),
        [
            "`const { count(100) }` = ()",
            "`const { inner() + inner() }` = ()",
        ]
    );
}

#[test]
fn division_by_zero_fails() {
    let compiled = compile("fn main() -> Int {\n    const { 1 / 0 }\n}\n");

    assert_eq!(
        compiled.errors(),
        ["2:5 ConstEvalFailed(\"Division by zero\")"]
    );
    assert_eq!(compiled.const_values(), ["`const { 1 / 0 }` = ()"]);
}

#[test]
fn raise_fails() {
    let compiled = compile("fn main() -> Int {\n    const {\n        raise 42;\n    }\n}\n");

    assert_eq!(compiled.errors(), ["2:5 ConstEvalFailed(\"clvm raise\")"]);
}

#[test]
fn exceeding_the_cost_limit_fails() {
    let compiled = compile_with(
        "
        fn count(value: Int) -> Int {
            if value == 0 {
                0
            } else {
                count(value - 1)
            }
        }

        fn main() -> Int {
            const { count(1000) }
        }
        ",
        |options| options.const_eval_max_cost = 10_000,
    );

    assert_eq!(compiled.errors(), ["11:13 ConstEvalCostExceeded(10000)"]);
}

#[test]
fn runtime_values_cannot_be_used() {
    let compiled = compile(
        "
        fn main(parameter: Int) -> Int {
            let binding = 1;
            const { parameter + 1 } + const { binding + 1 }
        }
        ",
    );

    assert_eq!(
        compiled.errors(),
        [
            "4:13 ConstRuntimeDependency(\"parameter\")",
            "4:39 ConstRuntimeDependency(\"binding\")",
        ]
    );
}

#[test]
fn bindings_inside_of_the_expression_can_be_used() {
    assert_eq!(
        evaluate(
            "Int",
            "const { let (a, b) = (3, 4); let add = fn(c: Int) => a + b + c; add(1) }",
        ),
        "8"
    );
}

#[test]
fn const_expressions_cannot_depend_on_themselves() {
    let compiled = compile(
        "
        fn main() -> Int {
            cycle_a()
        }

        fn cycle_a() -> Int {
            const { cycle_b() }
        }

        fn cycle_b() -> Int {
            const { cycle_a() }
        }
        ",
    );

    assert_eq!(compiled.errors(), ["11:13 RecursiveConstExpr"]);
}

#[test]
fn failed_dependencies_are_only_reported_once() {
    let compiled = compile(
        "
        fn main() -> Int {
            const { failing() + 1 }
        }

        fn failing() -> Int {
            const { 1 / 0 }
        }
        ",
    );

    assert_eq!(
        compiled.errors(),
        ["7:13 ConstEvalFailed(\"Division by zero\")"]
    );
    assert_eq!(
        compiled.const_values(),
        ["`const { failing() + 1 }` = ()", "`const { 1 / 0 }` = ()"]
    );
}

#[test]
fn nothing_is_evaluated_when_there_are_other_errors() {
    let compiled = compile(
        "
        fn main() -> Int {
            const { 1 + 2 } + missing
        }
        ",
    );

    assert_eq!(compiled.errors(), ["3:31 UndeclaredSymbol(\"missing\")"]);
    assert_eq!(compiled.const_values(), ["`const { 1 + 2 }` = unevaluated"]);
}
