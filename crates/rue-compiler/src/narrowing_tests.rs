//! Narrowing: the type a value has at each place it's used, after type guards in conditions,
//! early returns and assertions.

use crate::test_utils::compile;

/// Compiles a function `main` with the given parameters, return type and body, and returns the
/// type of each reference to `x` in it.
fn x_types(parameters: &str, return_type: &str, body: &str) -> Vec<String> {
    let mut compiled = compile(&format!(
        "fn main({parameters}) -> {return_type} {{\n{body}\n}}\n"
    ));
    compiled.assert_no_errors();
    compiled.reference_types("main", "x")
}

#[test]
fn if_else_narrows_each_branch() {
    assert_eq!(
        x_types(
            "x: Int | (Int, Int)",
            "Int",
            "if x is Int { x } else { x.first }",
        ),
        ["`x`: Int | (Int, Int)", "`x`: Int", "`x`: (Int, Int)"]
    );
}

#[test]
fn narrowing_ends_with_the_if_expression() {
    assert_eq!(
        x_types(
            "x: Int | (Int, Int)",
            "Int | (Int, Int)",
            "let y = if x is Int { x } else { 0 }; x",
        ),
        ["`x`: Int | (Int, Int)", "`x`: Int", "`x`: Int | (Int, Int)"]
    );
}

#[test]
fn early_return_narrows_the_rest_of_the_block() {
    assert_eq!(
        x_types(
            "x: Int | (Int, Int)",
            "Int",
            "if x is (Int, Int) { return x.first; } x",
        ),
        ["`x`: Int | (Int, Int)", "`x`: (Int, Int)", "`x`: Int"]
    );
}

#[test]
fn raise_narrows_the_rest_of_the_block() {
    assert_eq!(
        x_types("x: Bool", "true", "if !(x is true) { raise; } x"),
        ["`x`: Bool", "`x`: true"]
    );
}

#[test]
fn assert_narrows_the_rest_of_the_block() {
    assert_eq!(
        x_types("x: Int | (Int, Int)", "Int", "assert x is Int; x"),
        ["`x`: Int | (Int, Int)", "`x`: Int"]
    );
}

#[test]
fn and_narrows_both_operands_in_the_then_branch() {
    let mut compiled = compile(
        "
        fn main(x: Int | (Int, Int), y: Int | (Int, Int)) -> Int {
            if x is Int && y is Int {
                x + y
            } else {
                0
            }
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(
        compiled.reference_types("main", "x"),
        ["`x`: Int | (Int, Int)", "`x`: Int"]
    );
    assert_eq!(
        compiled.reference_types("main", "y"),
        ["`y`: Int | (Int, Int)", "`y`: Int"]
    );
}

#[test]
fn and_narrows_its_right_operand_by_its_left() {
    assert_eq!(
        x_types("x: Int | (Int, Int)", "Bool", "x is Int && x > 0"),
        ["`x`: Int | (Int, Int)", "`x`: Int"]
    );
}

#[test]
fn and_does_not_narrow_the_else_branch() {
    let mut compiled = compile(
        "
        fn main(x: Int | (Int, Int), y: Int | (Int, Int)) -> Int {
            if x is Int && y is Int {
                0
            } else {
                x
            }
        }
        ",
    );

    // The body's type still includes the pair, so it can't be returned as `Int`
    assert_eq!(
        compiled.errors(),
        ["2:66 UnconstrainableComparison(\"0 | Int | (Int, Int)\", \"Int\")"]
    );
    assert_eq!(
        compiled.reference_types("main", "x"),
        ["`x`: Int | (Int, Int)", "`x`: Int | (Int, Int)"]
    );
}

#[test]
fn or_narrows_both_operands_in_the_else_branch() {
    let mut compiled = compile(
        "
        fn main(x: Int | (Int, Int), y: Int | (Int, Int)) -> Int {
            if x is (Int, Int) || y is (Int, Int) {
                return 0;
            }
            x + y
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(
        compiled.reference_types("main", "x"),
        ["`x`: Int | (Int, Int)", "`x`: Int"]
    );
    assert_eq!(
        compiled.reference_types("main", "y"),
        ["`y`: Int | (Int, Int)", "`y`: Int"]
    );
}

#[test]
fn or_narrows_its_right_operand_by_the_negation_of_its_left() {
    assert_eq!(
        x_types("x: Int | (Int, Int)", "Bool", "x is (Int, Int) || x > 0"),
        ["`x`: Int | (Int, Int)", "`x`: Int"]
    );
}

#[test]
fn bitwise_and_narrows_both_operands_in_the_then_branch() {
    let mut compiled = compile(
        "
        fn main(x: Int | (Int, Int), y: Int | (Int, Int)) -> Int {
            if (x is Int) & (y is Int) {
                x + y
            } else {
                0
            }
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(
        compiled.reference_types("main", "x"),
        ["`x`: Int | (Int, Int)", "`x`: Int"]
    );
    assert_eq!(
        compiled.reference_types("main", "y"),
        ["`y`: Int | (Int, Int)", "`y`: Int"]
    );
}

#[test]
fn bitwise_or_narrows_both_operands_in_the_else_branch() {
    let mut compiled = compile(
        "
        fn main(x: Int | (Int, Int), y: Int | (Int, Int)) -> Int {
            if (x is (Int, Int)) | (y is (Int, Int)) {
                return 0;
            }
            x + y
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(
        compiled.reference_types("main", "x"),
        ["`x`: Int | (Int, Int)", "`x`: Int"]
    );
    assert_eq!(
        compiled.reference_types("main", "y"),
        ["`y`: Int | (Int, Int)", "`y`: Int"]
    );
}

#[test]
fn not_swaps_the_branches() {
    assert_eq!(
        x_types(
            "x: Int | (Int, Int)",
            "Int",
            "if !(x is Int) { x.first } else { x }",
        ),
        ["`x`: Int | (Int, Int)", "`x`: (Int, Int)", "`x`: Int"]
    );
}

#[test]
fn negating_a_value_does_not_narrow_it() {
    let mut compiled = compile(
        "
        fn main(x: Bool) -> true {
            if (!x) is false {
                raise;
            }
            x
        }
        ",
    );

    assert_eq!(
        compiled.errors(),
        ["2:34 UnconstrainableComparison(\"Bool\", \"true\")"]
    );
    assert_eq!(
        compiled.reference_types("main", "x"),
        ["`x`: Bool", "`x`: Bool"]
    );
}

#[test]
fn narrowing_applies_inside_of_nested_blocks() {
    assert_eq!(
        x_types(
            "x: Int | (Int, Int)",
            "Int",
            "if x is Int { let y = { x + 1 }; y } else { 0 }",
        ),
        ["`x`: Int | (Int, Int)", "`x`: Int"]
    );
}

#[test]
fn guarding_a_pair_field_narrows_the_whole_value() {
    assert_eq!(
        x_types(
            "x: (Int | (Int, Int), Int)",
            "Int",
            "if x.first is Int { x.first + x.rest } else { x.rest }",
        ),
        [
            "`x`: (Int | (Int, Int), Int)",
            "`x`: (Int, Int)",
            "`x`: (Int, Int)",
            "`x`: ((Int, Int), Int)",
        ]
    );
}

#[test]
fn guarding_a_struct_field_narrows_the_whole_value() {
    let mut compiled = compile(
        "
        struct Point {
            x: Int | (Int, Int),
            y: Int,
        }

        fn main(point: Point) -> Int {
            if point.x is Int {
                point.x + point.y
            } else {
                point.x.first
            }
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(
        compiled.reference_types("main", "point"),
        [
            "`point`: Point",
            "`point`: Point { x: Int, y: Int }",
            "`point`: Point { x: Int, y: Int }",
            "`point`: Point { x: (Int, Int), y: Int }",
        ]
    );
}

#[test]
fn guards_between_types_that_do_not_overlap_are_errors() {
    let compiled = compile(
        "
        fn main(x: Int) -> Bool {
            x is (Int, Int)
        }
        ",
    );

    assert_eq!(
        compiled.errors(),
        ["3:13 IncompatibleGuard(\"Int\", \"(Int, Int)\")"]
    );
}

#[test]
fn guards_cannot_check_function_or_generic_types() {
    let compiled = compile(
        "
        fn main<T>(x: Int | fn() -> Int, y: T | Int) -> Bool {
            x is fn() -> Int && y is T
        }
        ",
    );

    assert_eq!(
        compiled.errors(),
        ["3:13 FunctionTypeCheck", "3:33 GenericTypeCheck"]
    );
}
