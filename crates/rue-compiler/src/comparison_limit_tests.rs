use rue_types::with_comparison_step_limit;

use crate::test_utils::compile_with;

/// A comparison gives up once it takes too many steps, and each place that compares types reports
/// it as too complex. No program in the tests takes anywhere near the real limit since #362's step
/// 13b.3 (`tests/types/comparison_too_complex.rue` used to), so this one runs with a low limit:
/// comparing `A` with `B` takes about 20 steps, and the other comparisons here at most about 7.
#[test]
fn comparisons_that_take_too_many_steps_are_too_complex() {
    let source = "
        type A = (Int, B) | nil;
        type B = (Int, A) | nil;

        fn main(x: A) -> Int {
            // Assigning, which compares the types
            let _y: B = x;

            // Checking at runtime, which compares the types to work out what needs to be checked
            if x is B { 1 } else { 0 }
        }
    ";

    let compiled = with_comparison_step_limit(15, || compile_with(source, |_| {}));

    assert_eq!(
        compiled.errors(),
        [
            "7:25 TypeComparisonTooComplex(\"A\", \"B\")",
            "10:16 TypeComparisonTooComplex(\"A\", \"B\")",
        ]
    );
}

/// A field whose type is inferred from its default value, and so is exactly that value's type, is
/// compared with what an initializer gives it on its own, to report a value that doesn't fit with
/// its own diagnostic (see `struct_inferred_default_errors.rue`). Giving up there is reported for
/// the field, as an assignment's would be, which is all this tells apart: reporting it through
/// the assignment instead gives the same error.
#[test]
fn inferred_fields_that_take_too_many_steps_are_too_complex() {
    let source = "
        type A = (Int, B) | nil;
        type B = (Int, A) | nil;

        struct Holder {
            value = nil as B,
        }

        fn main(x: A) -> Holder {
            Holder { value: x }
        }
    ";

    let compiled = with_comparison_step_limit(15, || compile_with(source, |_| {}));
    assert_eq!(
        compiled.errors(),
        ["10:22 TypeComparisonTooComplex(\"A\", \"B\")"]
    );

    // Within the real limit, `A` and `B` are the same type, so the field takes it
    assert!(compile_with(source, |_| {}).errors().is_empty());
}

/// A field that a struct update copies from the value it spreads is compared with the field's type
/// on its own, so giving up there is reported for the spread.
#[test]
fn spread_fields_that_take_too_many_steps_are_too_complex() {
    let source = "
        type A = (Int, B) | nil;
        type B = (Int, A) | nil;

        struct Holder<T> {
            value: T,
            other: Int,
        }

        fn main(source: Holder<A>) -> Int {
            let updated = Holder::<B> { ...source, other: 1 };
            updated.other
        }
    ";

    let compiled = with_comparison_step_limit(15, || compile_with(source, |_| {}));
    assert_eq!(
        compiled.errors(),
        ["11:41 TypeComparisonTooComplex(\"A\", \"B\")"]
    );

    // Within the real limit, the copied field fits
    assert!(compile_with(source, |_| {}).errors().is_empty());
}
