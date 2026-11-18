use rue_types::with_comparison_step_limit;

use crate::test_utils::compile_with;

/// A comparison gives up once it takes too many steps, and each place that compares types reports
/// it as too complex. No program in the tests takes anywhere near the real limit since #362's step
/// 13b.3 (`tests/types/comparison_too_complex.rue` used to), so this one runs with a low limit.
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

    let compiled = with_comparison_step_limit(10, || compile_with(source, |_| {}));

    assert_eq!(
        compiled.errors(),
        [
            "7:25 TypeComparisonTooComplex(\"A\", \"B\")",
            "10:16 TypeComparisonTooComplex(\"A\", \"B\")",
        ]
    );
}
