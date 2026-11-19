//! Step bounds for solving the generics of very wide calls. Each one counts the recursive
//! operations on types (`rue_types::count_steps`) while a generated program compiles, so a change
//! that makes the solver repeat work fails here rather than only making the stress snapshots slow.

use rue_types::count_steps;

use crate::test_utils::compile;

/// A call that passes `swap` as each of `count` arguments, whose generics all take the same value,
/// so that each argument's instance is solved with the others.
fn passed_generic_functions(count: usize) -> String {
    let generics: Vec<String> = (0..count).map(|i| format!("B{i}")).collect();
    let params: Vec<String> = (0..count)
        .map(|i| format!("    f{i}: fn(a: A) -> B{i},\n"))
        .collect();
    let args = "        swap,\n".repeat(count);

    format!(
        "
fn swap<T, U>(pair: (T, U)) -> (U, T) {{
    (pair.rest, pair.first)
}}

fn fan<A, {}>(
    value: A,
{}) -> A {{
    value
}}

fn main() -> Int {{
    let result = fan(
        (1, nil),
{args}    );
    result.first
}}
",
        generics.join(", "),
        params.concat(),
    )
}

fn steps(source: &str) -> usize {
    let (compiled, steps) = count_steps(|| compile(source));
    assert_eq!(compiled.errors(), Vec::<String>::new());
    steps
}

/// Each solve used to pass bounds on to the instances again for the same inference, up to three
/// times before each argument, and the argument order substituted each mapping once per generic.
/// Main took about 505,000 steps here, and 3,300,000 for 120 arguments.
#[test]
fn passed_generic_functions_are_instantiated_once_per_inference() {
    let steps = steps(&passed_generic_functions(60));
    assert!(steps < 150_000, "took {steps} steps");
}
