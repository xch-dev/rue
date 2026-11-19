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
/// Main took about 505,000 steps here (3,300,000 for 120 arguments), about 447,000 with only the
/// substitution put back, and about 162,000 with only the instances' cache turned off. Steps don't
/// count the walks `Inference::resolve_instances` makes, so its index isn't measured here.
#[test]
fn passed_generic_functions_are_instantiated_once_per_inference() {
    let steps = steps(&passed_generic_functions(60));
    assert!(steps < 125_000, "took {steps} steps");
}

/// A chain of `count` generics where each one is passed `first_of` for every later one, so that
/// the lower bounds passed down through `first_of`'s compound upper bound `(T, T)` reach each
/// generic through every earlier one, one round at a time.
fn dense_compound_chain(count: usize) -> String {
    let generics: Vec<String> = (0..count).map(|i| format!("G{i}")).collect();
    let mut params = String::new();
    let mut args = String::new();

    for i in 0..count {
        for j in i + 1..count {
            params.push_str(&format!("    f{i}_{j}: fn(a: G{i}) -> G{j},\n"));
            args.push_str("        first_of,\n");
        }
    }

    let pairs: String = (1..=count)
        .map(|k| format!("    let p{k} = (p{}, p{});\n", k - 1, k - 1))
        .collect();

    format!(
        "
fn first_of<T>(pair: (T, T)) -> T {{
    pair.first
}}

fn chain<{}>(
    value: G0,
{params}) -> G0 {{
    value
}}

fn main() -> Int {{
    let p0 = 1;
{pairs}    let _result = chain(
        p{count},
{args}    );
    1
}}
",
        generics.join(", "),
    )
}

/// Each round of passing lower bounds down compared every member of each generic's expanded lower
/// bounds again, though only the new ones can pass on anything new. Main took about 3,150,000
/// steps here, and 684,000 for 12 generics.
#[test]
fn dense_compound_chain_compares_new_members_once() {
    let steps = steps(&dense_compound_chain(16));
    assert!(steps < 1_800_000, "took {steps} steps");
}
