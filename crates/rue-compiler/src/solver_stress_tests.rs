//! Step bounds for solving the generics of very wide calls. Each one counts the recursive
//! operations on types (`rue_types::count_steps`) while a generated program compiles, so a change
//! that makes the solver repeat work fails here rather than only making the stress snapshots slow.

use std::fmt::Write;

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
            writeln!(params, "    f{i}_{j}: fn(a: G{i}) -> G{j},").unwrap();
            args.push_str("        first_of,\n");
        }
    }

    let mut pairs = String::new();

    for k in 1..=count {
        writeln!(pairs, "    let p{k} = (p{}, p{});", k - 1, k - 1).unwrap();
    }

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
/// steps here (684,000 for 12 generics), about 2,890,000 with only the solver's instances cached,
/// and about 1,450,000 without the cache of which bounds are equivalent.
#[test]
fn dense_compound_chain_compares_new_members_once() {
    let steps = steps(&dense_compound_chain(16));
    assert!(steps < 1_380_000, "took {steps} steps");
}

/// A chain of `count` generics that each take the pair `(5, 5)` and are inferred through every
/// earlier one by `identity`, with `five_pair` bounding the last, so each one's union of lower
/// bounds is built from the unions of all the ones before it.
fn dense_pair_chain(count: usize) -> String {
    let generics: Vec<String> = (0..count).map(|i| format!("G{i}")).collect();
    let mut params = String::new();

    for i in 0..count {
        writeln!(params, "    v{i}: G{i},").unwrap();
    }

    let mut args = "        (5, 5),\n".repeat(count);

    for i in 0..count {
        for j in i + 1..count {
            writeln!(params, "    f{i}_{j}: fn(a: G{i}) -> G{j},").unwrap();
            args.push_str("        identity,\n");
        }
    }

    format!(
        "
fn identity<U>(value: U) -> U {{
    value
}}

fn five_pair(value: (5, 5)) -> Int {{
    value.first
}}

fn chain<{}>(
{params}    last: fn(b: G{}) -> Int,
) -> Int {{
    1
}}

fn main() -> Int {{
    chain(
{args}        five_pair,
    )
}}
",
        generics.join(", "),
        count - 1,
    )
}

/// Each generic's union held a copy of `(5, 5)` for every path to it through the earlier ones, so
/// it doubled in size with each generic, and comparing two of them compared every member of one
/// with every member of the other. Main took about 3,040,000 steps here, and 269,000 for 8
/// generics.
#[test]
fn dense_pair_chain_unions_stay_flat() {
    let steps = steps(&dense_pair_chain(10));
    assert!(steps < 100_000, "took {steps} steps");
}
