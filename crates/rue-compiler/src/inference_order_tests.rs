use expect_test::expect;

use crate::test_utils::compile;

/// The order of a union's members in an inferred type comes from the order of the generics that
/// `free_generics` finds, which used to be a `HashSet`'s, so it changed from run to run. Each
/// `HashSet` gets new random keys, so compiling the same source again in this process would give
/// the other order about half the time. The union is read as inference built it, since a union is
/// displayed without the members that others include, in whatever order they are.
#[test]
fn inferred_union_order_is_deterministic() {
    let source = "
fn swap<A, B>(pair: (A, B)) -> (B, A) {
    (pair.rest, pair.first)
}

fn same<T>(function: fn(a: T) -> T, produce: fn() -> T) -> T {
    function(produce())
}

fn main() -> Any {
    let result = same(swap, fn() => (4, 7));
    result
}
";

    let types = |source| compile(source).reference_types_exact("main", "result");
    let first = types(source);

    for _ in 0..32 {
        assert_eq!(types(source), first);
    }

    expect![[r#"
        [
            "`result`: (Int | 7 | 4, Int | 7 | 4) | (4, 7)",
        ]
    "#]]
    .assert_debug_eq(&first);
}

/// How many copies of a compound member inference keeps in a union that it builds (#362): at most
/// two of a member that's reached from more than one argument, and members that only look alike
/// are kept apart. A union is displayed with each of these once, so they're read as inference
/// built them.
#[test]
fn inferred_union_keeps_at_most_two_copies_of_a_member() {
    let source = "
fn pick<T>(_first: T, second: T) -> T {
    second
}

fn pick3<T>(_first: T, _second: T, third: T) -> T {
    third
}

fn main() -> Any {
    let either: Int | (Int, Int) = 1;
    let pair: (Int, Int) = (1, 2);
    let two = pick(either, pair);
    let three = pick3(pair, either, pair);

    let shorter: (Int, 1 | 2) = (1, 1);
    let same: (Int, 1 | 2) = (1, 2);
    let longer: (Int, 1 | 2 | 3) = (1, 3);
    let lengths = pick3(shorter, same, longer);

    let backward: (Int, 2 | 1) = (1, 1);
    let orders = pick3(shorter, same, backward);

    (two, (three, (lengths, orders)))
}
";

    let mut compiled = compile(source);
    assert_eq!(compiled.errors(), Vec::<String>::new());

    let types = ["two", "three", "lengths", "orders"]
        .map(|name| compiled.reference_types_exact("main", name).join(", "));

    expect![[r#"
        [
            "`two`: Int | (Int, Int) | (Int, Int)",
            "`three`: (Int, Int) | Int | (Int, Int)",
            "`lengths`: (Int, 1 | 2) | (Int, 1 | 2) | (Int, 1 | 2 | 3)",
            "`orders`: (Int, 1 | 2) | (Int, 1 | 2) | (Int, 2 | 1)",
        ]
    "#]]
    .assert_debug_eq(&types);

    // And as they're displayed
    let types =
        ["two", "three", "lengths", "orders"].map(|name| compiled.reference_types("main", name));

    expect![[r#"
        [
            [
                "`two`: Int | (Int, Int)",
            ],
            [
                "`three`: (Int, Int) | Int",
            ],
            [
                "`lengths`: (Int, 1 | 2 | 3)",
            ],
            [
                "`orders`: (Int, 1 | 2)",
            ],
        ]
    "#]]
    .assert_debug_eq(&types);
}

/// Swapping `(4, 7)` gives `(7, 4)`, which `T`'s first member `(Int | 7 | 4, Int | 7 | 4)` holds
/// with `A` and `B` both `Int | 7 | 4`. Before #362's step 13g, the second member, `(4, 7)`, also
/// matched, and its own bounds conflicted, which rejected this
#[test]
fn swapped_pair_matches_a_member_of_the_inferred_union() {
    let source = "
fn swap<A, B>(pair: (A, B)) -> (B, A) {
    (pair.rest, pair.first)
}

fn same<T>(function: fn(a: T) -> T, produce: fn() -> T) -> T {
    function(produce())
}

fn main() -> Any {
    same(swap, fn() => (4, 7))
}
";

    assert_eq!(compile(source).errors(), Vec::<String>::new());
}
