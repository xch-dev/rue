use expect_test::expect;

use crate::test_utils::compile;

/// The order of a union's members in an inferred type comes from the order of the generics that
/// `free_generics` finds, which used to be a `HashSet`'s, so it changed from run to run. Each
/// `HashSet` gets new random keys, so compiling the same source again in this process would give
/// the other order about half the time. The inferred union shows in the error, since `main`
/// returns `Int`, and compiling again must give the same error each time.
#[test]
fn inferred_union_order_is_deterministic() {
    let source = "
fn swap<A, B>(pair: (A, B)) -> (B, A) {
    (pair.rest, pair.first)
}

fn same<T>(function: fn(a: T) -> T, produce: fn() -> T) -> T {
    function(produce())
}

fn main() -> Int {
    same(swap, fn() => (4, 7))
}
";

    let first = compile(source).errors();

    for _ in 0..32 {
        assert_eq!(compile(source).errors(), first);
    }

    expect![[r#"
        [
            "10:18 IncompatibleType(\"(Int | 7 | 4, Int | 7 | 4) | (4, 7)\", \"Int\")",
        ]
    "#]]
    .assert_debug_eq(&first);
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
