use expect_test::expect;

use crate::test_utils::compile;

/// The order of a union's members in an inferred type comes from the order of the generics that
/// `free_generics` finds, which used to be a `HashSet`'s, so it changed from run to run. Each
/// `HashSet` gets new random keys, so compiling the same source again in this process would give
/// the other order about half the time.
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
    same(swap, fn() => (4, 7))
}
";

    let first = compile(source).errors();

    for _ in 0..32 {
        assert_eq!(compile(source).errors(), first);
    }

    expect![[r#"
        [
            "11:10 IncompatibleType(\"fn<A, B>(pair: (A, B)) -> (B, A)\", \"fn(a: (Int | 7 | 4, Int | 7 | 4) | (4, 7)) -> (Int | 7 | 4, Int | 7 | 4) | (4, 7)\")",
        ]
    "#]]
    .assert_debug_eq(&first);
}
