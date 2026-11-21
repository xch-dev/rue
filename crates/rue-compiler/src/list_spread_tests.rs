//! A list that spreads a value before its last item has the item types of what's spread, found
//! by following its pairs (see `list_item_types`).

use crate::test_utils::compile;

/// A spread tuple longer than the pairs that are followed has `Any` for its items past them, so the
/// list's type isn't narrower than what it holds: here the `"x"` at the end would otherwise be
/// left out, and the list would be accepted as a `List<Int>`.
#[test]
fn items_past_the_limit_are_any() {
    let zeros = vec!["0"; 1100].join(", ");
    let source = format!(
        "fn id<T>(value: T) -> T {{ value }}\n\
         fn main() -> List<Int> {{\n    let tuple = id([{zeros}, \"x\"]);\n    [...tuple, 1]\n}}\n"
    );

    // `Any` is shown as the recursive type it is
    let errors = compile(&source).errors();
    assert_eq!(
        errors,
        [
            r#"2:24 IncompatibleType("List<0 | Atom | 2 @ (Atom | <2>, Atom | <2>) | 1>", "List<Int>")"#
        ]
    );
}

/// A tuple shorter than the limit has the type of each of its items.
#[test]
fn items_of_a_short_tuple() {
    let zeros = vec!["0"; 100].join(", ");
    let source = format!(
        "fn id<T>(value: T) -> T {{ value }}\n\
         fn main() -> List<Int> {{\n    let tuple = id([{zeros}, \"x\"]);\n    [...tuple, 1]\n}}\n"
    );

    let errors = compile(&source).errors();
    assert_eq!(
        errors,
        [r#"2:24 UnassignableType("List<0 | \"x\" | 1>", "List<Int>")"#]
    );
}

/// Each member of a union is followed on its own, so a union with a long tuple in it isn't slow,
/// and has the item types of both members.
#[test]
fn union_with_a_long_tuple() {
    let names = vec!["\"a\""; 300].join(", ");
    let source = format!(
        "fn id<T>(value: T) -> T {{ value }}\n\
         fn main(c: Bool, list: List<Int>) -> List<Int> {{\n    \
         let spread = if c {{ list }} else {{ id([{names}]) }};\n    [...spread, 1]\n}}\n"
    );

    let errors = compile(&source).errors();
    assert_eq!(
        errors,
        [r#"2:48 UnassignableType("List<Int | \"a\">", "List<Int>")"#]
    );
}
