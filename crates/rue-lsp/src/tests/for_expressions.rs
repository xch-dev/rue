//! What the language server makes of `for` expressions: the item and the result of a fold are
//! bindings like any other, in the scopes of the parts of the `for` that they're in.

use std::fmt::Write;
use std::sync::atomic::Ordering;

use expect_test::{Expect, expect};

use super::TestProject;
use crate::cache::HintOptions;
use crate::lock;

/// A project of one file that compiles, so that a test is about what the code means to be.
fn compiled(source: &str) -> TestProject {
    let project = TestProject::new(&[("main.rue", source)]);
    let diagnostics = project.diagnostics();
    assert!(!diagnostics.contains(" Error"), "{diagnostics}");
    project
}

/// Renames what's at the cursor of each of the bodies of `main`, which has the given parameters.
#[allow(clippy::needless_pass_by_value)]
fn check_renames(parameters: &str, cases: &[(&str, &str)], expect: Expect) {
    let mut output = String::new();

    for (body, new_name) in cases {
        let source = format!(
            "fn main({parameters}) -> Any {{\n    {body}\n}}\n\nfn size(_list: List<Int>) -> Int {{\n    1\n}}\n"
        );
        let renamed = compiled(&source).rename(new_name);
        writeln!(output, "== {body} => {new_name}\n{renamed}").unwrap();
    }

    expect.assert_eq(&output);
}

#[allow(clippy::needless_pass_by_value)]
fn check_hints(source: &str, expect: Expect) {
    let project = compiled(source);
    *lock(&project.backend().shared.hints) = HintOptions::default();
    expect.assert_eq(&project.inlay_hints("main.rue"));
}

/// The item is renamed from its declaration, and from a use in the filter and in the body, and
/// so is one that's destructured.
#[test]
fn test_rename_for_item() {
    check_renames(
        "xs: List<Int>, pairs: List<(Int, Int)>",
        &[
            ("for it$0em in xs if item > 0 { item + 1 }", "value"),
            ("for item in xs if it$0em > 0 { item + 1 }", "value"),
            ("for item in xs if item > 0 { it$0em + 1 }", "value"),
            ("for (first, sec$0ond) in pairs { first + second }", "other"),
            ("for (first, second) in pairs { first + sec$0ond }", "other"),
        ],
        expect![[r#"
            == for it$0em in xs if item > 0 { item + 1 } => value
            main.rue:
            fn main(xs: List<Int>, pairs: List<(Int, Int)>) -> Any {
                for value in xs if value > 0 { value + 1 }
            }

            fn size(_list: List<Int>) -> Int {
                1
            }

            == for item in xs if it$0em > 0 { item + 1 } => value
            main.rue:
            fn main(xs: List<Int>, pairs: List<(Int, Int)>) -> Any {
                for value in xs if value > 0 { value + 1 }
            }

            fn size(_list: List<Int>) -> Int {
                1
            }

            == for item in xs if item > 0 { it$0em + 1 } => value
            main.rue:
            fn main(xs: List<Int>, pairs: List<(Int, Int)>) -> Any {
                for value in xs if value > 0 { value + 1 }
            }

            fn size(_list: List<Int>) -> Int {
                1
            }

            == for (first, sec$0ond) in pairs { first + second } => other
            main.rue:
            fn main(xs: List<Int>, pairs: List<(Int, Int)>) -> Any {
                for (first, other) in pairs { first + other }
            }

            fn size(_list: List<Int>) -> Int {
                1
            }

            == for (first, second) in pairs { first + sec$0ond } => other
            main.rue:
            fn main(xs: List<Int>, pairs: List<(Int, Int)>) -> Any {
                for (first, other) in pairs { first + other }
            }

            fn size(_list: List<Int>) -> Int {
                1
            }

        "#]],
    );
}

/// A field that's destructured keeps its name, and the binding for it is renamed.
#[test]
fn test_rename_for_item_field() {
    let source = "struct Point {\n    x: Int,\n    y: Int,\n}\n\nfn main(points: List<Point>) -> List<Int> {\n    for { x, y: height } in points { x + hei$0ght }\n}\n";
    expect![[r#"
        main.rue:
        struct Point {
            x: Int,
            y: Int,
        }

        fn main(points: List<Point>) -> List<Int> {
            for { x, y: tall } in points { x + tall }
        }
    "#]]
    .assert_eq(&compiled(source).rename("tall"));

    let shorthand = "struct Point {\n    x: Int,\n    y: Int,\n}\n\nfn main(points: List<Point>) -> List<Int> {\n    for { x, y } in points { x$0 + y }\n}\n";
    expect![[r#"
        main.rue:
        struct Point {
            x: Int,
            y: Int,
        }

        fn main(points: List<Point>) -> List<Int> {
            for { x: left, y } in points { left + y }
        }
    "#]]
    .assert_eq(&compiled(shorthand).rename("left"));
}

/// The result of a fold is renamed from its declaration and from the body.
#[test]
fn test_rename_for_fold() {
    check_renames(
        "xs: List<Int>",
        &[
            ("for x in xs fold to$0tal = 0 { total + x }", "sum"),
            ("for x in xs fold total = 0 { to$0tal + x }", "sum"),
        ],
        expect![[r#"
            == for x in xs fold to$0tal = 0 { total + x } => sum
            main.rue:
            fn main(xs: List<Int>) -> Any {
                for x in xs fold sum = 0 { sum + x }
            }

            fn size(_list: List<Int>) -> Int {
                1
            }

            == for x in xs fold total = 0 { to$0tal + x } => sum
            main.rue:
            fn main(xs: List<Int>) -> Any {
                for x in xs fold sum = 0 { sum + x }
            }

            fn size(_list: List<Int>) -> Int {
                1
            }

        "#]],
    );
}

/// A rename is refused if a reference in the scope of the item, or of the result of a fold,
/// would find another binding. The list isn't in the item's scope, and neither is the fold's
/// value, so a name that's only used there isn't in the way. The filter is in the item's scope,
/// but not in the fold's.
#[test]
fn test_rename_for_conflicts() {
    check_renames(
        "xs: List<Int>, limit: Int, start: Int",
        &[
            // The body's reference to the outer name would find the item, and the other way
            ("for it$0em in xs { item + limit }", "limit"),
            ("for item in xs { item + lim$0it }", "item"),
            // The same for the result of a fold
            (
                "for x in xs fold to$0tal = 0 { total + x + limit }",
                "limit",
            ),
            (
                "for x in xs fold total = 0 { total + x + lim$0it }",
                "total",
            ),
            // The item and the result of the fold are in the body together
            ("for it$0em in xs fold total = 0 { total + item }", "total"),
            ("for item in xs fold to$0tal = 0 { total + item }", "item"),
            // Names that are only used in the list and in the fold's value
            ("for it$0em in xs fold total = start { total + item }", "xs"),
            (
                "for it$0em in xs fold total = start { total + item }",
                "start",
            ),
            (
                "for item in xs fold to$0tal = start { total + item }",
                "start",
            ),
            // A name that's used in the filter
            (
                "for it$0em in xs if item > limit fold total = 0 { total + item }",
                "limit",
            ),
            (
                "for item in xs if item > limit fold to$0tal = 0 { total + item }",
                "limit",
            ),
            // An outer name that's used in the list can be the item's name, but not if the body
            // uses it too
            ("for item in x$0s { item }", "item"),
            ("for item in x$0s { item + size(xs) }", "item"),
        ],
        expect![[r#"
            == for it$0em in xs { item + limit } => limit
            error: Renaming `item` to `limit` would change what `limit` refers to at main.rue:2:29

            == for item in xs { item + lim$0it } => item
            error: Renaming `limit` to `item` would change what `item` refers to at main.rue:2:29

            == for x in xs fold to$0tal = 0 { total + x + limit } => limit
            error: Renaming `total` to `limit` would change what `limit` refers to at main.rue:2:46

            == for x in xs fold total = 0 { total + x + lim$0it } => total
            error: Renaming `limit` to `total` would change what `total` refers to at main.rue:2:46

            == for it$0em in xs fold total = 0 { total + item } => total
            error: Renaming `item` to `total` would change what `total` refers to at main.rue:2:45

            == for item in xs fold to$0tal = 0 { total + item } => item
            error: Renaming `total` to `item` would change what `item` refers to at main.rue:2:45

            == for it$0em in xs fold total = start { total + item } => xs
            main.rue:
            fn main(xs: List<Int>, limit: Int, start: Int) -> Any {
                for xs in xs fold total = start { total + xs }
            }

            fn size(_list: List<Int>) -> Int {
                1
            }

            == for it$0em in xs fold total = start { total + item } => start
            main.rue:
            fn main(xs: List<Int>, limit: Int, start: Int) -> Any {
                for start in xs fold total = start { total + start }
            }

            fn size(_list: List<Int>) -> Int {
                1
            }

            == for item in xs fold to$0tal = start { total + item } => start
            main.rue:
            fn main(xs: List<Int>, limit: Int, start: Int) -> Any {
                for item in xs fold start = start { start + item }
            }

            fn size(_list: List<Int>) -> Int {
                1
            }

            == for it$0em in xs if item > limit fold total = 0 { total + item } => limit
            error: Renaming `item` to `limit` would change what `limit` refers to at main.rue:2:30

            == for item in xs if item > limit fold to$0tal = 0 { total + item } => limit
            main.rue:
            fn main(xs: List<Int>, limit: Int, start: Int) -> Any {
                for item in xs if item > limit fold limit = 0 { limit + item }
            }

            fn size(_list: List<Int>) -> Int {
                1
            }

            == for item in x$0s { item } => item
            main.rue:
            fn main(item: List<Int>, limit: Int, start: Int) -> Any {
                for item in item { item }
            }

            fn size(_list: List<Int>) -> Int {
                1
            }

            == for item in x$0s { item + size(xs) } => item
            error: Renaming `xs` to `item` would change what `item` refers to at main.rue:2:34

        "#]],
    );
}

/// An outer symbol is renamed in the list, the filter, the fold's value and the body.
#[test]
fn test_rename_outer_symbol_used_in_for() {
    check_renames(
        "values: List<Int>",
        &[(
            "for x in val$0ues if x > size(values) fold total = size(values) { total + x + size(values) }",
            "numbers",
        )],
        expect![[r#"
            == for x in val$0ues if x > size(values) fold total = size(values) { total + x + size(values) } => numbers
            main.rue:
            fn main(numbers: List<Int>) -> Any {
                for x in numbers if x > size(numbers) fold total = size(numbers) { total + x + size(numbers) }
            }

            fn size(_list: List<Int>) -> Int {
                1
            }

        "#]],
    );
}

#[test]
fn test_for_navigation() {
    let mut output = String::new();

    for body in [
        "for item in xs if item > 0 { it$0em + 1 }",
        "for it$0em in xs if item > 0 { item + 1 }",
        "for x in xs fold total = 0 { to$0tal + x }",
        "for x in xs fold to$0tal = 0 { total + x }",
        "for (first, second) in pairs { first + sec$0ond }",
    ] {
        let source =
            format!("fn main(xs: List<Int>, pairs: List<(Int, Int)>) -> Any {{\n    {body}\n}}\n");
        let project = compiled(&source);
        writeln!(
            output,
            "== {body}\ndefinition:\n{}references:\n{}",
            project.definitions(),
            project.references()
        )
        .unwrap();
    }

    expect![[r#"
        == for item in xs if item > 0 { it$0em + 1 }
        definition:
        main.rue:1:8-1:12
        references:
        main.rue:1:8-1:12
        main.rue:1:22-1:26
        main.rue:1:33-1:37

        == for it$0em in xs if item > 0 { item + 1 }
        definition:
        main.rue:1:22-1:26
        main.rue:1:33-1:37
        references:
        main.rue:1:8-1:12
        main.rue:1:22-1:26
        main.rue:1:33-1:37

        == for x in xs fold total = 0 { to$0tal + x }
        definition:
        main.rue:1:21-1:26
        references:
        main.rue:1:21-1:26
        main.rue:1:33-1:38

        == for x in xs fold to$0tal = 0 { total + x }
        definition:
        main.rue:1:33-1:38
        references:
        main.rue:1:21-1:26
        main.rue:1:33-1:38

        == for (first, second) in pairs { first + sec$0ond }
        definition:
        main.rue:1:16-1:22
        references:
        main.rue:1:16-1:22
        main.rue:1:43-1:49

    "#]]
    .assert_eq(&output);
}

/// The item gets the type it's inferred to be from the list, as a lambda's parameter without a
/// type does, and so do the parts of a destructured item, other than the fields of a struct. The
/// result of a fold gets its type as a `let` binding does: not if it has one, or if its value is
/// a literal, except for nil, which doesn't say what the result is.
#[test]
fn test_for_inlay_hints() {
    check_hints(
        "fn main(xs: List<Int>, pairs: List<(Int, Bytes)>) -> Int {\n    let doubled = for x in xs { x * 2 };\n    let total = for x in doubled fold sum = 0 { sum + x };\n    let typed = for x in xs fold sum: Int = 0 { sum + x };\n    for (count, name) in pairs {\n        assert count > name.length;\n    }\n    let list: List<Int> = for x in xs fold rest = nil { [x, ...rest] };\n    let start = total;\n    let more = for x in list fold sum = start { sum + x };\n    for _ in xs {\n        assert total > 0;\n    }\n    total + typed + more\n}\n",
        expect![[r#"
            fn main(xs: List<Int>, pairs: List<(Int, Bytes)>) -> Int {
                let doubled«: List<Int>» = for x«: Int» in xs { x * 2 };
                let total«: Int» = for x«: Int» in doubled fold sum = 0 { sum + x };
                let typed«: Int» = for x«: Int» in xs fold sum: Int = 0 { sum + x };
                for (count«: Int», name«: Bytes») in pairs {
                    assert count > name.length;
                }
                let list: List<Int> = for x«: Int» in xs fold rest«: List<Int>» = nil { [x, ...rest] };
                let start«: Int» = total;
                let more«: Int» = for x«: Int» in list fold sum«: Int» = start { sum + x };
                for _ in xs {
                    assert total > 0;
                }
                total + typed + more
            }
        "#]],
    );
}

/// A generic call in the list, the filter and the body gets its hints.
#[test]
fn test_for_inlay_hints_of_calls() {
    check_hints(
        "fn same<T>(value: T) -> T {\n    value\n}\n\nfn main(xs: List<Int>) -> List<Int> {\n    for x in same(xs) if same(x) > 0 { same(x + 1) }\n}\n",
        expect![[r#"
            fn same<T>(value: T) -> T {
                value
            }

            fn main(xs: List<Int>) -> List<Int> {
                for x«: Int» in same«::<List<Int>>»(«value:» xs) if same«::<Int>»(«value:» x) > 0 { same«::<Int>»(«value:» x + 1) }
            }
        "#]],
    );
}

/// Signature help in the body, the list, the filter and the fold's value.
#[test]
fn test_for_signature_help() {
    const ADD: &str = "fn add(first: Int, second: Int) -> Int {\n    first + second\n}\n\nfn all(count: Int) -> List<Int> {\n    [count]\n}\n\n";

    let mut output = String::new();

    for body in [
        "for x in all(1) { add(x, $0) }",
        "for x in all($0) { add(x, 1) }",
        "for x in all(1) if add($0x, 1) > 0 { x }",
        "for x in all(1) fold total = add(1, $0) { total + x }",
    ] {
        let source = format!("{ADD}fn main() -> Any {{\n    {body}\n}}\n");
        let project = TestProject::new(&[("main.rue", &source)]);
        writeln!(output, "== {body}\n{}", project.signature_help()).unwrap();
    }

    expect![[r#"
        == for x in all(1) { add(x, $0) }
        > fn add(first: Int, [second: Int]) -> Int

        == for x in all($0) { add(x, 1) }
        > fn all([count: Int]) -> List<Int>

        == for x in all(1) if add($0x, 1) > 0 { x }
        > fn add([first: Int], second: Int) -> Int

        == for x in all(1) fold total = add(1, $0) { total + x }
        > fn add(first: Int, [second: Int]) -> Int

    "#]]
    .assert_eq(&output);
}

/// Unfinished `for` expressions don't panic, and what's around them is still found where it is:
/// the rename of `xs` reaches the uses that are still there, the symbols after the `for` are
/// listed, and the hints and signature help are for the code that's left.
#[test]
fn test_unfinished_for() {
    const ADD: &str = "fn add(first: Int, second: Int) -> Int {\n    first + second\n}\n\n";

    let mut output = String::new();

    for body in [
        "for x in",
        "for x in xs fold total =",
        "for x in xs { add(x, ",
    ] {
        let source = format!(
            "{ADD}fn main(xs: List<Int>) -> Any {{\n    let before = add(1, $0);\n    let list = xs;\n    {body}\n}}\n\nfn after() -> Int {{\n    1\n}}\n"
        );
        let project = TestProject::new(&[("main.rue", &source)]);
        project
            .backend()
            .nested_symbols
            .store(true, Ordering::Relaxed);
        *lock(&project.backend().shared.hints) = HintOptions::default();

        writeln!(
            output,
            "== {body}\nsymbols:\n{}hints:\n{}signature:\n{}",
            project.document_symbols("main.rue"),
            project.inlay_hints("main.rue"),
            project.signature_help()
        )
        .unwrap();

        // Renaming `xs` from its use before the `for`
        let source = source.replace("$0", "").replace("list = xs", "list = x$0s");
        let project = TestProject::new(&[("main.rue", &source)]);
        project
            .backend()
            .shared
            .rename_files
            .store(true, Ordering::Relaxed);
        writeln!(output, "rename:\n{}", project.rename("ys")).unwrap();
    }

    expect![[r#"
        == for x in
        symbols:
        Function add `(first: Int, second: Int) -> Int` 0:0-2:1 [0:3-0:6]
        Function main `(xs: List<Int>) -> Any` 4:0-8:1 [4:3-4:7]
        Function after `() -> Int` 10:0-12:1 [10:3-10:8]
        hints:
        fn add(first: Int, second: Int) -> Int {
            first + second
        }

        fn main(xs: List<Int>) -> Any {
            let before«: Int» = add(«first:» 1, );
            let list«: List<Int>» = xs;
            for x in
        }

        fn after() -> Int {
            1
        }
        signature:
        > fn add(first: Int, [second: Int]) -> Int

        rename:
        main.rue:
        fn add(first: Int, second: Int) -> Int {
            first + second
        }

        fn main(ys: List<Int>) -> Any {
            let before = add(1, );
            let list = ys;
            for x in
        }

        fn after() -> Int {
            1
        }

        == for x in xs fold total =
        symbols:
        Function add `(first: Int, second: Int) -> Int` 0:0-2:1 [0:3-0:6]
        Function main `(xs: List<Int>) -> Any` 4:0-8:1 [4:3-4:7]
        Function after `() -> Int` 10:0-12:1 [10:3-10:8]
        hints:
        fn add(first: Int, second: Int) -> Int {
            first + second
        }

        fn main(xs: List<Int>) -> Any {
            let before«: Int» = add(«first:» 1, );
            let list«: List<Int>» = xs;
            for x«: Int» in xs fold total =
        }

        fn after() -> Int {
            1
        }
        signature:
        > fn add(first: Int, [second: Int]) -> Int

        rename:
        main.rue:
        fn add(first: Int, second: Int) -> Int {
            first + second
        }

        fn main(ys: List<Int>) -> Any {
            let before = add(1, );
            let list = ys;
            for x in ys fold total =
        }

        fn after() -> Int {
            1
        }

        == for x in xs { add(x, 
        symbols:
        Function add `(first: Int, second: Int) -> Int` 0:0-2:1 [0:3-0:6]
        Function main `(xs: List<Int>) -> Any` 4:0-8:1 [4:3-4:7]
        Function after `() -> Int` 10:0-12:1 [10:3-10:8]
        hints:
        fn add(first: Int, second: Int) -> Int {
            first + second
        }

        fn main(xs: List<Int>) -> Any {
            let before«: Int» = add(«first:» 1, );
            let list«: List<Int>» = xs;
            for x«: Int» in xs { add(«first:» x, 
        }

        fn after() -> Int {
            1
        }
        signature:
        > fn add(first: Int, [second: Int]) -> Int

        rename:
        main.rue:
        fn add(first: Int, second: Int) -> Int {
            first + second
        }

        fn main(ys: List<Int>) -> Any {
            let before = add(1, );
            let list = ys;
            for x in ys { add(x, 
        }

        fn after() -> Int {
            1
        }

    "#]]
    .assert_eq(&output);
}
