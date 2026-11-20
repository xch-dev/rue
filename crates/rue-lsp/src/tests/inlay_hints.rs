//! Inlay hints show the types, generic arguments and parameter names that the code leaves out.

use expect_test::{Expect, expect};
use tower_lsp_server::ls_types::{Position, Range};

use super::TestProject;
use crate::cache::HintOptions;
use crate::lock;

/// Shows the hints of code that compiles, so that a test is about the types the code has, rather
/// than what's left of code that was rejected.
#[allow(clippy::needless_pass_by_value)]
fn check(source: &str, expect: Expect) {
    check_with(HintOptions::default(), source, expect);
}

/// Shows the hints of code that compiles, with some of them turned off.
#[allow(clippy::needless_pass_by_value)]
fn check_with(options: HintOptions, source: &str, expect: Expect) {
    let project = TestProject::new(&[("main.rue", source)]);
    let diagnostics = project.diagnostics();
    assert!(!diagnostics.contains(" Error"), "{diagnostics}");
    *lock(&project.backend().shared.hints) = options;
    expect.assert_eq(&project.inlay_hints("main.rue"));
}

/// Shows the hints of code that has errors.
#[allow(clippy::needless_pass_by_value)]
fn check_with_errors(options: HintOptions, source: &str, expect: Expect) {
    let project = TestProject::new(&[("main.rue", source)]);
    let diagnostics = project.diagnostics();
    assert!(diagnostics.contains(" Error"), "{diagnostics}");
    *lock(&project.backend().shared.hints) = options;
    expect.assert_eq(&project.inlay_hints("main.rue"));
}

const TYPES: HintOptions = HintOptions {
    types: true,
    generic_arguments: false,
    parameter_names: false,
};

const GENERIC_ARGUMENTS: HintOptions = HintOptions {
    types: false,
    generic_arguments: true,
    parameter_names: false,
};

const PARAMETER_NAMES: HintOptions = HintOptions {
    types: false,
    generic_arguments: false,
    parameter_names: true,
};

#[test]
fn test_hints_for_let_bindings() {
    check_with(
        TYPES,
        r#"
struct Point {
    x: Int,
    y: Int,
}

fn main(point: Point, list: List<Int>) -> Int {
    let number = 1;
    let text = "hello";
    let copy = point;
    let items = list;
    let sum = number + point.x;
    let written: Int = 2;
    let typed: Int = number + point.x;
    let grouped = (5);
    let lambda = (fn(value: Int) => value);
    let _ = sum;
    let _unused = sum;
    let made = Point { x: 1, y: 2 };
    sum + written + text.length + copy.y + made.x + typed + grouped + lambda(1)
}
"#,
        expect![[r#"

            struct Point {
                x: Int,
                y: Int,
            }

            fn main(point: Point, list: List<Int>) -> Int {
                let number = 1;
                let text = "hello";
                let copy«: Point» = point;
                let items«: List<Int>» = list;
                let sum«: Int» = number + point.x;
                let written: Int = 2;
                let typed: Int = number + point.x;
                let grouped = (5);
                let lambda = (fn(value: Int) => value);
                let _ = sum;
                let _unused«: Int» = sum;
                let made = Point { x: 1, y: 2 };
                sum + written + text.length + copy.y + made.x + typed + grouped + lambda(1)
            }
        "#]],
    );
}

#[test]
fn test_hints_for_destructuring() {
    // Each name gets its type, other than the fields of a struct binding, where a type after
    // the field's name would read as the binding for it
    check_with(
        TYPES,
        r#"
struct Point {
    x: Int,
    y: Bytes,
}

fn main(pair: (Int, Bytes), list: [Int, ...List<Bytes>], point: Point) -> Int {
    let (first, second) = pair;
    let [head, ...tail] = list;
    let { x, y: other } = point;
    let (_, (kept, [inner])) = (pair, (pair.rest, [first]));
    let _rest = tail;
    first + head + x + second.length + other.length + kept.length + inner
}
"#,
        expect![[r#"

            struct Point {
                x: Int,
                y: Bytes,
            }

            fn main(pair: (Int, Bytes), list: [Int, ...List<Bytes>], point: Point) -> Int {
                let (first«: Int», second«: Bytes») = pair;
                let [head«: Int», ...tail«: List<Bytes>»] = list;
                let { x, y: other } = point;
                let (_, (kept«: Bytes», [inner«: Int»])) = (pair, (pair.rest, [first]));
                let _rest«: List<Bytes>» = tail;
                first + head + x + second.length + other.length + kept.length + inner
            }
        "#]],
    );
}

#[test]
fn test_hints_for_lambda_parameters() {
    check_with(
        TYPES,
        r#"
fn apply(value: Int, function: fn(item: Int) -> Int) -> Int {
    function(value)
}

fn main() -> Int {
    let add = fn(first: Int, second: Int) => first + second;
    let double: fn(value: Int) -> Int = fn(value) => value * 2;
    apply(1, fn(item) => item + add(1, 2)) + double(2)
}
"#,
        expect![[r#"

            fn apply(value: Int, function: fn(item: Int) -> Int) -> Int {
                function(value)
            }

            fn main() -> Int {
                let add = fn(first: Int, second: Int) => first + second;
                let double: fn(value: Int) -> Int = fn(value«: Int») => value * 2;
                apply(1, fn(item«: Int») => item + add(1, 2)) + double(2)
            }
        "#]],
    );
}

#[test]
fn test_hints_for_generic_arguments() {
    check_with(
        GENERIC_ARGUMENTS,
        r#"
fn pick<T, U>(first: T, second: U) -> T {
    first
}

fn same<T>(value: T) -> T {
    same(value)
}

fn one() -> Int {
    1
}

mod inner {
    export fn id<T>(value: T) -> T {
        value
    }
}

struct Circle { kind = 1, radius: Int }
struct Square { kind = 2, side: Int }
struct Triangle { kind = 3, base: Int, height: Int }

fn main(shape: Circle | Square | Triangle | nil) -> Int {
    let picked = pick(1, "a");
    let written = pick::<Int, String>(1, "a");
    let nested = inner::id(pick(true, nil));
    let union = inner::id(shape);
    one() + picked + written + same(2)
}

"#,
        expect![[r#"

            fn pick<T, U>(first: T, second: U) -> T {
                first
            }

            fn same<T>(value: T) -> T {
                same«::<T>»(value)
            }

            fn one() -> Int {
                1
            }

            mod inner {
                export fn id<T>(value: T) -> T {
                    value
                }
            }

            struct Circle { kind = 1, radius: Int }
            struct Square { kind = 2, side: Int }
            struct Triangle { kind = 3, base: Int, height: Int }

            fn main(shape: Circle | Square | Triangle | nil) -> Int {
                let picked = pick«::<Int, String>»(1, "a");
                let written = pick::<Int, String>(1, "a");
                let nested = inner::id«::<true | false>»(pick«::<Bool, nil>»(true, nil));
                let union = inner::id«::<Circle | Square | Triangle | nil>»(shape);
                one() + picked + written + same«::<Int>»(2)
            }

        "#]],
    );
}

#[test]
fn test_hints_for_parameter_names() {
    // An argument that's named like the parameter already says what it's for, and a parameter
    // that's destructured has no name
    check_with(
        PARAMETER_NAMES,
        r#"
struct Point {
    first: Int,
    second: Int,
}

fn add(first: Int, second: Int) -> Int {
    first + second
}

fn total(first: Int, ...rest: List<Int>) -> Int {
    first
}

mod inner {
    export const first: Int = 1;
    export const other: Int = 2;
}

fn pair((left, right): (Int, Int), [head]: [Int]) -> Int {
    left + right + head
}

fn main(first: Int, point: Point, list: List<Int>) -> Int {
    let _pair = pair((1, 2), [3]);
    let add_lambda = fn(left: Int, right: Int) => left + right;
    add(1, 2)
        + add(first, point.second)
        + add(point.first, first)
        + add(add(3, 4), 5)
        + total(6, ...list)
        + add(inner::first, inner::other)
        + add_lambda(7, 8)
        + sha256("a").length
}
"#,
        expect![[r#"

            struct Point {
                first: Int,
                second: Int,
            }

            fn add(first: Int, second: Int) -> Int {
                first + second
            }

            fn total(first: Int, ...rest: List<Int>) -> Int {
                first
            }

            mod inner {
                export const first: Int = 1;
                export const other: Int = 2;
            }

            fn pair((left, right): (Int, Int), [head]: [Int]) -> Int {
                left + right + head
            }

            fn main(first: Int, point: Point, list: List<Int>) -> Int {
                let _pair = pair((1, 2), [3]);
                let add_lambda = fn(left: Int, right: Int) => left + right;
                add(«first:» 1, «second:» 2)
                    + add(first, point.second)
                    + add(point.first, «second:» first)
                    + add(«first:» add(«first:» 3, «second:» 4), «second:» 5)
                    + total(«first:» 6, «rest:» ...list)
                    + add(inner::first, «second:» inner::other)
                    + add_lambda(«left:» 7, «right:» 8)
                    + sha256("a").length
            }
        "#]],
    );
}

#[test]
fn test_hints_together() {
    check(
        r#"
fn pick<T, U>(first: T, second: U) -> T {
    first
}

fn main() -> Int {
    let picked = pick(1, "a");
    let function: fn(value: Int) -> Int = fn(value) => value + picked;
    function(2)
}
"#,
        expect![[r#"

            fn pick<T, U>(first: T, second: U) -> T {
                first
            }

            fn main() -> Int {
                let picked«: Int» = pick«::<Int, String>»(«first:» 1, «second:» "a");
                let function: fn(value: Int) -> Int = fn(value«: Int») => value + picked;
                function(«value:» 2)
            }
        "#]],
    );
}

#[test]
fn test_hints_for_large_types_are_short() {
    // A union is cut after the last member that fits, and the tooltip has more of it
    check_with(
        TYPES,
        r#"
struct Circle { kind = 1, radius: Int }
struct Square { kind = 2, side: Int }
struct Triangle { kind = 3, base: Int, height: Int }
struct Rectangle { kind = 4, width: Int, height: Int }

type Shape = Circle | Square | Triangle | Rectangle;

fn main(shape: Shape, value: Circle | Square | Triangle | Rectangle | nil | Bytes32) -> Int {
    let same = shape;
    let other = value;
    let pair = (value, (value, value));
    let many = [value, value];
    1
}
"#,
        expect![[r#"

            struct Circle { kind = 1, radius: Int }
            struct Square { kind = 2, side: Int }
            struct Triangle { kind = 3, base: Int, height: Int }
            struct Rectangle { kind = 4, width: Int, height: Int }

            type Shape = Circle | Square | Triangle | Rectangle;

            fn main(shape: Shape, value: Circle | Square | Triangle | Rectangle | nil | Bytes32) -> Int {
                let same«: Shape» = shape;
                let other«: Circle | Square | Triangle | ...» = value;
                let pair«: (Circle | Square | Triangle |...» = (value, (value, value));
                let many«: (Circle | Square | Triangle |...» = [value, value];
                1
            }
            tooltip of «: Circle | Square | Triangle | ...»: Circle | Square | Triangle | Rectangle | nil | Bytes32
            tooltip of «: (Circle | Square | Triangle |...»: (Circle | Square | Triangle | Rectangle | nil | Bytes32, (Circle | Square | Triangle | Rectangle | nil | Bytes32, Circle | Square | Triangle | Rectangle | nil | Bytes32))
            tooltip of «: (Circle | Square | Triangle |...»: (Circle | Square | Triangle | Rectangle | nil | Bytes32, (Circle | Square | Triangle | Rectangle | nil | Bytes32, nil))
        "#]],
    );
}

#[test]
fn test_hints_for_huge_types() {
    // Each call doubles the type, which is over a million times as long written out as it is in
    // memory
    let nested = format!("{}1{}", "pair(".repeat(20), ")".repeat(20));

    check_with(
        TYPES,
        &format!(
            "fn pair<T>(value: T) -> (T, T) {{\n    (value, value)\n}}\n\nfn main() -> Int {{\n    let big = {nested};\n    1\n}}\n"
        ),
        expect![[r#"
            fn pair<T>(value: T) -> (T, T) {
                (value, value)
            }

            fn main() -> Int {
                let big«: ((((((((((((((((((((Int, Int)...» = pair(pair(pair(pair(pair(pair(pair(pair(pair(pair(pair(pair(pair(pair(pair(pair(pair(pair(pair(pair(1))))))))))))))))))));
                1
            }
            tooltip of «: ((((((((((((((((((((Int, Int)...»: ((((((((((((((((((((Int, Int), (Int, Int)), ((Int, Int), (Int, Int))), (((Int, Int), (Int, Int)), ((Int, Int), (Int, Int)))), ((((Int, Int), (Int, Int)), ((Int, Int), (Int, Int))), (..., ...))), ...), ...), ...), ...), ...), ...), ...), ...), ...), ...), ...), ...), ...), ...), ...)
        "#]],
    );
}

#[test]
fn test_hints_in_range() {
    // Only the hints in the range that's asked for are given, which is what's on screen
    let project = TestProject::new(&[(
        "main.rue",
        "fn main(value: Int) -> Int {\n    let first = value;\n    let second = value;\n    let third = value;\n    first + second + third\n}\n",
    )]);

    expect![[r#"
        fn main(value: Int) -> Int {
            let first = value;
            let second«: Int» = value;
            let third = value;
            first + second + third
        }
    "#]]
    .assert_eq(&project.inlay_hints_in(
        "main.rue",
        Range::new(Position::new(2, 0), Position::new(3, 0)),
    ));

    // A hint at either end of the range is in it
    expect![[r#"
        fn main(value: Int) -> Int {
            let first«: Int» = value;
            let second = value;
            let third = value;
            first + second + third
        }
    "#]]
    .assert_eq(&project.inlay_hints_in(
        "main.rue",
        Range::new(Position::new(1, 13), Position::new(2, 4)),
    ));

    expect![[r#"
        fn main(value: Int) -> Int {
            let first«: Int» = value;
            let second = value;
            let third = value;
            first + second + third
        }
    "#]]
    .assert_eq(&project.inlay_hints_in(
        "main.rue",
        Range::new(Position::new(1, 0), Position::new(1, 13)),
    ));

    expect![[r#"
        fn main(value: Int) -> Int {
            let first = value;
            let second = value;
            let third = value;
            first + second + third
        }
    "#]]
    .assert_eq(&project.inlay_hints_in(
        "main.rue",
        Range::new(Position::new(0, 0), Position::new(0, 5)),
    ));
}

#[test]
fn test_hints_across_files() {
    let project = TestProject::new(&[
        (
            "main.rue",
            "import utils::{Point, make};\n\nfn main() -> Int {\n    let point = make(1, 2);\n    let moved = utils::shift(point, 3);\n    moved.x\n}\n",
        ),
        (
            "utils.rue",
            "export struct Point {\n    x: Int,\n    y: Int,\n}\n\nexport fn make(x: Int, y: Int) -> Point {\n    Point { x, y }\n}\n\nexport fn shift(point: Point, by: Int) -> Point {\n    let x = point.x + by;\n    Point { x, y: point.y }\n}\n",
        ),
    ]);

    expect![[r#"
        import utils::{Point, make};

        fn main() -> Int {
            let point«: Point» = make(«x:» 1, «y:» 2);
            let moved«: Point» = utils::shift(point, «by:» 3);
            moved.x
        }
    "#]]
    .assert_eq(&project.inlay_hints("main.rue"));
    expect![[r#"
        export struct Point {
            x: Int,
            y: Int,
        }

        export fn make(x: Int, y: Int) -> Point {
            Point { x, y }
        }

        export fn shift(point: Point, by: Int) -> Point {
            let x«: Int» = point.x + by;
            Point { x, y: point.y }
        }
    "#]]
    .assert_eq(&project.inlay_hints("utils.rue"));
}

#[test]
fn test_hints_with_syntax_errors() {
    // The parts that still parse have hints, and what couldn't be worked out has none
    check_with_errors(
        HintOptions::default(),
        "fn add(first: Int, second: Int) -> Int {\n    first + second\n}\n\nfn main() -> Int {\n    let value = add(1, ;\n    let other = ;\n    let unknown = missing(1);\n    let last = add(2,\n",
        expect![[r#"
            fn add(first: Int, second: Int) -> Int {
                first + second
            }

            fn main() -> Int {
                let value«: Int» = add(«first:» 1, ;
                let other = ;
                let unknown = missing(1);
                let last«: Int» = add(«first:» 2,
        "#]],
    );

    check_with_errors(
        HintOptions::default(),
        "fn main() -> Int {\n    let = 1;\n    let (a, = (1, 2);\n",
        expect![[r#"
            fn main() -> Int {
                let = 1;
                let (a, = (1, 2);
        "#]],
    );
}

#[test]
fn test_hints_after_non_ascii() {
    // Positions are in UTF-16 code units
    check(
        "fn main() -> Int {\n    let text = \"\u{1F600}\u{e9}\"; let length = text.length;\n    length\n}\n",
        expect![[r#"
            fn main() -> Int {
                let text = "😀é"; let length«: Int» = text.length;
                length
            }
        "#]],
    );
}

#[test]
fn test_hints_with_narrowed_binding() {
    // The type is the one the binding was declared with, wherever it's narrowed later
    check_with(
        TYPES,
        "fn main(value: Int | nil) -> Int {\n    let copy = value;\n    if copy is nil {\n        return 0;\n    }\n    copy\n}\n",
        expect![[r#"
            fn main(value: Int | nil) -> Int {
                let copy«: Int | nil» = value;
                if copy is nil {
                    return 0;
                }
                copy
            }
        "#]],
    );
}

#[test]
fn test_hints_in_assert_message() {
    // The message of an `assert` has hints like any other code
    check(
        "fn add(first: Int, second: Int) -> Int {\n    first + second\n}\n\nfn main(value: Int) -> Int {\n    assert add(value, 1) > 1, add(2, value);\n    value\n}\n",
        expect![[r#"
            fn add(first: Int, second: Int) -> Int {
                first + second
            }

            fn main(value: Int) -> Int {
                assert add(«first:» value, «second:» 1) > 1, add(«first:» 2, «second:» value);
                value
            }
        "#]],
    );
}

#[test]
fn test_hints_with_errors() {
    // What couldn't be worked out has no hint: a call that's missing a type for a generic, the
    // parameters of the lambdas in it, and a value with a part that's unknown
    check_with_errors(
        HintOptions::default(),
        r#"
fn compose<A, B, C>(first: fn(a: A) -> B, second: fn(b: B) -> C) -> fn(a: A) -> C {
    fn(a) => second(first(a))
}

fn add(first: Int, second: Int) -> Int {
    first + second
}

fn total(first: Int, ...rest: List<Int>) -> Int {
    first
}

fn make<T>() -> List<T> {
    []
}

fn main(list: List<Int>) -> Int {
    let made = make();
    let composed = compose(fn(a) => a, fn(b) => b);
    let pair = (missing(1), 1);
    let unknown = fn(value) => value;
    add(...list) + total(1, list) + total(1, 2, 3) + add(1, 2, 3)
}
"#,
        expect![[r#"

            fn compose<A, B, C>(first: fn(a: A) -> B, second: fn(b: B) -> C) -> fn(a: A) -> C {
                fn(a«: A») => second(«b:» first(a))
            }

            fn add(first: Int, second: Int) -> Int {
                first + second
            }

            fn total(first: Int, ...rest: List<Int>) -> Int {
                first
            }

            fn make<T>() -> List<T> {
                []
            }

            fn main(list: List<Int>) -> Int {
                let made = make();
                let composed = compose(«first:» fn(a) => a, «second:» fn(b) => b);
                let pair = (missing(1), 1);
                let unknown = fn(value) => value;
                add(...list) + total(«first:» 1, list) + total(«first:» 1, 2, 3) + add(«first:» 1, «second:» 2, 3)
            }
        "#]],
    );
}

#[test]
fn test_hints_for_very_large_union() {
    // A union with more members than fit still shows the first of them
    let members = (1..=40)
        .map(|i| i.to_string())
        .collect::<Vec<_>>()
        .join(" | ");

    check_with(
        TYPES,
        &format!(
            "fn main(value: {members}) -> Int {{\n    let copy = value;\n    let list = [value];\n    1\n}}\n"
        ),
        expect![[r#"
            fn main(value: 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39 | 40) -> Int {
                let copy«: 1 | 2 | 3 | 4 | 5 | 6 | 7 | ...» = value;
                let list«: (1 | 2 | 3 | 4 | 5 | 6 | 7 |...» = [value];
                1
            }
            tooltip of «: 1 | 2 | 3 | 4 | 5 | 6 | 7 | ...»: 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39 | 40
            tooltip of «: (1 | 2 | 3 | 4 | 5 | 6 | 7 |...»: (1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39 | 40, nil)
        "#]],
    );
}

#[test]
fn test_hints_with_string_types() {
    // A `|` or a bracket inside of a string isn't part of the type around it
    check_with(
        TYPES,
        "fn main(value: \"a | b\" | \"((<\" | \"a \\\" | quoted\" | \"another long string\") -> Int {\n    let copy = value;\n    1\n}\n",
        expect![[r#"
            fn main(value: "a | b" | "((<" | "a \" | quoted" | "another long string") -> Int {
                let copy«: "a | b" | "((<" | ...» = value;
                1
            }
            tooltip of «: "a | b" | "((<" | ...»: "a | b" | "((<" | "a \" | quoted" | "another long string"
        "#]],
    );
}

#[test]
fn test_hints_in_lambda_passed_to_generic_function() {
    // The body of the lambda is compiled again once the type of its parameter is known, and the
    // hints are for the types it has in the end
    check(
        r#"
fn map<T, U>(value: T, function: fn(item: T) -> U) -> U {
    function(value)
}

fn same<T>(value: T) -> T {
    value
}

fn main() -> Bytes {
    map(1, fn(item) => {
        let copy = item;
        let text = same("a");
        text + same("b")
    })
}
"#,
        expect![[r#"

            fn map<T, U>(value: T, function: fn(item: T) -> U) -> U {
                function(«item:» value)
            }

            fn same<T>(value: T) -> T {
                value
            }

            fn main() -> Bytes {
                map«::<Int, Bytes>»(«value:» 1, «function:» fn(item«: Int») => {
                    let copy«: Int» = item;
                    let text«: String» = same«::<String>»(«value:» "a");
                    text + same«::<String>»(«value:» "b")
                })
            }
        "#]],
    );
}

#[test]
fn test_hints_of_the_file_they_are_for() {
    // Each file has a binding and a call in the same place
    let file = |ty: &str, name: &str| {
        format!(
            "fn make({name}: {ty}) -> {ty} {{\n    {name}\n}}\n\nfn main(value: {ty}) -> {ty} {{\n    let copy = make(value);\n    copy\n}}\n"
        )
    };

    let project = TestProject::new(&[
        ("main.rue", &file("Int", "aaa")),
        ("other.rue", &file("Any", "bbb")),
        ("zzz.rue", &file("nil", "ccc")),
    ]);

    expect![[r#"
        fn make(aaa: Int) -> Int {
            aaa
        }

        fn main(value: Int) -> Int {
            let copy«: Int» = make(«aaa:» value);
            copy
        }
    "#]]
    .assert_eq(&project.inlay_hints("main.rue"));
    expect![[r#"
        fn make(bbb: Any) -> Any {
            bbb
        }

        fn main(value: Any) -> Any {
            let copy«: Any» = make(«bbb:» value);
            copy
        }
    "#]]
    .assert_eq(&project.inlay_hints("other.rue"));
}
