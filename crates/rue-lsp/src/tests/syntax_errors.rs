//! Queries on code that doesn't parse, such as while it's being typed, give what can be worked out
//! from the parts that do, and never panic.

use expect_test::{Expect, expect};

use super::TestProject;

#[allow(clippy::needless_pass_by_value)]
fn check(source: &str, expect: Expect) {
    let project = TestProject::new(&[("main.rue", source)]);

    // Only the first few completions are shown, which is enough to tell whether locals are
    // offered, without listing every builtin
    let completions = project.completions();
    let mut lines = completions.lines();
    let mut shown = lines.by_ref().take(4).collect::<Vec<_>>().join("\n");

    if lines.next().is_some() {
        shown.push_str("\n...");
    }

    let output = format!("{}completions:\n{shown}\n", project.queries());

    expect.assert_eq(&output);
}

const POINT: &str = "struct Point {\n    x: Int,\n    y: Int,\n}\n\n";

#[test]
fn test_missing_closing_brace() {
    check(
        "fn main() -> Int {\n    let value = 42;\n    val$0ue\n",
        expect![[r#"
            hover:
            let value: 42
            definitions:
            main.rue:1:8-1:13
            references:
            main.rue:1:8-1:13
            main.rue:2:4-2:9
            completions:
            value [Variable] 42
        "#]],
    );
}

#[test]
fn test_missing_opening_brace() {
    check(
        "fn main() -> Int\n    let value = 42;\n    val$0ue\n}\n",
        expect![[r#"
            hover:
            <none>
            definitions:
            <none>
            references:
            <none>
            completions:
            <none>
        "#]],
    );
}

#[test]
fn test_unterminated_string() {
    check(
        "fn main() -> Int {\n    let value = 42;\n    let s = \"abc;\n    val$0ue\n}\n",
        expect![[r#"
            hover:
            <none>
            definitions:
            <none>
            references:
            <none>
            completions:
            <none>
        "#]],
    );
}

#[test]
fn test_cursor_inside_unterminated_string() {
    check(
        "fn main() -> Int {\n    let value = 42;\n    let s = \"va$0l\n}\n",
        expect![[r#"
            hover:
            <none>
            definitions:
            <none>
            references:
            <none>
            completions:
            <none>
        "#]],
    );
}

#[test]
fn test_field_access_without_field() {
    check(
        &format!("{POINT}fn main(point: Point) -> Int {{\n    point.$0\n}}\n"),
        expect![[r#"
            hover:
            <none>
            definitions:
            <none>
            references:
            <none>
            completions:
            x [Field] Int
            y [Field] Int
        "#]],
    );
}

#[test]
fn test_field_access_without_field_before_dot() {
    check(
        &format!("{POINT}fn main(point: Point) -> Int {{\n    poi$0nt.\n}}\n"),
        expect![[r#"
            hover:
            point: Point
            definitions:
            main.rue:5:8-5:13
            references:
            main.rue:5:8-5:13
            main.rue:6:4-6:9
            completions:
            point [Variable] Point
            Point [Struct]
        "#]],
    );
}

#[test]
fn test_field_access_without_field_or_brace() {
    check(
        &format!("{POINT}fn main(point: Point) -> Int {{\n    point.$0"),
        expect![[r#"
        hover:
        <none>
        definitions:
        <none>
        references:
        <none>
        completions:
        x [Field] Int
        y [Field] Int
    "#]],
    );
}

#[test]
fn test_path_without_name() {
    check(
        "mod math {\n    export fn double(num: Int) -> Int {\n        num * 2\n    }\n}\n\nfn main() -> Int {\n    math::$0\n}\n",
        expect![[r#"
            hover:
            <none>
            definitions:
            <none>
            references:
            <none>
            completions:
            double [Function] fn(num: Int) -> Int
        "#]],
    );
}

#[test]
fn test_path_without_name_or_brace() {
    check(
        "mod math {\n    export fn double(num: Int) -> Int {\n        num * 2\n    }\n}\n\nfn main() -> Int {\n    math::$0",
        expect![[r#"
            hover:
            <none>
            definitions:
            <none>
            references:
            <none>
            completions:
            double [Function] fn(num: Int) -> Int
        "#]],
    );
}

#[test]
fn test_let_without_value() {
    check(
        "fn main() -> Int {\n    let value = 42;\n    let other = $0\n}\n",
        expect![[r#"
            hover:
            <none>
            definitions:
            <none>
            references:
            <none>
            completions:
            super [Module]
            value [Variable] 42
            main [Function] fn() -> Int
            unchecked_cast [Function] fn<T>(value: Any) -> T
            ...
        "#]],
    );
}

#[test]
fn test_let_without_value_before_semicolon() {
    check(
        "fn main() -> Int {\n    let value = 42;\n    let other = $0;\n    other\n}\n",
        expect![[r#"
            hover:
            <none>
            definitions:
            <none>
            references:
            <none>
            completions:
            super [Module]
            value [Variable] 42
            main [Function] fn() -> Int
            unchecked_cast [Function] fn<T>(value: Any) -> T
            ...
        "#]],
    );
}

#[test]
fn test_return_without_value() {
    check(
        "fn main() -> Int {\n    let value = 42;\n    return $0\n}\n",
        expect![[r#"
            hover:
            <none>
            definitions:
            <none>
            references:
            <none>
            completions:
            super [Module]
            value [Variable] 42
            main [Function] fn() -> Int
            unchecked_cast [Function] fn<T>(value: Any) -> T
            ...
        "#]],
    );
}

#[test]
fn test_if_without_condition() {
    check(
        "fn main() -> Int {\n    let value = 42;\n    if $0\n}\n",
        expect![[r#"
            hover:
            <none>
            definitions:
            <none>
            references:
            <none>
            completions:
            super [Module]
            value [Variable] 42
            main [Function] fn() -> Int
            unchecked_cast [Function] fn<T>(value: Any) -> T
            ...
        "#]],
    );
}

#[test]
fn test_assert_without_condition() {
    check(
        "fn main() -> Int {\n    let value = 42;\n    assert $0\n}\n",
        expect![[r#"
            hover:
            <none>
            definitions:
            <none>
            references:
            <none>
            completions:
            super [Module]
            value [Variable] 42
            main [Function] fn() -> Int
            unchecked_cast [Function] fn<T>(value: Any) -> T
            ...
        "#]],
    );
}

#[test]
fn test_raise_without_value() {
    check(
        "fn main() -> Int {\n    let value = 42;\n    raise $0\n}\n",
        expect![[r#"
            hover:
            <none>
            definitions:
            <none>
            references:
            <none>
            completions:
            super [Module]
            value [Variable] 42
            main [Function] fn() -> Int
            unchecked_cast [Function] fn<T>(value: Any) -> T
            ...
        "#]],
    );
}

#[test]
fn test_constant_without_value() {
    check(
        "const LIMIT: Int = $0
",
        expect![[r#"
    hover:
    <none>
    definitions:
    <none>
    references:
    <none>
    completions:
    super [Module]
    LIMIT [Constant] Int
    unchecked_cast [Function] fn<T>(value: Any) -> T
    sha256 [Function] fn(value: Bytes) -> Bytes32
    ...
"#]],
    );
}

#[test]
fn test_cursor_inside_string() {
    check(
        "fn main() -> Int {\n    let value = 42;\n    let s = \"va$0l\";\n    value\n}\n",
        expect![[r#"
            hover:
            <none>
            definitions:
            <none>
            references:
            <none>
            completions:
            <none>
        "#]],
    );
}

#[test]
fn test_cursor_after_string() {
    // Right after the closing quote is outside of the string.
    check(
        "fn main() -> Int {\n    let value = 42;\n    let s = \"val\"$0;\n    value\n}\n",
        expect![[r#"
            hover:
            <none>
            definitions:
            <none>
            references:
            <none>
            completions:
            super [Module]
            value [Variable] 42
            main [Function] fn() -> Int
            unchecked_cast [Function] fn<T>(value: Any) -> T
            ...
        "#]],
    );
}

#[test]
fn test_cursor_inside_line_comment() {
    check(
        "fn main() -> Int {\n    let value = 42;\n    // val$0\n    value\n}\n",
        expect![[r#"
            hover:
            <none>
            definitions:
            <none>
            references:
            <none>
            completions:
            <none>
        "#]],
    );
}

#[test]
fn test_cursor_inside_block_comment() {
    check(
        "fn main() -> Int {\n    let value = 42;\n    /* val$0 */\n    value\n}\n",
        expect![[r#"
            hover:
            <none>
            definitions:
            <none>
            references:
            <none>
            completions:
            <none>
        "#]],
    );
}

#[test]
fn test_cursor_after_block_comment() {
    check(
        "fn main() -> Int {\n    let value = 42;\n    /* comment */val$0\n}\n",
        expect![[r#"
            hover:
            <none>
            definitions:
            <none>
            references:
            <none>
            completions:
            value [Variable] 42
        "#]],
    );
}

#[test]
fn test_cursor_inside_unterminated_nested_block_comment() {
    check(
        "fn main() -> Int {\n    let value = 42;\n    /* /* */ val$0",
        expect![[r#"
            hover:
            <none>
            definitions:
            <none>
            references:
            <none>
            completions:
            <none>
        "#]],
    );
}

#[test]
fn test_unfinished_parameters() {
    check(
        "fn main(value: Int$0\n",
        expect![[r#"
        hover:
        type Int
        definitions:
        <none>
        references:
        main.rue:0:15-0:18
        completions:
        Int [Struct]
        Signature [Struct]
        K1Signature [Struct]
        R1Signature [Struct]
        ...
    "#]],
    );
}

#[test]
fn test_unfinished_struct() {
    check(
        "struct Point {\n    x: Int,\n    y: $0\n\nfn main() -> Int {\n    42\n}\n",
        expect![[r#"
            hover:
            <none>
            definitions:
            <none>
            references:
            <none>
            completions:
            super [Module]
            Point [Struct]
            Atom [Struct]
            Bytes [Struct]
            ...
        "#]],
    );
}

#[test]
fn test_unfinished_binary_expression() {
    check(
        "fn main(value: Int) -> Int {\n    value + $0\n}\n",
        expect![[r#"
            hover:
            <none>
            definitions:
            <none>
            references:
            <none>
            completions:
            super [Module]
            value [Variable] Int
            main [Function] fn(value: Int) -> Int
            unchecked_cast [Function] fn<T>(value: Any) -> T
            ...
        "#]],
    );
}

#[test]
fn test_unfinished_call() {
    check(
        "fn main(value: Int) -> Int {\n    double(val$0\n}\n\nfn double(num: Int) -> Int {\n    num * 2\n}\n",
        expect![[r#"
            hover:
            <none>
            definitions:
            <none>
            references:
            <none>
            completions:
            value [Variable] Int
        "#]],
    );
}

#[test]
fn test_empty_file() {
    check(
        "$0",
        expect![[r#"
        hover:
        <none>
        definitions:
        <none>
        references:
        <none>
        completions:
        <none>
    "#]],
    );
}

#[test]
fn test_only_garbage() {
    check(
        "}}} ;; $0 ))) \"",
        expect![[r#"
        hover:
        <none>
        definitions:
        <none>
        references:
        <none>
        completions:
        <none>
    "#]],
    );
}
