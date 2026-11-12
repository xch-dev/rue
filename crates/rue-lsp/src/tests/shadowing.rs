//! An inner binding that shadows an outer one with the same name must win in every query.

use expect_test::{Expect, expect};

use super::TestProject;

#[allow(clippy::needless_pass_by_value)]
fn check_queries(source: &str, expect: Expect) {
    let project = TestProject::new(&[("main.rue", source)]);
    expect.assert_eq(&project.queries());
}

#[allow(clippy::needless_pass_by_value)]
fn check_completions(source: &str, expect: Expect) {
    let project = TestProject::new(&[("main.rue", source)]);
    expect.assert_eq(&project.completions());
}

#[test]
fn test_shadowing_nested_block() {
    check_queries(
        r#"
fn main() -> Int {
    let value = 42;
    let inner = {
        let value = "hello";
        val$0ue
    };
    value + inner.length
}
"#,
        expect![[r#"
            hover:
            let value: "hello"
            definitions:
            main.rue:4:12-4:17
            references:
            main.rue:4:12-4:17
            main.rue:5:8-5:13
        "#]],
    );
}

#[test]
fn test_shadowing_nested_block_completion() {
    check_completions(
        r#"
fn main() -> Int {
    let value = 42;
    let inner = {
        let value = "hello";
        val$0
    };
    value + inner.length
}
"#,
        expect![[r#"
            value [Variable] "hello"
        "#]],
    );
}

#[test]
fn test_shadowing_outer_after_block() {
    check_queries(
        r#"
fn main() -> Int {
    let value = 42;
    let inner = {
        let value = "hello";
        value.length
    };
    val$0ue + inner
}
"#,
        expect![[r#"
            hover:
            let value: 42
            definitions:
            main.rue:2:8-2:13
            references:
            main.rue:2:8-2:13
            main.rue:7:4-7:9
        "#]],
    );
}

#[test]
fn test_shadowing_parameter() {
    check_queries(
        r#"
fn main(value: Int) -> Bool {
    let value = value > 1;
    val$0ue
}
"#,
        expect![[r#"
            hover:
            let value: Bool
            definitions:
            main.rue:2:8-2:13
            references:
            main.rue:2:8-2:13
            main.rue:3:4-3:9
        "#]],
    );
}

#[test]
fn test_shadowing_parameter_in_initializer() {
    // The initializer of a `let` still refers to the parameter it shadows.
    check_queries(
        r#"
fn main(value: Int) -> Bool {
    let value = val$0ue > 1;
    value
}
"#,
        expect![[r#"
            hover:
            value: Int
            definitions:
            main.rue:1:8-1:13
            references:
            main.rue:1:8-1:13
            main.rue:2:16-2:21
        "#]],
    );
}

#[test]
fn test_shadowing_parameter_completion() {
    check_completions(
        r#"
fn main(value: Int) -> Bool {
    let value = value > 1;
    val$0
}
"#,
        expect![[r#"
            value [Variable] Bool
        "#]],
    );
}

#[test]
fn test_shadowing_parameter_completion_in_initializer() {
    // The binding isn't in scope in its own value, so the parameter is offered.
    check_completions(
        r#"
fn main(value: Int) -> Bool {
    let value = val$0 > 1;
    value
}
"#,
        expect![[r#"
            value [Variable] Int
        "#]],
    );
}

#[test]
fn test_shadowing_twice_in_one_block() {
    check_queries(
        r#"
fn main() -> Bool {
    let value = 1;
    let value = value + 1;
    let value = value > 1;
    val$0ue
}
"#,
        expect![[r#"
            hover:
            let value: Bool
            definitions:
            main.rue:4:8-4:13
            references:
            main.rue:4:8-4:13
            main.rue:5:4-5:9
        "#]],
    );
}

#[test]
fn test_shadowing_if_then_branch() {
    check_queries(
        r#"
fn main(value: Int) -> Int {
    if value > 1 {
        let value = "then";
        return val$0ue.length;
    }
    value
}
"#,
        expect![[r#"
            hover:
            let value: "then"
            definitions:
            main.rue:3:12-3:17
            references:
            main.rue:3:12-3:17
            main.rue:4:15-4:20
        "#]],
    );
}

#[test]
fn test_shadowing_if_else_branch() {
    check_queries(
        r#"
fn main(value: Int) -> Int {
    if value > 1 {
        let value = "then";
        value.length
    } else {
        let value = true;
        if val$0ue { 1 } else { 2 }
    }
}
"#,
        expect![[r#"
            hover:
            let value: true
            definitions:
            main.rue:6:12-6:17
            references:
            main.rue:6:12-6:17
            main.rue:7:11-7:16
        "#]],
    );
}

#[test]
fn test_shadowing_if_else_branch_completion() {
    check_completions(
        r#"
fn main(value: Int) -> Int {
    if value > 1 {
        let value = "then";
        value.length
    } else {
        let value = true;
        if val$0 { 1 } else { 2 }
    }
}
"#,
        expect![[r#"
            value [Variable] true
        "#]],
    );
}

#[test]
fn test_shadowing_type_alias_in_module() {
    // A type declared in an inner module shadows the outer one, so hover names it by the inner
    // declaration.
    check_queries(
        r#"
type Amount = Int;

mod inner {
    type Amount = Bytes;

    export fn get(amount: Amo$0unt) -> Amount {
        amount
    }
}

fn main() -> Amount {
    inner::get("x") as Int
}
"#,
        expect![[r#"
            hover:
            type Amount = Bytes
            definitions:
            main.rue:4:9-4:15
            references:
            main.rue:4:9-4:15
            main.rue:6:26-6:32
            main.rue:6:37-6:43
        "#]],
    );
}

#[test]
fn test_shadowing_lambda_parameter() {
    check_queries(
        r#"
fn main(value: Int) -> Int {
    let f = fn(value: Bool) => if val$0ue { 1 } else { 2 };
    f(value > 1)
}
"#,
        expect![[r#"
            hover:
            value: Bool
            definitions:
            main.rue:2:15-2:20
            references:
            main.rue:2:15-2:20
            main.rue:2:34-2:39
        "#]],
    );
}
