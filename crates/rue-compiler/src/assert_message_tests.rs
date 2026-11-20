//! The message of an `assert`, as in `assert a > b, "a must be larger";`. A debug build raises it
//! along with the location of the statement. A release build never lowers it, so the program is
//! the same with and without it, and the message can't change whether the program fails.

use std::path::PathBuf;

use clvmr::{Allocator, ChiaDialect, error::EvalErr, run_program};
use rue_clvm::{assemble, disassemble_program, disassemble_value};
use rue_diagnostic::SourceKind;
use rue_lir::ClvmTarget;
use rue_options::CompilerOptions;

use crate::test_utils::{Compiled, compile, compile_with};

fn compile_build(source: &str, debug: bool) -> Compiled {
    let compiled = compile_with(source, |options| {
        *options = CompilerOptions {
            std: false,
            ..if debug {
                CompilerOptions::debug()
            } else {
                CompilerOptions::default()
            }
        };
    });
    compiled.assert_no_errors();
    compiled
}

/// The program that `main` compiles to.
fn program(source: &str, debug: bool) -> String {
    let mut compiled = compile_build(source, debug);
    let mut allocator = Allocator::new();
    let kind = SourceKind::File("main.rue".to_string());
    let ptr = compiled
        .tree
        .main(&mut compiled.ctx, &mut allocator, &kind, PathBuf::from("."))
        .unwrap()
        .unwrap();
    disassemble_program(&allocator, ptr)
}

/// What `main` returns for the solution, or what it raises.
fn run(source: &str, debug: bool, solution: &str) -> Result<String, String> {
    let mut compiled = compile_build(source, debug);
    let mut allocator = Allocator::new();
    let kind = SourceKind::File("main.rue".to_string());
    let ptr = compiled
        .tree
        .main(&mut compiled.ctx, &mut allocator, &kind, PathBuf::from("."))
        .unwrap()
        .unwrap();
    let solution = assemble(&mut allocator, solution).unwrap();
    let dialect = ChiaDialect::new(ClvmTarget::default().flags());

    match run_program(&mut allocator, &dialect, ptr, solution, u64::MAX) {
        Ok(output) => Ok(disassemble_value(&allocator, output.1)),
        Err(EvalErr::Raise(value)) => Err(disassemble_value(&allocator, value)),
        Err(error) => Err(error.to_string()),
    }
}

/// A function that asserts `a > b` with each message (or without one, for an empty message).
fn source(prelude: &str, message: &str) -> String {
    let message = if message.is_empty() {
        String::new()
    } else {
        format!(", {message}")
    };

    format!(
        "fn main(a: Int, b: Int) -> Int {{\n    {prelude}\n    assert a > b{message};\n    a - b\n}}\n\n\
         fn double(value: Int) -> Int {{\n    value * 2\n}}\n\n\
         fn checked(value: Int) -> Int {{\n    assert value > 100, \"nested\";\n    value\n}}\n"
    )
}

/// Messages of every kind: a string, values, calls, and messages that fail or raise themselves.
const MESSAGES: &[&str] = &[
    "\"a must be larger\"",
    "a",
    "[\"a must be larger\", a, b]",
    "(\"difference\", a - b)",
    "double(a)",
    "fn() => a",
    "a / 0",
    "a / b",
    "checked(a)",
    "{ raise \"inside\"; }",
    "{ assert b > a, \"other\"; a }",
    "if a > 5 { \"large\" } else { \"small\" }",
    "sha256(a as Bytes)",
];

#[test]
fn release_program_is_the_same_with_and_without_a_message() {
    let without = program(&source("", ""), false);

    // The assertion is the bare `(x)` that it is without debug symbols
    assert!(without.contains("(q 8)"), "{without}");

    for message in MESSAGES {
        assert_eq!(program(&source("", message), false), without, "{message}");
    }
}

/// A binding that's only used in the message isn't evaluated in a release build, unless it may
/// fail, in which case it's evaluated at its statement in both builds, as it is without a message.
#[test]
fn release_program_is_the_same_with_a_binding_only_used_in_the_message() {
    for binding in ["let sum = a + b;", "let quotient = a / b;"] {
        let name = binding.split(' ').nth(1).unwrap();

        assert_eq!(
            program(&source(binding, name), false),
            program(&source(binding, ""), false),
            "{binding}"
        );
    }

    let quotient = "let quotient = a / b;";

    for debug in [false, true] {
        // The division is evaluated whether the assertion fails or not
        assert_eq!(
            run(&source(quotient, "quotient"), debug, "(5 0)"),
            Err("Division by zero".to_string()),
        );
        assert!(run(&source(quotient, "quotient"), debug, "(-5 0)").is_err());

        assert_eq!(
            run(&source(quotient, "quotient"), debug, "(5 1)").as_deref(),
            Ok("4")
        );
    }
}

#[test]
fn debug_program_raises_the_location_and_the_message() {
    assert_eq!(
        run(&source("", ""), true, "(1 2)"),
        Err("\"assertion failed at main.rue:3:5\"".to_string())
    );

    for (message, raised) in [
        ("\"a must be larger\"", "\"a must be larger\""),
        ("a", "1"),
        ("[\"a must be larger\", a, b]", "(\"a must be larger\" 1 2)"),
        ("(\"difference\", a - b)", "(\"difference\" . -1)"),
        ("double(a)", "2"),
        ("if a > 5 { \"large\" } else { \"small\" }", "\"small\""),
    ] {
        assert_eq!(
            run(&source("", message), true, "(1 2)"),
            Err(format!("(\"assertion failed at main.rue:3:5\" {raised})")),
            "{message}"
        );
    }
}

/// Whatever the message is, the program fails exactly when the condition is false, and returns the
/// same value when it's true, in debug and release builds.
#[test]
fn message_never_changes_whether_the_program_fails() {
    for message in MESSAGES {
        let source = source("", message);

        for debug in [false, true] {
            assert_eq!(
                run(&source, debug, "(7 2)").as_deref(),
                Ok("5"),
                "{message}"
            );
            // The message isn't evaluated if the condition is true, even if it would fail
            assert_eq!(
                run(&source, debug, "(7 0)").as_deref(),
                Ok("7"),
                "{message}"
            );
            assert!(run(&source, debug, "(2 7)").is_err(), "{message}");
            assert!(run(&source, debug, "(0 0)").is_err(), "{message}");
        }

        assert_eq!(run(&source, false, "(2 7)"), Err("()".to_string()));
    }
}

/// A message that fails while it's evaluated replaces the failure of the assertion with its own,
/// which only happens in a debug build, once the program is already failing.
#[test]
fn message_that_fails_is_only_evaluated_in_debug_builds() {
    assert_eq!(
        run(&source("", "{ raise \"inside\"; }"), true, "(2 7)"),
        Err("(\"raise called at main.rue:3:21\" \"inside\")".to_string())
    );
    assert_eq!(
        run(&source("", "checked(a)"), true, "(2 7)"),
        Err("(\"assertion failed at main.rue:12:5\" \"nested\")".to_string())
    );
    assert_eq!(
        run(&source("", "a / 0"), true, "(2 7)"),
        Err("Division by zero".to_string())
    );
}

/// The message is only evaluated if the condition is false, so it sees the values that it's false
/// for, and what comes after the statement sees the values that it's true for.
#[test]
fn message_is_narrowed_by_the_condition_being_false() {
    let source = "fn main(value: Int | nil) -> Int {\n    \
                  assert !(value is nil), nil_only(value);\n    value + 1\n}\n\n\
                  fn nil_only(value: nil) -> String {\n    \"missing\"\n}\n";

    assert_eq!(
        run(source, true, "(())"),
        Err("(\"assertion failed at main.rue:2:5\" \"missing\")".to_string())
    );
    assert_eq!(run(source, true, "(5)").as_deref(), Ok("6"));

    // It doesn't see what the statements after it do
    let compiled = compile(
        &source
            .replace("nil_only(value)", "int_only(value)")
            .replace(
                "fn nil_only(value: nil) -> String",
                "fn int_only(value: Int) -> String",
            ),
    );
    assert_eq!(
        compiled.errors(),
        [r#"2:38 UnassignableType("nil", "Int")"#]
    );
}

/// Each assertion keeps its own location and message.
#[test]
fn consecutive_assertions_keep_their_own_messages() {
    let source = "fn main(a: Int, b: Int) -> Int {\n    assert a > 0, \"a is zero\";\n    \
                  assert b > 0;\n    assert a > b, [\"both\", a, b];\n    a - b\n}\n";

    assert_eq!(
        run(source, true, "(0 0)"),
        Err("(\"assertion failed at main.rue:2:5\" \"a is zero\")".to_string())
    );
    assert_eq!(
        run(source, true, "(1 0)"),
        Err("\"assertion failed at main.rue:3:5\"".to_string())
    );
    assert_eq!(
        run(source, true, "(1 1)"),
        Err("(\"assertion failed at main.rue:4:5\" (\"both\" 1 1))".to_string())
    );
    assert_eq!(run(source, true, "(2 1)").as_deref(), Ok("1"));
}

/// What's only used in a message counts as used, in both builds, like the value of a `raise`.
#[test]
fn message_uses_count_for_the_unused_lints() {
    let source = "fn main(a: Int, unused: Int) -> Int {\n    let sum = a + 1;\n    \
                  assert a > 0, [sum, unused, double(a)];\n    a\n}\n\n\
                  fn double(value: Int) -> Int {\n    value * 2\n}\n";

    for debug in [false, true] {
        assert_eq!(
            compile_with(source, |options| options.debug_symbols = debug).warnings(),
            Vec::<String>::new()
        );
    }

    assert_eq!(
        compile(&source.replace(", [sum, unused, double(a)]", "")).warnings(),
        [
            "1:17 UnusedParameter",
            "2:9 UnusedBinding",
            "7:4 UnusedFunction"
        ]
    );
}

/// A message is only evaluated some of the time, like the value of a `raise` in an `if`, so an
/// inline value that can fail and is only used in one can have its check skipped.
#[test]
fn inline_value_that_may_fail_in_a_message_is_reported() {
    let source = "fn main(a: Int, b: Int) -> Int {\n    inline let quotient = a / b;\n    \
                  assert a > 0, quotient;\n    a\n}\n";

    assert_eq!(compile(source).warnings(), ["2:5 InlineValueMayFail"]);
    assert_eq!(
        program(source, false),
        program(&source.replace(", quotient", ""), false)
    );
}

/// A const expression is evaluated without debug symbols, so a message isn't evaluated in one.
#[test]
fn const_evaluation_ignores_the_message() {
    let compiled = compile(
        "fn main() -> Int {\n    const {\n        assert 1 > 2, 1 / 0;\n        1\n    }\n}\n",
    );
    assert_eq!(compiled.errors(), ["2:5 ConstEvalFailed(\"clvm raise\")"]);

    let compiled = compile(
        "fn main() -> Int {\n    const {\n        assert 2 > 1, 1 / 0;\n        1\n    }\n}\n",
    );
    compiled.assert_no_errors();
    assert_eq!(
        compiled
            .const_results()
            .into_iter()
            .map(|(_, value)| value)
            .collect::<Vec<_>>(),
        ["1"]
    );
}

/// The condition is still checked without the message, and a missing message is an error.
#[test]
fn message_must_be_an_expression() {
    for (source, error) in [
        (
            "assert a > 0, ;",
            "UnexpectedToken(\"`;`\", [\"expression\"])",
        ),
        ("assert a > 0, \"a\", \"b\";", "UnexpectedToken(\"`,`\""),
        ("assert a > 0 \"a\";", "UnexpectedToken(\"string literal\""),
        ("assert a > 0, missing;", "UndeclaredSymbol(\"missing\")"),
    ] {
        let compiled = compile(&format!(
            "fn main(a: Int) -> Int {{\n    {source}\n    a\n}}\n"
        ));
        let errors = compiled.errors();
        assert!(
            errors.iter().any(|message| message.contains(error)),
            "{source}: {errors:?}"
        );
    }
}
