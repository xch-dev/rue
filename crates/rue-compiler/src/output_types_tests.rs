//! Displaying the values that programs return and print, using the types the compiler gave them.

use std::{path::Path, sync::Arc};

use clvmr::{Allocator, NodePtr, run_program, serde::node_to_bytes};
use rue_diagnostic::{DiagnosticSeverity, Source, SourceKind};
use rue_hir::SymbolId;
use rue_lir::{CLVM_FLAGS, DebugDialect};
use rue_options::CompilerOptions;
use rue_types::DisplayOptions;

use crate::{Compiler, File, FileTree, OutputTypes};

struct Output {
    /// The value `main` returned, as displayed with its return type.
    value: String,
    /// The values printed by `debug` statements, as displayed with their types.
    prints: Vec<String>,
    /// The value `main` returned, serialized.
    bytes: Vec<u8>,
}

/// Compiles the source in debug mode, then runs `main` and displays its output.
fn run(source: &str, options: DisplayOptions) -> Output {
    let mut ctx = Compiler::new(CompilerOptions::debug());
    let source = Source::new(Arc::from(source), SourceKind::File("main.rue".to_string()));
    let file = File::new(&mut ctx, "main".to_string(), source.clone());
    let module = file.module;
    let tree = FileTree::File(file);
    tree.compile(&mut ctx);

    let errors = ctx
        .take_diagnostics()
        .into_iter()
        .filter(|diagnostic| diagnostic.severity == DiagnosticSeverity::Error)
        .map(|diagnostic| format!("{} {:?}", diagnostic.start(), diagnostic.kind))
        .collect::<Vec<_>>();
    assert!(errors.is_empty(), "{errors:?}\n{}", source.text);

    let main: SymbolId = ctx
        .scope(ctx.module(module).scope)
        .symbol("main")
        .expect("missing main");

    let mut allocator = Allocator::new();
    let program = tree
        .main(&mut ctx, &mut allocator, &source.kind, ".".into())
        .unwrap()
        .unwrap();

    let types = Arc::new(OutputTypes::new(&mut ctx, [main], Path::new(".")));

    let print_types = types.clone();
    let dialect =
        DebugDialect::new(CLVM_FLAGS, false).with_display(move |srcloc, allocator, node| {
            print_types.display_debug(srcloc, allocator, node, options)
        });

    let output = run_program(&mut allocator, &dialect, program, NodePtr::NIL, u64::MAX)
        .unwrap()
        .1;

    Output {
        value: types
            .display_return(main, &allocator, output, options)
            .unwrap(),
        prints: dialect
            .log()
            .into_iter()
            .map(|print| print.message)
            .collect(),
        bytes: node_to_bytes(&allocator, output).unwrap(),
    }
}

/// Runs `main`, which returns `expr` as `ty`, and checks how its value is displayed. Also checks
/// that the displayed value is a Rue expression of the same type, which evaluates to the same
/// value.
#[track_caller]
fn check(items: &str, ty: &str, expr: &str, expected: &str) {
    check_with(items, ty, expr, expected, DisplayOptions::SINGLE_LINE);
}

#[track_caller]
fn check_with(items: &str, ty: &str, expr: &str, expected: &str, options: DisplayOptions) {
    let output = run(
        &format!("{items}\nfn main() -> {ty} {{\n{expr}\n}}\n"),
        options,
    );
    assert_eq!(output.value, expected);

    let roundtrip = run(
        &format!("{items}\nfn main() -> {ty} {{\n{expected}\n}}\n"),
        options,
    );
    assert_eq!(
        hex::encode(roundtrip.bytes),
        hex::encode(output.bytes),
        "`{expected}` evaluates to a different value"
    );
}

#[test]
fn atoms_are_displayed_by_their_semantics() {
    check("", "Int", "42", "42");
    check("", "Int", "-1000", "-1000");
    check("", "Int", "0", "0");
    check(
        "",
        "Int",
        "18446744073709551616 * 18446744073709551616",
        "340282366920938463463374607431768211456",
    );
    check("", "Bytes", "0xcafef00d", "0xcafef00d");
    check("", "Bytes", "nil", "nil");
    check("", "Bool", "true", "true");
    check("", "Bool", "false", "false");
    check("", "Bool", "1 > 2", "false");
    check("", "String", "\"hello\"", "\"hello\"");
    check("", "String", "\"\"", "\"\"");
    check("", "String", "\"hi\"", "\"hi\"");
    check("", "nil", "nil", "nil");
}

#[test]
fn atoms_that_look_like_other_atoms_keep_their_type() {
    // The disassembler would guess that these are an integer and a string
    check("", "Bytes", "0x01", "0x01");
    check("", "Int", "6513249", "6513249");
    check(
        "",
        "Bytes32",
        &format!("0x{}", "ab".repeat(32)),
        &format!("0x{}", "ab".repeat(32)),
    );
}

#[test]
fn strings_that_cannot_be_written_as_literals_are_written_as_bytes() {
    check_display_only("", "String", "\"a\" + 0x22 as String", "0x6122");
}

#[test]
fn concatenated_strings_are_strings() {
    check("", "String", "\"hello, \" + \"world\"", "\"hello, world\"");
    check("", "Bytes", "\"a\" + 0x01", "0x6101");

    // A debug print is displayed with the type of its expression, rather than a declared type
    let output = run(
        "fn main() -> nil {\n\
             debug \"hello, \" + \"world\";\n\
             debug \"a\" + 0x01;\n\
             nil\n\
         }\n",
        DisplayOptions::SINGLE_LINE,
    );

    assert_eq!(output.prints, ["\"hello, world\"", "0x6101"]);
}

#[test]
fn bytes_are_written_as_bytes_even_if_they_are_text() {
    check("", "Bytes", "\"hello\"", "0x68656c6c6f");
}

#[test]
fn items_that_dont_match_their_type_dont_affect_the_rest() {
    let point = "struct Point { x: Int, y: Int }";
    check_display_only(
        point,
        "List<Point>",
        "unchecked_cast([Point { x: 1, y: 2 }, 3, Point { x: 4, y: 5 }])",
        "[Point { x: 1, y: 2 }, 3, Point { x: 4, y: 5 }]",
    );
}

#[test]
fn integers_with_redundant_leading_bytes_are_written_as_bytes() {
    check_display_only("", "Int", "0x0001 as Int", "0x0001");
}

#[test]
fn lists_are_displayed_as_lists() {
    check("", "List<Int>", "[1, 2, 3]", "[1, 2, 3]");
    check("", "List<Int>", "[]", "[]");
    check("", "List<String>", "[\"a\", \"b\"]", "[\"a\", \"b\"]");
    check(
        "",
        "List<List<Int>>",
        "[[1], [], [2, 3]]",
        "[[1], [], [2, 3]]",
    );
    check("", "List<Bool>", "[true, false]", "[true, false]");
}

#[test]
fn pairs_are_displayed_as_pairs() {
    check("", "(Int, Int)", "(1, 2)", "(1, 2)");
    check(
        "",
        "(Int, (Bytes, Bool))",
        "(1, (0x02, true))",
        "(1, (0x02, true))",
    );
    check("", "(String, nil)", "(\"a\", nil)", "(\"a\", nil)");
    check("", "(Int, Int) | nil", "(1, 2)", "(1, 2)");
    check("", "(Int, Int) | nil", "nil", "nil");
    check("", "List<Int> | nil", "nil", "[]");
    check("", "(Int, Bool)", "(1, true)", "(1, true)");
}

#[test]
fn structs_are_displayed_with_their_fields() {
    let point = "struct Point { x: Int, y: Int }";
    check(
        point,
        "Point",
        "Point { x: 1, y: -2 }",
        "Point { x: 1, y: -2 }",
    );
    check(
        point,
        "List<Point>",
        "[Point { x: 1, y: 2 }, Point { x: 3, y: 4 }]",
        "[Point { x: 1, y: 2 }, Point { x: 3, y: 4 }]",
    );

    let nested =
        "struct Line { start: Point, end: Point, label: String }\nstruct Point { x: Int, y: Int }";
    check(
        nested,
        "Line",
        "Line { start: Point { x: 0, y: 0 }, end: Point { x: 1, y: 1 }, label: \"diagonal\" }",
        "Line { start: Point { x: 0, y: 0 }, end: Point { x: 1, y: 1 }, label: \"diagonal\" }",
    );
}

#[test]
fn generic_structs_are_displayed_with_their_arguments() {
    let wrapper = "struct Wrapper<T> { value: T, flag: Bool }";
    check(
        wrapper,
        "Wrapper<Bytes>",
        "Wrapper::<Bytes> { value: 0x01, flag: true }",
        "Wrapper::<Bytes> { value: 0x01, flag: true }",
    );
    check(
        wrapper,
        "Wrapper<Int>",
        "Wrapper::<Int> { value: 1, flag: true }",
        "Wrapper::<Int> { value: 1, flag: true }",
    );
}

#[test]
fn unions_are_displayed_as_the_variant_the_value_matches() {
    let shapes = "struct Circle { radius: Int }\nstruct Rect { width: Int, height: Int }\ntype Shape = Circle | Rect;";
    check(
        shapes,
        "List<Shape>",
        "[Circle { radius: 5 }, Rect { width: 2, height: 3 }]",
        "[Circle { radius: 5 }, Rect { width: 2, height: 3 }]",
    );

    // Nil is more specific than any integer, even though it's also zero
    check("", "Int | nil", "nil", "nil");
    check("", "Int | nil", "5", "5");
    check(
        "",
        "Bytes32 | Int",
        &format!("0x{}", "00".repeat(32)),
        &format!("0x{}", "00".repeat(32)),
    );
}

#[test]
fn values_without_a_useful_type_are_guessed() {
    // Without a type, a short atom is guessed to be an integer, the way the disassembler does
    check_display_only(
        "",
        "Any",
        "[1, \"hello\", 0xcafef00d]",
        "[1, \"hello\", -889262067]",
    );
    check_display_only(
        "",
        "Any",
        &format!("0x{}", "ab".repeat(32)),
        &format!("0x{}", "ab".repeat(32)),
    );
    check_display_only("", "Any", "(1, 2)", "(1, 2)");
}

#[test]
fn long_values_are_broken_over_lines() {
    let point = "struct Point { x: Int, y: Int }";
    check_with(
        point,
        "List<Point>",
        "[Point { x: 1, y: 2 }, Point { x: 3, y: 4 }, Point { x: 5, y: 6 }]",
        "[\n    Point { x: 1, y: 2 },\n    Point { x: 3, y: 4 },\n    Point { x: 5, y: 6 },\n]",
        DisplayOptions::multiline(40),
    );
}

#[test]
fn debug_prints_are_displayed_with_their_types() {
    let output = run(
        "struct Point { x: Int, y: Int }\n\
         fn main() -> nil {\n\
             debug Point { x: 1, y: 2 };\n\
             debug 0xcafef00d;\n\
             debug [true, false];\n\
             nil\n\
         }\n",
        DisplayOptions::SINGLE_LINE,
    );

    assert_eq!(
        output.prints,
        ["Point { x: 1, y: 2 }", "0xcafef00d", "[true, false]"]
    );
}

/// Like `check`, but for values whose displayed expression has a different type, or can't be
/// written as a Rue expression of the type at all.
#[track_caller]
fn check_display_only(items: &str, ty: &str, expr: &str, expected: &str) {
    let output = run(
        &format!("{items}\nfn main() -> {ty} {{\n{expr}\n}}\n"),
        DisplayOptions::SINGLE_LINE,
    );
    assert_eq!(output.value, expected);
}
