//! A function that's used as a value is the program it's compiled to on its own, so its tree hash
//! is the same in a const expression, at runtime in any other program, and from `rue build`.

use std::path::Path;

use clvm_utils::tree_hash;
use clvmr::{Allocator, NodePtr, SExp};
use rue_diagnostic::{DiagnosticKind, SourceKind};
use rue_options::CompilerOptions;

use crate::test_utils::{Compiled, compile_with_options, run_with_solution};

/// The standard library's `tree_hash`, which the tests don't compile.
const TREE_HASH: &str = "
fn hash(value: Any) -> Bytes32 {
    if value is Bytes {
        sha256(1 as Bytes + value)
    } else {
        sha256(2 as Bytes + hash(value.first) + hash(value.rest))
    }
}
";

/// The program of the export.
fn export(compiled: &mut Compiled, allocator: &mut Allocator, name: &str) -> NodePtr {
    let kind = SourceKind::File("main.rue".to_string());
    let exports = compiled
        .tree
        .exports(
            &mut compiled.ctx,
            allocator,
            Some(&kind),
            Some(name),
            Path::new("."),
        )
        .unwrap();
    let [export] = exports.as_slice() else {
        panic!("`{name}` is exported once");
    };
    export.ptr
}

/// The tree hash of the export's program, as `rue build --hash` prints it, with the options.
fn build_hash(source: &str, options: CompilerOptions, name: &str) -> String {
    let mut compiled = compile_with_options(source, options);
    compiled.assert_no_errors();
    let mut allocator = Allocator::new();
    let program = export(&mut compiled, &mut allocator, name);
    format!("0x{}", tree_hash(&allocator, program))
}

/// What the export returns for the solution, compiled with the options.
fn output(source: &str, options: CompilerOptions, name: &str, solution: &str) -> String {
    let mut compiled = compile_with_options(source, options);
    compiled.assert_no_errors();
    let mut allocator = Allocator::new();
    let program = export(&mut compiled, &mut allocator, name);
    run_with_solution(&mut allocator, program, solution).unwrap()
}

/// `helper` is called twice in `puzzle`, which inlines it when it's built on its own. Before
/// functions used as values were built on their own, a const expression didn't inline it, so
/// the hash it found was that of a different program.
const HELPER_CALLED_TWICE: &str = "
fn helper(x: Int) -> Int {
    x * 2 + 1
}

export fn puzzle(x: Int) -> Int {
    helper(x) + helper(x + 1)
}

export fn const_hash() -> Bytes32 {
    const { hash(puzzle) }
}

export fn runtime_hash() -> Bytes32 {
    hash(puzzle)
}

export fn value() -> fn(x: Int) -> Int {
    puzzle
}
";

#[test]
fn const_and_runtime_hashes_are_the_hash_of_the_release_build() {
    let source = format!("{TREE_HASH}{HELPER_CALLED_TWICE}");
    let expected = build_hash(&source, CompilerOptions::default(), "puzzle");

    // The program is built for release in a debug build too, so a test sees the hash of the
    // program that's deployed
    for options in [CompilerOptions::default(), CompilerOptions::debug()] {
        for name in ["const_hash", "runtime_hash"] {
            assert_eq!(
                output(&source, options, name, "()"),
                expected,
                "{name} ({options:?})"
            );
        }
    }
}

#[test]
fn function_value_is_the_release_build_as_a_constant() {
    let source = format!("{TREE_HASH}{HELPER_CALLED_TWICE}");
    let expected = build_hash(&source, CompilerOptions::default(), "puzzle");

    for options in [CompilerOptions::default(), CompilerOptions::debug()] {
        let mut compiled = compile_with_options(&source, options);
        compiled.assert_no_errors();
        let mut allocator = Allocator::new();
        let value = export(&mut compiled, &mut allocator, "value");

        // `(q . PROGRAM)`
        let SExp::Pair(quote, program) = allocator.sexp(value) else {
            panic!("the value is quoted ({options:?})");
        };
        assert_eq!(allocator.atom(quote).as_ref(), [1], "{options:?}");
        assert_eq!(
            format!("0x{}", tree_hash(&allocator, program)),
            expected,
            "{options:?}"
        );
    }
}

/// `other` calls `helper` with large arguments, so inlining it there makes `other` larger, and it
/// isn't. `puzzle` is smaller with it inlined. Lowered as part of `other`, `puzzle` would have
/// called `helper` instead, and been a different program.
const INLINED_DIFFERENTLY: &str = "
fn helper(x: Int) -> Int {
    x * x * x + x * 3 + x * 7 + 11
}

export fn puzzle(x: Int) -> Int {
    helper(x) + helper(x)
}

export fn other(a: Int, b: Int, c: Int, d: Int) -> (Int, Bytes32) {
    let total = helper(a * b + c * d + a * c + b * d) + helper(b * c + a * d)
        + helper(c * a + d * b + 5) + helper(d * a + b * c + a)
        + helper(a + b + c + d + 9) + helper(a * b * c * d);
    (total, hash(puzzle))
}
";

#[test]
fn runtime_hash_ignores_what_the_other_program_inlines() {
    let source = format!("{TREE_HASH}{INLINED_DIFFERENTLY}");
    let expected = build_hash(&source, CompilerOptions::default(), "puzzle");

    for options in [CompilerOptions::default(), CompilerOptions::debug()] {
        let output = output(&source, options, "other", "(1 2 3 4)");
        assert_eq!(output, format!("(43851 . {expected})"), "{options:?}");
    }
}

#[test]
fn const_hash_uses_the_target() {
    let source = format!("{TREE_HASH}{HELPER_CALLED_TWICE}");

    for options in [
        CompilerOptions {
            operand_limits: true,
            ..CompilerOptions::default()
        },
        CompilerOptions {
            hard_fork_2: true,
            ..CompilerOptions::default()
        },
    ] {
        let expected = build_hash(&source, options, "puzzle");
        assert_eq!(
            output(&source, options, "const_hash", "()"),
            expected,
            "{options:?}"
        );
    }
}

#[test]
fn generic_function_value_is_the_same_program_for_each_type() {
    let source = format!(
        "{TREE_HASH}
fn id<T>(value: T) -> T {{
    value
}}

export const ID: fn(value: Int) -> Int = id;

export fn ints() -> Bytes32 {{
    hash(id::<Int>)
}}

export fn bytes() -> Bytes32 {{
    const {{ hash(id::<Bytes>) }}
}}
"
    );

    let expected = build_hash(&source, CompilerOptions::default(), "ID");

    for name in ["ints", "bytes"] {
        assert_eq!(
            output(&source, CompilerOptions::default(), name, "()"),
            expected,
            "{name}"
        );
    }
}

#[test]
fn functions_that_use_each_other_as_values_are_closures() {
    // Neither program could contain the other, so they're built at runtime as before
    let source = format!(
        "{TREE_HASH}
fn ping(n: Int) -> Bytes32 {{
    if n > 0 {{ pong(n - 1) }} else {{ hash(pong) }}
}}

fn pong(n: Int) -> Bytes32 {{
    if n > 0 {{ ping(n - 1) }} else {{ hash(ping) }}
}}

fn call(f: fn(n: Int) -> Bytes32, n: Int) -> Bytes32 {{
    f(n)
}}

export fn main(n: Int) -> (Bytes32, Bytes32) {{
    (call(ping, n), call(pong, n))
}}
"
    );

    for options in [CompilerOptions::default(), CompilerOptions::debug()] {
        let mut compiled = compile_with_options(&source, options);
        compiled.assert_no_errors();
        let mut allocator = Allocator::new();
        let program = export(&mut compiled, &mut allocator, "main");
        assert!(
            run_with_solution(&mut allocator, program, "(3)").is_ok(),
            "{options:?}"
        );
    }
}

#[test]
fn function_that_hashes_itself_in_a_const_expression_is_an_error() {
    let source = format!(
        "{TREE_HASH}
export fn puzzle() -> Bytes32 {{
    const {{ hash(puzzle) }}
}}
"
    );

    let compiled = compile_with_options(&source, CompilerOptions::default());

    assert!(
        compiled
            .diagnostics
            .iter()
            .any(|diagnostic| matches!(diagnostic.kind, DiagnosticKind::RecursiveConstExpr)),
        "{:?}",
        compiled.errors()
    );
}
