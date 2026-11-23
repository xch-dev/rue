//! A function that's used as a value is the program it's compiled to on its own, so its tree hash
//! is the same in a const expression, at runtime in any other program, and from `rue build`.

use std::path::Path;

use clvm_utils::tree_hash;
use clvmr::{Allocator, ChiaDialect, NodePtr, run_program};
use rue_diagnostic::{DiagnosticKind, SourceKind};
use rue_lir::ClvmTarget;
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
fn function_value_is_the_release_build() {
    let source = format!("{TREE_HASH}{HELPER_CALLED_TWICE}");
    let expected = build_hash(&source, CompilerOptions::default(), "puzzle");

    for options in [CompilerOptions::default(), CompilerOptions::debug()] {
        let mut compiled = compile_with_options(&source, options);
        compiled.assert_no_errors();
        let mut allocator = Allocator::new();
        let program = export(&mut compiled, &mut allocator, "value");
        let dialect = ChiaDialect::new(ClvmTarget::default().flags());
        let value = run_program(&mut allocator, &dialect, program, NodePtr::NIL, u64::MAX)
            .unwrap()
            .1;

        assert_eq!(
            format!("0x{}", tree_hash(&allocator, value)),
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
    // `LIMITS` keeps the products from being folded into one, and hard fork 2 changes the opcode
    // of `secp256k1_verify`, so each target compiles the puzzles to a different program
    let source = format!(
        "{TREE_HASH}
export fn product(x: Int) -> Int {{
    2 * x * 3
}}

export fn verify(public_key: K1PublicKey, message: Bytes32, signature: K1Signature) {{
    secp256k1_verify(public_key, message, signature);
}}

export fn product_hash() -> Bytes32 {{
    const {{ hash(product) }}
}}

export fn verify_hash() -> Bytes32 {{
    const {{ hash(verify) }}
}}
"
    );

    let today = CompilerOptions::default();
    let operand_limits = CompilerOptions {
        operand_limits: true,
        ..today
    };
    let hard_fork_2 = CompilerOptions {
        hard_fork_2: true,
        ..today
    };

    for (puzzle, other) in [("product", operand_limits), ("verify", hard_fork_2)] {
        assert_ne!(
            build_hash(&source, today, puzzle),
            build_hash(&source, other, puzzle),
            "{puzzle} is the same program for both targets"
        );

        for options in [today, other, CompilerOptions::debug(), other.release()] {
            assert_eq!(
                output(&source, options, &format!("{puzzle}_hash"), "()"),
                build_hash(&source, options.release(), puzzle),
                "{puzzle} ({options:?})"
            );
        }
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
fn ping(n: Int) -> Int {{
    if n > 0 {{ pong(n - 1) + 1 }} else {{ hash(pong).length }}
}}

fn pong(n: Int) -> Int {{
    if n > 0 {{ ping(n - 1) + 1 }} else {{ hash(ping).length }}
}}

fn call(f: fn(n: Int) -> Int, n: Int) -> Int {{
    f(n)
}}

export fn main(n: Int) -> (Int, Int) {{
    (call(ping, n), call(pong, n))
}}
"
    );

    for options in [CompilerOptions::default(), CompilerOptions::debug()] {
        assert_eq!(
            output(&source, options, "main", "(3)"),
            "(35 . 35)",
            "{options:?}"
        );
    }
}

/// A `debug` statement is only lowered in a debug build, and the programs of functions used as
/// values are release builds, so a reference in one doesn't make the functions refer to each
/// other's programs.
#[test]
fn reference_in_a_debug_statement_is_not_a_cycle() {
    let source = format!(
        "{TREE_HASH}{INLINED_DIFFERENTLY}
export fn first() -> Bytes32 {{
    hash(second)
}}

export fn second() -> Int {{
    debug hash(first);
    1
}}

export fn first_hash() -> Bytes32 {{
    const {{ hash(first) }}
}}

export fn runtime_hash() -> Bytes32 {{
    hash(first)
}}
"
    );

    let expected = build_hash(&source, CompilerOptions::default(), "first");

    for options in [CompilerOptions::default(), CompilerOptions::debug()] {
        for name in ["first_hash", "runtime_hash"] {
            assert_eq!(
                output(&source, options, name, "()"),
                expected,
                "{name} ({options:?})"
            );
        }
    }
}

/// A function that's used as a value more than once is in the program once, like a constant
/// that's referenced more than once.
#[test]
fn function_used_as_a_value_more_than_once_is_bound_once() {
    let source = "
fn big(x: Int) -> Int {
    x * x * x + x * 3 + x * 7 + 11 + x * 13 + x * 17 + x * 19 + x * 23 + x * 29
}

fn apply(f: fn(x: Int) -> Int, x: Int) -> Int {
    f(x)
}

export fn main(x: Int) -> Int {
    apply(big, x) + apply(big, x + 1) + apply(big, x + 2) + apply(big, x * 5)
}
";

    for options in [CompilerOptions::default(), CompilerOptions::debug()] {
        let mut compiled = compile_with_options(source, options);
        compiled.assert_no_errors();
        let mut allocator = Allocator::new();
        let program = export(&mut compiled, &mut allocator, "main");
        let disassembled = rue_clvm::disassemble_program(&allocator, program);

        assert_eq!(
            disassembled.matches("(q . 29)").count(),
            1,
            "{disassembled}"
        );
        assert_eq!(
            run_with_solution(&mut allocator, program, "(1)").unwrap(),
            "1426",
            "{options:?}"
        );
    }
}

/// The const expressions in a function that's used as a value in another const expression are
/// evaluated first, wherever they're declared, since the function's program is compiled with
/// their values.
#[test]
fn const_expressions_in_a_function_value_are_evaluated_first() {
    let source = format!(
        "{TREE_HASH}
export fn const_hash() -> Bytes32 {{
    const {{ hash(puzzle) }}
}}

export fn puzzle(x: Int) -> Int {{
    x + const {{ hash(42) }}.length
}}
"
    );

    assert_eq!(
        output(&source, CompilerOptions::default(), "const_hash", "()"),
        build_hash(&source, CompilerOptions::default(), "puzzle")
    );
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
