//! Constants as the program: `main` and the exports can be constants as well as functions.

use std::path::Path;

use clvmr::{Allocator, ChiaDialect, NodePtr, run_program};
use rue_clvm::{assemble, disassemble_program, disassemble_value};
use rue_diagnostic::SourceKind;
use rue_lir::clvm_flags;
use rue_options::CompilerOptions;

use crate::test_utils::{Compiled, compile_with};

const SOURCE: &str = "
fn main() -> Int { 1 }
fn sq(x: Int) -> Int { x * x }
export fn f(x: Int) -> Int { assert x > 0; x * x + 1 }
inline fn g(x: Int) -> Int { x + 2 }
fn rest(...xs: List<Int>) -> List<Int> { xs }

export const SQ: Int = sq(5);
export const PAIR: (Int, Int) = (sq(2), f(2));
export const F: fn(x: Int) -> Int = f;
export const G: fn(x: Int) -> Int = g;
export const LAMBDA: fn(x: Int) -> Int = fn(x: Int) => f(x) + sq(x);
export const ALIAS: fn(x: Int) -> Int = F;
const REST_FN: fn(...xs: List<Int>) -> List<Int> = rest;
export const REST: fn(...xs: List<Int>) -> List<Int> = REST_FN;
export const REST_LAMBDA: fn(...xs: List<Int>) -> List<Int> = fn(...xs: List<Int>) => rest(...xs);
export const VALUE: Any = f;
";

/// [`SOURCE`], compiled with the options.
fn compile(options: CompilerOptions) -> Compiled {
    let compiled = compile_with(SOURCE, |o| {
        *o = CompilerOptions {
            std: false,
            ..options
        };
    });
    compiled.assert_no_errors();
    compiled
}

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

/// Runs the program with the solution, and returns its output, or `None` if it fails.
fn run(allocator: &mut Allocator, program: NodePtr, solution: &str) -> Option<NodePtr> {
    let solution = assemble(allocator, solution).unwrap();
    let dialect = ChiaDialect::new(clvm_flags(true));
    Some(
        run_program(allocator, &dialect, program, solution, u64::MAX)
            .ok()?
            .1,
    )
}

/// Runs the export with the solution, and returns its output, or `None` if it fails.
fn output(compiled: &mut Compiled, name: &str, solution: &str) -> Option<String> {
    let mut allocator = Allocator::new();
    let program = export(compiled, &mut allocator, name);
    let output = run(&mut allocator, program, solution)?;
    Some(disassemble_value(&allocator, output))
}

#[test]
fn constant_exports_evaluate_to_their_values() {
    for options in [CompilerOptions::default(), CompilerOptions::debug()] {
        let mut compiled = compile(options);

        assert_eq!(output(&mut compiled, "SQ", "()").as_deref(), Some("25"));
        assert_eq!(
            output(&mut compiled, "PAIR", "()").as_deref(),
            Some("(4 . 5)")
        );
    }
}

#[test]
fn function_constant_exports_behave_like_the_function() {
    for options in [CompilerOptions::default(), CompilerOptions::debug()] {
        let mut compiled = compile(options);

        for (name, expected) in [
            ("f", "10"),
            ("F", "10"),
            ("ALIAS", "10"),
            ("G", "5"),
            ("LAMBDA", "19"),
        ] {
            assert_eq!(
                output(&mut compiled, name, "(3)").as_deref(),
                Some(expected),
                "{name} ({options:?})"
            );
        }

        // The function's assertion still fails the program
        for name in ["F", "ALIAS", "LAMBDA"] {
            assert_eq!(output(&mut compiled, name, "(0)"), None, "{name}");
        }

        // The rest parameter takes the rest of the solution
        for name in ["REST", "REST_LAMBDA"] {
            assert_eq!(
                output(&mut compiled, name, "(1 2 3)").as_deref(),
                Some("(1 2 3)"),
                "{name} ({options:?})"
            );
        }

        let mut allocator = Allocator::new();

        // A constant that names a function is that function's program
        let f = export(&mut compiled, &mut allocator, "f");
        let constant = export(&mut compiled, &mut allocator, "F");
        assert_eq!(
            disassemble_program(&allocator, constant),
            disassemble_program(&allocator, f)
        );

        // A constant that isn't a function evaluates to its value, even if that's a function,
        // which can then be called
        let value = export(&mut compiled, &mut allocator, "VALUE");
        assert_ne!(
            disassemble_program(&allocator, value),
            disassemble_program(&allocator, f)
        );
        let closure = run(&mut allocator, value, "()").unwrap();
        let output = run(&mut allocator, closure, "(3)").unwrap();
        assert_eq!(disassemble_value(&allocator, output), "10");
    }
}
