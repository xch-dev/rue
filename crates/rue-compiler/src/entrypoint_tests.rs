//! Constants as the program: `main` and the exports can be constants as well as functions.

use std::{path::Path, sync::Arc};

use clvmr::{Allocator, ChiaDialect, NodePtr, run_program, serde::node_to_bytes};
use rue_diagnostic::{DiagnosticSeverity, Source, SourceKind};
use rue_lir::CLVM_FLAGS;
use rue_options::CompilerOptions;

use crate::{Compiler, File, FileTree};

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

/// Each export of [`SOURCE`] with its serialized program, compiled with the options.
fn exports(options: CompilerOptions) -> (Allocator, Vec<(String, NodePtr)>) {
    let mut ctx = Compiler::new(CompilerOptions {
        std: false,
        ..options
    });
    let kind = SourceKind::File("main.rue".to_string());
    let tree = FileTree::File(File::new(
        &mut ctx,
        "main".to_string(),
        Source::new(Arc::from(SOURCE), kind.clone()),
    ));
    tree.compile(&mut ctx);

    let errors: Vec<_> = ctx
        .take_diagnostics()
        .into_iter()
        .filter(|diagnostic| diagnostic.severity == DiagnosticSeverity::Error)
        .collect();
    assert!(errors.is_empty(), "{errors:?}");

    let mut allocator = Allocator::new();
    let exports = tree
        .exports(&mut ctx, &mut allocator, Some(&kind), None, Path::new("."))
        .unwrap()
        .into_iter()
        .map(|export| (export.name, export.ptr))
        .collect();

    (allocator, exports)
}

fn program<'a>(exports: &'a [(String, NodePtr)], name: &str) -> NodePtr {
    exports
        .iter()
        .find(|(export, _)| export == name)
        .unwrap_or_else(|| panic!("`{name}` is exported"))
        .1
}

/// Runs the program with the solution, and returns its output.
fn run(allocator: &mut Allocator, program: NodePtr, solution: &[i64]) -> Option<Vec<u8>> {
    let mut env = NodePtr::NIL;
    for &value in solution.iter().rev() {
        let value = allocator.new_number(value.into()).unwrap();
        env = allocator.new_pair(value, env).unwrap();
    }

    let output = run_program(
        allocator,
        &ChiaDialect::new(CLVM_FLAGS),
        program,
        env,
        u64::MAX,
    )
    .ok()?
    .1;

    Some(node_to_bytes(allocator, output).unwrap())
}

fn serialized(allocator: &mut Allocator, f: impl FnOnce(&mut Allocator) -> NodePtr) -> Vec<u8> {
    let node = f(allocator);
    node_to_bytes(allocator, node).unwrap()
}

fn int(allocator: &mut Allocator, value: i64) -> Vec<u8> {
    serialized(allocator, |allocator| {
        allocator.new_number(value.into()).unwrap()
    })
}

#[test]
fn constant_exports_evaluate_to_their_values() {
    for options in [CompilerOptions::default(), CompilerOptions::debug()] {
        let (mut allocator, exports) = exports(options);

        let sq = program(&exports, "SQ");
        let expected = int(&mut allocator, 25);
        assert_eq!(run(&mut allocator, sq, &[]), Some(expected));

        let pair = program(&exports, "PAIR");
        let expected = serialized(&mut allocator, |allocator| {
            let first = allocator.new_number(4.into()).unwrap();
            let rest = allocator.new_number(5.into()).unwrap();
            allocator.new_pair(first, rest).unwrap()
        });
        assert_eq!(run(&mut allocator, pair, &[]), Some(expected));
    }
}

#[test]
fn function_constant_exports_behave_like_the_function() {
    for options in [CompilerOptions::default(), CompilerOptions::debug()] {
        let (mut allocator, exports) = exports(options);

        for (name, solution, expected) in [
            ("F", 3, 10),
            ("ALIAS", 3, 10),
            ("G", 3, 5),
            ("LAMBDA", 3, 19),
            ("f", 3, 10),
        ] {
            let ptr = program(&exports, name);
            let expected = int(&mut allocator, expected);
            assert_eq!(
                run(&mut allocator, ptr, &[solution]),
                Some(expected),
                "{name} ({options:?})"
            );
        }

        // The function's assertion still fails the program
        for name in ["F", "ALIAS", "LAMBDA"] {
            let ptr = program(&exports, name);
            assert_eq!(run(&mut allocator, ptr, &[0]), None, "{name} ({options:?})");
        }

        // A constant that names a function is that function's program
        let f = node_to_bytes(&allocator, program(&exports, "f")).unwrap();
        assert_eq!(
            node_to_bytes(&allocator, program(&exports, "F")).unwrap(),
            f
        );

        // A constant that isn't a function evaluates to its value, even if that's a function,
        // which can then be called
        let value = program(&exports, "VALUE");
        assert_ne!(node_to_bytes(&allocator, value).unwrap(), f);
        let closure = run_program(
            &mut allocator,
            &ChiaDialect::new(CLVM_FLAGS),
            value,
            NodePtr::NIL,
            u64::MAX,
        )
        .unwrap()
        .1;
        let expected = int(&mut allocator, 10);
        assert_eq!(run(&mut allocator, closure, &[3]), Some(expected));

        // The rest parameter takes the rest of the solution
        let expected = serialized(&mut allocator, |allocator| {
            let mut list = NodePtr::NIL;
            for value in [3, 2, 1] {
                let value = allocator.new_number(value.into()).unwrap();
                list = allocator.new_pair(value, list).unwrap();
            }
            list
        });
        for name in ["REST", "REST_LAMBDA"] {
            let ptr = program(&exports, name);
            assert_eq!(
                run(&mut allocator, ptr, &[1, 2, 3]),
                Some(expected.clone()),
                "{name} ({options:?})"
            );
        }
    }
}
