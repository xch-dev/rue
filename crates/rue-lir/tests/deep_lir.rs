//! Optimization and code generation recurse once per level of LIR, which can be nested as deeply
//! as a list literal is long, since a list is a chain of pairs. These tests build deep LIR and
//! make sure both passes get through it on a 2 MiB stack, the size of the language server's
//! worker thread and of the threads that tests run on.
//!
//! They're in their own test binary, since a stack overflow aborts the whole process.

use clvmr::{Allocator, serde::node_to_bytes};
use id_arena::Arena;
use rue_lir::{CodegenOptions, Lir, LirId, codegen, optimize};

/// How deeply each test nests LIR. Without growing the stack, this overflows a 2 MiB stack in
/// both debug and release builds.
const DEPTH: usize = 20_000;

/// The checks for whether a list is static recurse only in debug builds, since release builds
/// turn their recursion into a loop, and they need a longer list to overflow there.
const LONG_LIST: usize = 50_000;

/// Runs `f` on a thread with a 2 MiB stack.
fn on_small_stack(f: impl FnOnce() + Send + 'static) {
    std::thread::Builder::new()
        .stack_size(2 * 1024 * 1024)
        .spawn(f)
        .unwrap()
        .join()
        .unwrap();
}

/// Generates code for `lir` with and without optimization, with each set of options, and returns
/// the serialized output of the unoptimized code generated without static pairs.
fn compile_all_ways(arena: &mut Arena<Lir>, lir: LirId) -> Vec<u8> {
    let optimized = optimize(arena, lir);

    let mut unoptimized_output = None;

    for root in [lir, optimized] {
        for optimize_static_pairs in [false, true] {
            let mut allocator = Allocator::new();
            let program = codegen(
                arena,
                &mut allocator,
                root,
                CodegenOptions {
                    optimize_static_pairs,
                },
            )
            .unwrap();
            let bytes = node_to_bytes(&allocator, program).unwrap();
            unoptimized_output.get_or_insert(bytes);
        }
    }

    unoptimized_output.unwrap()
}

fn nil(arena: &mut Arena<Lir>) -> LirId {
    arena.alloc(Lir::Atom(vec![]))
}

fn path(arena: &mut Arena<Lir>, path: u8) -> LirId {
    arena.alloc(Lir::Path(path.into()))
}

#[test]
fn deeply_nested_operator() {
    on_small_stack(|| {
        let mut arena = Arena::new();
        let mut lir = path(&mut arena, 1);
        for _ in 0..DEPTH {
            lir = arena.alloc(Lir::Not(lir));
        }

        // `(not (not ... (not 1)))`
        let mut expected = Vec::new();
        for _ in 0..DEPTH {
            expected.extend([0xff, 0x20, 0xff]);
        }
        expected.push(0x01);
        expected.extend(std::iter::repeat_n(0x80, DEPTH));

        assert_eq!(compile_all_ways(&mut arena, lir), expected);
    });
}

#[test]
fn long_dynamic_list() {
    on_small_stack(|| {
        let mut arena = Arena::new();
        let item = path(&mut arena, 2);
        let mut lir = nil(&mut arena);
        for _ in 0..DEPTH {
            lir = arena.alloc(Lir::Cons(item, lir));
        }

        // `(c 2 (c 2 ... (c 2 ())))`
        let mut expected = Vec::new();
        for _ in 0..DEPTH {
            expected.extend([0xff, 0x04, 0xff, 0x02, 0xff]);
        }
        expected.push(0x80);
        expected.extend(std::iter::repeat_n(0x80, DEPTH));

        assert_eq!(compile_all_ways(&mut arena, lir), expected);
    });
}

#[test]
fn long_static_list() {
    on_small_stack(|| {
        let mut arena = Arena::new();
        let item = arena.alloc(Lir::Atom(vec![0x61]));
        let mut lir = nil(&mut arena);
        for _ in 0..LONG_LIST {
            lir = arena.alloc(Lir::Cons(item, lir));
        }

        // As a condition, the list is checked for purity all the way down
        let one = arena.alloc(Lir::Atom(vec![1]));
        let condition = arena.alloc(Lir::If(lir, one, lir, false));

        compile_all_ways(&mut arena, lir);
        compile_all_ways(&mut arena, condition);
    });
}

#[test]
fn quoted_program_reading_the_environment_deep_inside() {
    on_small_stack(|| {
        let mut arena = Arena::new();
        let mut body = path(&mut arena, 2);
        for _ in 0..DEPTH {
            body = arena.alloc(Lir::Strlen(body));
        }

        // Running a quoted program with a nil environment is checked for whether the program
        // reads the environment, which here is only at the very end
        let program = arena.alloc(Lir::Quote(body));
        let env = nil(&mut arena);
        let lir = arena.alloc(Lir::Run(program, env));

        compile_all_ways(&mut arena, lir);
    });
}

#[test]
fn deeply_nested_condition() {
    on_small_stack(|| {
        let mut arena = Arena::new();
        let mut condition = path(&mut arena, 2);
        for _ in 0..DEPTH {
            condition = arena.alloc(Lir::Listp(condition, false));
        }

        // The truthiness of a list check is that of the list, all the way down
        let one = arena.alloc(Lir::Atom(vec![1]));
        let nil = nil(&mut arena);
        let lir = arena.alloc(Lir::If(condition, one, nil, false));

        compile_all_ways(&mut arena, lir);
    });
}

#[test]
fn deeply_nested_boolean() {
    on_small_stack(|| {
        let mut arena = Arena::new();
        let condition = path(&mut arena, 2);
        let one = arena.alloc(Lir::Atom(vec![1]));
        let mut lir = arena.alloc(Lir::Not(condition));
        for _ in 0..DEPTH {
            lir = arena.alloc(Lir::If(condition, lir, one, false));
        }

        // A double negation is only removed if every branch is a boolean, all the way down
        let lir = arena.alloc(Lir::Not(lir));
        let lir = arena.alloc(Lir::Not(lir));

        compile_all_ways(&mut arena, lir);
    });
}
