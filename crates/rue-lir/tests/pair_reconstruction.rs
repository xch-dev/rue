use clvmr::{Allocator, ChiaDialect, MEMPOOL_MODE, run_program, serde::node_to_bytes};
use id_arena::Arena;
use rue_lir::{CodegenOptions, Lir, codegen, first_path, optimize, rest_path};

/// Builds `(c (f X) (r X))` where `X` is the given path, runs it against the environment,
/// and returns the serialized output, or `None` if the program failed.
fn reconstruct(parent: u8, projections: bool, optimized: bool, env: &str) -> Option<Vec<u8>> {
    let mut arena = Arena::new();
    let path = parent.into();

    let (first, rest) = if projections {
        let value = arena.alloc(Lir::Path(path));
        (
            arena.alloc(Lir::First(value)),
            arena.alloc(Lir::Rest(value)),
        )
    } else {
        (
            arena.alloc(Lir::Path(first_path(&path))),
            arena.alloc(Lir::Path(rest_path(&path))),
        )
    };

    let mut root = arena.alloc(Lir::Cons(first, rest));

    if optimized {
        root = optimize(&mut arena, root);
    }

    let mut allocator = Allocator::new();
    let program = codegen(
        &arena,
        &mut allocator,
        root,
        CodegenOptions {
            optimize_static_pairs: false,
        },
    )
    .unwrap();
    let env = rue_clvm::assemble(&mut allocator, env).unwrap();

    run_program(
        &mut allocator,
        &ChiaDialect::new(MEMPOOL_MODE),
        program,
        env,
        1_000_000,
    )
    .ok()
    .map(|reduction| node_to_bytes(&allocator, reduction.1).unwrap())
}

fn environment(parent: u8, selected: &str) -> String {
    match parent {
        1 => selected.to_string(),
        2 => format!("({selected} . 7)"),
        3 => format!("(7 . {selected})"),
        _ => unreachable!(),
    }
}

#[test]
fn test_pair_reconstruction_rejects_atoms() {
    for parent in [1, 2, 3] {
        for projections in [false, true] {
            for selected in ["()", "1", "0x00", "0x0001", "0xff"] {
                let env = environment(parent, selected);

                for optimized in [false, true] {
                    assert!(
                        reconstruct(parent, projections, optimized, &env).is_none(),
                        "parent={parent}, projections={projections}, optimized={optimized}, env={env} accepted an atom",
                    );
                }
            }
        }
    }
}

#[test]
fn test_pair_reconstruction_preserves_pairs() {
    for parent in [1, 2, 3] {
        for projections in [false, true] {
            for selected in [
                "(1 . 2)",
                "(0x0001 . 0x00)",
                "((1 . 2) . (3 . 4))",
                "(() . ())",
            ] {
                let env = environment(parent, selected);

                let mut allocator = Allocator::new();
                let expected = rue_clvm::assemble(&mut allocator, selected).unwrap();
                let expected = node_to_bytes(&allocator, expected).unwrap();

                for optimized in [false, true] {
                    assert_eq!(
                        reconstruct(parent, projections, optimized, &env),
                        Some(expected.clone()),
                        "parent={parent}, projections={projections}, optimized={optimized}, env={env}",
                    );
                }
            }
        }
    }
}
