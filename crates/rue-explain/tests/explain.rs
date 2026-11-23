//! Snapshots of the pseudocode for the programs in `tests/explain` and some of the ports of
//! Chia's puzzles (`UPDATE_EXPECT=1` updates them), along with checks that the pseudocode means
//! what the program does: every node of the program is part of it, and running it with each
//! solution (the `// solution:` lines at the top of a program) gives the same result.

use std::{
    fmt::Write,
    fs,
    path::{Path, PathBuf},
};

use clvmr::{Allocator, run_program, serde::node_to_bytes};
use expect_test::expect_file;
use rue_clvm::{assemble, disassemble_value};
use rue_compiler::{Compiler, FileTree, clvm_target, normalize_path};
use rue_explain::{EvalFailure, Explanation};
use rue_lir::DebugDialect;
use rue_options::CompilerOptions;

const MAX_COST: u64 = 11_000_000_000;

fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../..")
}

/// The programs that are snapshotted, relative to the root of the repository.
fn cases() -> Vec<PathBuf> {
    let mut cases = fs::read_dir(Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/explain"))
        .unwrap()
        .map(|entry| entry.unwrap().path())
        .filter(|path| path.extension().is_some_and(|extension| extension == "rue"))
        .collect::<Vec<_>>();

    for case in [
        "tests/chia_puzzles/p2_delegated_puzzle_or_hidden_puzzle.rue",
        "tests/chia_puzzles/p2_conditions.rue",
        "tests/chia_puzzles/p2_singleton.rue",
        "tests/chia_puzzles/cat_puzzles/cat_v2.rue",
    ] {
        cases.push(root().join(case));
    }

    for dir in ["crates/rue-compiler/tests/debug_metadata"] {
        for entry in fs::read_dir(root().join(dir)).unwrap() {
            let path = entry.unwrap().path();
            if path.extension().is_some_and(|extension| extension == "rue") {
                cases.push(path);
            }
        }
    }

    cases.sort();
    cases
}

/// Compiles `main` with its sidecar, and explains it.
fn explain(path: &Path, options: CompilerOptions) -> (Allocator, clvmr::NodePtr, Explanation) {
    let base_path = path.parent().unwrap().canonicalize().unwrap();
    let kind = normalize_path(path).unwrap();
    let mut ctx = Compiler::new(options);
    let tree = FileTree::compile_file(&mut ctx, path).unwrap();
    let mut allocator = Allocator::new();
    let (program, metadata) = tree
        .main_with_debug_metadata(&mut ctx, &mut allocator, &kind, base_path)
        .unwrap()
        .unwrap();
    let bytes = node_to_bytes(&allocator, program).unwrap();
    let explanation = Explanation::new(&bytes, Some(&metadata)).unwrap();
    (allocator, program, explanation)
}

/// Checks that running the explanation gives the same result as running the program.
fn check_solutions(
    path: &Path,
    options: CompilerOptions,
    allocator: &mut Allocator,
    program: clvmr::NodePtr,
    explanation: &Explanation,
) {
    let source = fs::read_to_string(path).unwrap();
    let dialect = DebugDialect::new(clvm_target(&options).flags(), false);

    for solution in source
        .lines()
        .filter_map(|line| line.trim().strip_prefix("// solution: "))
    {
        let env = assemble(allocator, solution).unwrap();
        let expected = run_program(allocator, &dialect, program, env, MAX_COST);
        let actual = explanation.evaluate(allocator, &dialect, env, MAX_COST);

        match (expected, actual) {
            (Ok(expected), Ok(actual)) => assert_eq!(
                disassemble_value(allocator, actual),
                disassemble_value(allocator, expected.1),
                "{} with {solution}",
                path.display()
            ),
            (Err(_), Err(EvalFailure::Clvm(_))) | (_, Err(EvalFailure::OutOfCost)) => {}
            (expected, actual) => panic!(
                "{} with {solution}: expected {expected:?}, got {actual:?}",
                path.display()
            ),
        }
    }
}

#[test]
fn explanations() {
    for path in cases() {
        let mut snapshot = String::new();

        for (build, options) in [
            ("release", CompilerOptions::default()),
            ("debug", CompilerOptions::debug()),
        ] {
            let (mut allocator, program, explanation) = explain(&path, options);

            assert_eq!(
                explanation.uncovered(),
                Vec::<usize>::new(),
                "{} ({build}) has nodes that aren't shown",
                path.display()
            );

            check_solutions(&path, options, &mut allocator, program, &explanation);

            writeln!(snapshot, "===== {build} =====").unwrap();
            snapshot.push_str(&explanation.render());
        }

        let name = path.file_stem().unwrap().to_string_lossy().to_string();
        expect_file![
            Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("tests/explain")
                .join(format!("{name}.txt"))
        ]
        .assert_eq(&snapshot);
    }
}
