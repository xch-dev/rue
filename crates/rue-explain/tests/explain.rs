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

/// Solutions for the ports of Chia's puzzles, which can't have `// solution:` lines of their own.
fn port_solutions(path: &Path) -> &'static [&'static str] {
    let key = "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
    let _ = key;
    match path.file_stem().and_then(|stem| stem.to_str()) {
        // The delegated puzzle is run with its solution, and its output is signed by the key
        Some("p2_delegated_puzzle_or_hidden_puzzle") => &[
            "(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa () (q (51 0xcafe 1)) ())",
            "(0xaaaa () (q . 2) ((60 1)))",
        ],
        Some("p2_conditions") => &["(((51 0xcafe 1)))", "(())"],
        Some("p2_singleton") => &[
            "(0x1111111111111111111111111111111111111111111111111111111111111111 0x2222222222222222222222222222222222222222222222222222222222222222 0x3333333333333333333333333333333333333333333333333333333333333333 0x4444444444444444444444444444444444444444444444444444444444444444 0x5555555555555555555555555555555555555555555555555555555555555555)",
        ],
        _ => &[],
    }
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
        .chain(port_solutions(path).iter().copied())
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
            (Err(expected), Err(EvalFailure::Clvm(actual))) => assert_eq!(
                actual.to_string(),
                expected.to_string(),
                "{} with {solution}",
                path.display()
            ),
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
                0,
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
