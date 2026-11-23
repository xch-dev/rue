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
use rue_clvm::assemble;
use rue_compiler::{Compiler, FileTree, clvm_target, normalize_path};
use rue_explain::Explanation;
use rue_lir::DebugDialect;
use rue_options::CompilerOptions;

const MAX_COST: u64 = 11_000_000_000;

/// The programs that fail with every solution: by design, or (for `cat_v2`, whose paths
/// rue-tests checks through its tests) because a solution that succeeds is hard to write.
const ALWAYS_FAIL: &[&str] = &["recursion", "recursion_tabs", "const_main_value", "cat_v2"];

fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../..")
}

/// The programs that are snapshotted, relative to the root of the repository.
fn cases() -> Vec<PathBuf> {
    let mut cases = rue_files(&Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/explain"));
    cases.extend(rue_files(
        &root().join("crates/rue-compiler/tests/debug_metadata"),
    ));

    for case in [
        "tests/chia_puzzles/p2_delegated_puzzle_or_hidden_puzzle.rue",
        "tests/chia_puzzles/p2_conditions.rue",
        "tests/chia_puzzles/p2_singleton.rue",
        "tests/chia_puzzles/cat_puzzles/cat_v2.rue",
    ] {
        cases.push(root().join(case));
    }

    cases.sort();
    cases
}

fn rue_files(dir: &Path) -> Vec<PathBuf> {
    fs::read_dir(dir)
        .unwrap()
        .map(|entry| entry.unwrap().path())
        .filter(|path| path.extension().is_some_and(|extension| extension == "rue"))
        .collect()
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

/// More solutions, for programs whose `// solution:` lines (which `rue-cli`'s trace tests share)
/// all fail, and for the ports of Chia's puzzles, which can't have lines of their own.
fn extra_solutions(path: &Path) -> &'static [&'static str] {
    match path.file_stem().and_then(|stem| stem.to_str()) {
        Some("assert_message") => &["(20 10)"],
        Some("closure_capture") => &["(100 (1 2 30 4))", "(100 ())"],
        Some("closure_no_params") => &["(9)"],
        Some("const_main_wrapper") => &["(5 2)"],
        Some("duplicate_bodies" | "duplicate_subexpr") => &["(2 5)"],
        Some("inline_fail") => &["(15)"],
        Some("nested_assert") => &["(100)"],
        Some("typed_param") => &[
            "(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa 0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb 100 \"hi\" 1)",
        ],
        Some("for_lists") => &["((1 2 3) (10 20 30))", "(() ())"],
        Some("struct_param") => &[
            "((0x1111111111111111111111111111111111111111111111111111111111111111 100) (3 . 5))",
            "((0x1111111111111111111111111111111111111111111111111111111111111111 100) (5 . 3))",
        ],
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

/// Checks that running the explanation gives the same result as running the program, with each
/// solution. Returns how many of them succeed.
fn check_solutions(
    path: &Path,
    options: CompilerOptions,
    allocator: &mut Allocator,
    program: clvmr::NodePtr,
    explanation: &Explanation,
) -> usize {
    let source = fs::read_to_string(path).unwrap();
    let dialect = DebugDialect::new(clvm_target(&options).flags(), false);
    let mut succeeded = 0;

    for solution in source
        .lines()
        .filter_map(|line| line.trim().strip_prefix("// solution: "))
        .chain(extra_solutions(path).iter().copied())
    {
        let env = assemble(allocator, solution).unwrap();
        let expected = run_program(allocator, &dialect, program, env, MAX_COST);
        if let Err(error) = explanation.check(allocator, &dialect, &expected, env, MAX_COST) {
            panic!("{} with {solution}: {error}", path.display());
        }
        succeeded += usize::from(expected.is_ok());
    }

    succeeded
}

/// Checks that each line of code (other than its source location) is wrapped to 100 columns,
/// unless it's a comment or has a word that's too long to wrap.
fn check_widths(path: &Path, rendered: &str) {
    for line in rendered.lines() {
        let code = match line.find("  // ") {
            Some(index) if line[index..].contains(".rue:") => &line[..index],
            _ => line,
        };
        let unbreakable = code.split(' ').any(|word| word.len() > 40);
        assert!(
            code.trim_start().starts_with("//") || unbreakable || code.trim_end().len() <= 100,
            "{} has a line that isn't wrapped: {line}",
            path.display()
        );
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

            let succeeded = check_solutions(&path, options, &mut allocator, program, &explanation);

            // Each program is checked on a path that doesn't fail, other than the ones that
            // always do
            if path
                .file_stem()
                .is_some_and(|stem| !ALWAYS_FAIL.iter().any(|name| stem == *name))
            {
                assert!(
                    succeeded > 0,
                    "{} ({build}) has no solution that succeeds",
                    path.display()
                );
            }

            let rendered = explanation.render();
            check_widths(&path, &rendered);
            writeln!(snapshot, "===== {build} =====").unwrap();
            snapshot.push_str(&rendered);
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
