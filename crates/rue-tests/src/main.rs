use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::env;
use std::fmt::Write;
use std::fs;
use std::io::Write as _;
use std::num::NonZeroUsize;
use std::path::{Path, PathBuf};
use std::process;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::thread;

use anyhow::{Context, Result};
use clvm_utils::tree_hash;
use clvmr::error::EvalErr;
use clvmr::{Allocator, ChiaDialect, NodePtr, run_program, serde::node_to_bytes};
use indexmap::IndexMap;
use rue_clvm::{assemble, disassemble_program, disassemble_value};
use rue_compiler::normalize_path;
use rue_compiler::{CompiledTest, Compiler, Error, FileTree};
use rue_diagnostic::{Diagnostic, DiagnosticSeverity, SourceKind};
use rue_explain::Explanation;
use rue_formatter::{FormatError, FormatOptions, format_source};
use rue_lir::{COST_PER_BYTE, ClvmTarget, DebugDialect};
use rue_options::CompilerOptions;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};

const USAGE: &str = "\
Usage: rue-tests [--bless] [FILTER]

Checks the .yaml snapshots under tests/ and examples/, std_tests.yaml, and the formatting of
the .rue files in tests/, examples/ and crates/rue-compiler/src/std/. Reports each difference
and exits 1 if there are any, without changing files.

Arguments:
  [FILTER]  Only run the tests whose name contains FILTER, and only check the formatting of
            files whose path contains it. std_tests.yaml is always checked

Options:
  --bless     Rewrite the snapshots and reformat the .rue files to match the current output
  -h, --help  Print this message";

#[derive(Debug, Default, Clone, PartialEq, Eq)]
struct Args {
    bless: bool,
    filter: Option<String>,
    help: bool,
}

fn parse_args(args: impl IntoIterator<Item = String>) -> Result<Args, String> {
    let mut parsed = Args::default();

    for arg in args {
        match arg.as_str() {
            "--bless" => parsed.bless = true,
            "-h" | "--help" => parsed.help = true,
            _ if arg.starts_with('-') => return Err(format!("unknown option `{arg}`")),
            _ if parsed.filter.is_some() => {
                return Err(format!(
                    "unexpected argument `{arg}`, only one filter is allowed"
                ));
            }
            _ => parsed.filter = Some(arg),
        }
    }

    Ok(parsed)
}

fn matches_filter(filter: Option<&str>, name: &str) -> bool {
    filter.is_none_or(|filter| name.contains(filter))
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(default)]
struct TestFile {
    /// Whether the file is compiled with `operand_limits`, and run with `LIMITS`. Like a
    /// `solution`, this is set by hand in the snapshot.
    #[serde(skip_serializing_if = "std::ops::Not::not")]
    operand_limits: bool,

    /// Whether the file is compiled with `hard_fork_2`, and run as it would be after hard fork 2.
    /// Like a `solution`, this is set by hand in the snapshot.
    #[serde(skip_serializing_if = "std::ops::Not::not")]
    hard_fork_2: bool,

    #[serde(flatten)]
    main: TestCase,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    tests: Vec<TestCase>,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    diagnostics: Vec<String>,

    #[serde(skip_serializing_if = "BTreeMap::is_empty")]
    puzzle_hashes: BTreeMap<String, String>,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(default)]
struct TestCase {
    #[serde(skip_serializing_if = "Option::is_none")]
    name: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    program: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    debug_program: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    solution: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    output: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    debug_output: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    runtime_cost: Option<u64>,

    #[serde(skip_serializing_if = "Option::is_none")]
    byte_cost: Option<u64>,

    #[serde(skip_serializing_if = "Option::is_none")]
    total_cost: Option<u64>,

    #[serde(skip_serializing_if = "Option::is_none")]
    clvm_error: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    debug_clvm_error: Option<String>,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    print_messages: Vec<String>,
}

fn main() -> Result<()> {
    env_logger::init();

    let args = match parse_args(env::args().skip(1)) {
        Ok(args) => args,
        Err(error) => {
            eprintln!("error: {error}\n\n{USAGE}");
            process::exit(2);
        }
    };

    if args.help {
        println!("{USAGE}");
        return Ok(());
    }

    let failed = run_tests(args.filter.as_deref(), Path::new("."), args.bless)?;

    if failed {
        if args.bless {
            eprintln!(
                "\nUpdated the files that differed. Run again without --bless to check them; \
                 anything that still fails, such as a syntax error no snapshot expects, \
                 needs fixing by hand"
            );
        } else {
            eprintln!(
                "\nSome checks failed. If the changes are intended, update the files with \
                 `cargo run -p rue-tests --release -- --bless`"
            );
        }
        process::exit(1);
    }

    Ok(())
}

fn run_tests(filter_arg: Option<&str>, base_path: &Path, update: bool) -> Result<bool> {
    let mut failed = false;

    let unparsed = check_formatting_corpus(base_path, filter_arg, update, &mut failed)?;
    let mut jobs = Vec::new();
    collect_test_files(&base_path.join("tests"), filter_arg, &mut jobs)?;
    collect_test_files(&base_path.join("examples"), filter_arg, &mut jobs)?;
    run_test_files(&jobs, update, &mut failed)?;
    check_expected_parse_errors(&unparsed, filter_arg, &mut failed)?;

    println!("Running std tests");

    let yaml_file = base_path.join("std_tests.yaml");

    let mut file: TestFile = if yaml_file.try_exists()? {
        read_snapshot(&yaml_file)?
    } else {
        TestFile::default()
    };

    let mut allocator = Allocator::new();

    let (mut ctx, mut debug_ctx) = compilers(&file)?;
    let target = ctx.target();

    let unit = FileTree::empty(&mut ctx)?;
    let debug_unit = FileTree::empty(&mut debug_ctx)?;

    file.diagnostics = Vec::new();

    let mut codegen = true;

    for diagnostic in ctx.take_diagnostics() {
        file.diagnostics.push(diagnostic.full_message(base_path));

        if diagnostic.severity == DiagnosticSeverity::Error {
            codegen = false;
        }
    }

    let tests = codegen
        .then(|| unit.tests(&mut ctx, &mut allocator, None, None, base_path, true))
        .transpose()?
        .unwrap_or_default();
    let debug_tests = codegen
        .then(|| debug_unit.tests(&mut debug_ctx, &mut allocator, None, None, base_path, true))
        .transpose()?
        .unwrap_or_default();

    let sidecars = test_sidecars(&mut ctx, &mut allocator, &tests, base_path)?;
    let debug_sidecars = test_sidecars(&mut debug_ctx, &mut allocator, &debug_tests, base_path)?;

    handle_tests(
        &mut allocator,
        target,
        &mut file.tests,
        &tests,
        &debug_tests,
        [&sidecars, &debug_sidecars],
    )?;

    let mut log = String::new();
    compare_snapshot(&yaml_file, &file, update, &mut failed, &mut log)?;
    eprint!("{log}");

    Ok(failed)
}

/// Checks that each `.rue` file is formatted, and with `update`, reformats it. Returns the files
/// the formatter couldn't parse, with the parser's diagnostics, for
/// [`check_expected_parse_errors`] to check against their snapshots.
fn check_formatting_corpus(
    base_path: &Path,
    filter_arg: Option<&str>,
    update: bool,
    failed: &mut bool,
) -> Result<Vec<(PathBuf, Vec<Diagnostic>)>> {
    println!("Checking Rue formatting");
    let mut files = Vec::new();
    collect_rue_files(&base_path.join("tests"), &mut files)?;
    collect_rue_files(&base_path.join("examples"), &mut files)?;
    collect_rue_files(&base_path.join("crates/rue-compiler/src/std"), &mut files)?;
    files.sort();

    let mut unparsed = Vec::new();

    for path in files {
        let display = path.strip_prefix(base_path).unwrap_or(&path);
        if !matches_filter(filter_arg, &display.to_string_lossy()) {
            continue;
        }

        let original = fs::read_to_string(&path)?;
        let formatted = match format_source(&original, &FormatOptions::default()) {
            Ok(formatted) => formatted,
            Err(FormatError::Parse { diagnostics }) => {
                unparsed.push((path, diagnostics));
                continue;
            }
            Err(error) => {
                eprintln!("Formatter failed for {}: {error}", display.display());
                *failed = true;
                continue;
            }
        };

        if formatted == original {
            continue;
        }

        *failed = true;
        if update {
            fs::write(&path, formatted)?;
            eprintln!("Formatting differed, updated {}", display.display());
        } else {
            eprintln!("Formatting differs for {}", display.display());
        }
    }

    Ok(unparsed)
}

/// The formatter can't format a file with syntax errors, which some tests contain on purpose.
/// A file is only skipped if it has a snapshot next to it (`<name>.yaml`) that records each of
/// the syntax errors, so a file that stops parsing by mistake is still reported.
///
/// Errors are matched by message and file, not position: the formatter adds a final newline
/// before parsing, so an error at the end of a file without one is reported a line later than
/// the compiler reports it.
///
/// The formatting pass filters by path but tests are filtered by name, so a file whose test
/// didn't run under the filter isn't checked, since its snapshot may not have been written yet.
fn check_expected_parse_errors(
    unparsed: &[(PathBuf, Vec<Diagnostic>)],
    filter_arg: Option<&str>,
    failed: &mut bool,
) -> Result<()> {
    for (path, diagnostics) in unparsed {
        let display = short_path(path).display();
        let yaml_file = path.with_extension("yaml");
        if !matches_filter(filter_arg, &test_name(path)?) {
            println!("Skipped formatting {display}, since its test doesn't match the filter");
            continue;
        }

        let expected = if yaml_file.try_exists()? {
            read_snapshot(&yaml_file)?.diagnostics
        } else {
            Vec::new()
        };

        let file_name = path.file_name().unwrap_or_default().to_string_lossy();
        let unexpected: Vec<String> = diagnostics
            .iter()
            .filter_map(|diagnostic| {
                let prefix = format!("{} at {file_name}:", diagnostic.kind);
                (!expected.iter().any(|message| message.starts_with(&prefix)))
                    .then(|| format!("{prefix}{}", diagnostic.start()))
            })
            .collect();

        if unexpected.is_empty() {
            println!("Skipped formatting {display}, since its snapshot expects its syntax errors");
            continue;
        }

        *failed = true;
        eprintln!(
            "Formatter could not parse {display}, and these syntax errors aren't in {}:",
            short_path(&yaml_file).display()
        );
        for message in unexpected {
            eprintln!("  {message}");
        }
    }

    Ok(())
}

/// The name `collect_test_files` filters the test that compiles a `.rue` file by: the name of the nearest
/// directory test (a `<name>/` directory next to `<name>.yaml` and no `<name>.rue`) that contains
/// the file, or else the file's own name.
fn test_name(path: &Path) -> Result<String> {
    for directory in path.ancestors().skip(1) {
        if directory.with_extension("yaml").try_exists()?
            && !directory.with_extension("rue").try_exists()?
            && let Some(name) = directory.file_name()
        {
            return Ok(name.to_string_lossy().to_string());
        }
    }
    Ok(path
        .file_stem()
        .unwrap_or_default()
        .to_string_lossy()
        .to_string())
}

fn collect_rue_files(path: &Path, files: &mut Vec<std::path::PathBuf>) -> Result<()> {
    for entry in fs::read_dir(path)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            collect_rue_files(&path, files)?;
        } else if path.extension().is_some_and(|extension| extension == "rue") {
            files.push(path);
        }
    }
    Ok(())
}

/// A snapshot test that `collect_test_files` found: its name, its `.rue` file or `.yaml`
/// snapshot, and whether it's a directory test.
#[derive(Debug, Clone)]
struct TestJob {
    name: String,
    path: PathBuf,
    is_dir: bool,
}

/// Checks the snapshot tests under `path`.
#[cfg(test)]
fn walk_dir(path: &Path, filter_arg: Option<&str>, update: bool, failed: &mut bool) -> Result<()> {
    let mut jobs = Vec::new();
    collect_test_files(path, filter_arg, &mut jobs)?;
    run_test_files(&jobs, update, failed)
}

fn collect_test_files(
    path: &Path,
    filter_arg: Option<&str>,
    jobs: &mut Vec<TestJob>,
) -> Result<()> {
    let mut directories = IndexMap::new();
    let mut directories_to_exclude = HashSet::new();

    for entry in fs::read_dir(path)? {
        let entry = entry?;
        let path = entry.path();

        if path.is_dir() {
            directories.insert(entry.file_name().to_string_lossy().to_string(), path);
            continue;
        }

        let file_name = entry.file_name();
        let file_name = file_name.to_str().unwrap();

        if let Some(name) = file_name.strip_suffix(".rue") {
            if matches_filter(filter_arg, name) && !path.with_extension("yaml").try_exists()? {
                jobs.push(TestJob {
                    name: name.to_string(),
                    path,
                    is_dir: false,
                });
            }
            continue;
        }

        let Some(name) = file_name.strip_suffix(".yaml") else {
            continue;
        };

        let mut is_dir = false;

        if !entry
            .path()
            .parent()
            .unwrap()
            .join(format!("{name}.rue"))
            .try_exists()?
        {
            directories_to_exclude.insert(name.to_string());
            is_dir = true;
        }

        if !matches_filter(filter_arg, name) {
            continue;
        }

        jobs.push(TestJob {
            name: name.to_string(),
            path: entry.path(),
            is_dir,
        });
    }

    for (name, path) in directories {
        if directories_to_exclude.contains(&name) {
            continue;
        }
        collect_test_files(&path, filter_arg, jobs)?;
    }

    Ok(())
}

/// The slowest snapshot tests, slowest first. They're started before the others, since the
/// slowest takes about as long as all of the others together on four cores, and the run would
/// otherwise wait for whichever of them started last. A test missing from this list only makes
/// the run slower.
const SLOW_TESTS: &[&str] = &[
    "nested_generic_function_cycles",
    "imprecise_narrowing",
    "prefarm_inner",
    "dao_lockup",
    "dao_proposal",
    "dao_spend_p2_singleton_v2",
];

/// Runs the snapshot tests on a thread for each core, the ones in [`SLOW_TESTS`] first. Each
/// test's output is buffered and printed in one piece when it finishes, so the reports of tests
/// that run at the same time don't interleave.
fn run_test_files(jobs: &[TestJob], update: bool, failed: &mut bool) -> Result<()> {
    let mut jobs: Vec<&TestJob> = jobs.iter().collect();
    jobs.sort_by_key(|job| {
        SLOW_TESTS
            .iter()
            .position(|name| *name == job.name)
            .unwrap_or(SLOW_TESTS.len())
    });
    let jobs = jobs.as_slice();

    let threads = thread::available_parallelism().map_or(1, NonZeroUsize::get);
    let next = AtomicUsize::new(0);
    let any_failed = AtomicBool::new(false);

    thread::scope(|scope| {
        let handles: Vec<_> = (0..threads.min(jobs.len()))
            .map(|_| {
                thread::Builder::new()
                    // Some of the stress tests nest deeply
                    .stack_size(64 * 1024 * 1024)
                    .spawn_scoped(scope, || -> Result<()> {
                        loop {
                            let index = next.fetch_add(1, Ordering::Relaxed);
                            let Some(job) = jobs.get(index) else {
                                return Ok(());
                            };
                            let mut log = String::new();
                            let mut job_failed = false;
                            let result = handle_test_file(
                                &job.name,
                                &job.path,
                                &mut job_failed,
                                update,
                                job.is_dir,
                                &mut log,
                            );
                            print_log(&log);
                            if job_failed {
                                any_failed.store(true, Ordering::Relaxed);
                            }
                            if let Err(error) = result {
                                // The other threads stop after the test they're on
                                next.store(jobs.len(), Ordering::Relaxed);
                                return Err(
                                    error.context(format!("couldn't run {}", job.path.display()))
                                );
                            }
                        }
                    })
                    .expect("couldn't spawn a test thread")
            })
            .collect();

        handles.into_iter().try_for_each(|handle| {
            handle
                .join()
                .unwrap_or_else(|panic| std::panic::resume_unwind(panic))
        })
    })?;

    if any_failed.load(Ordering::Relaxed) {
        *failed = true;
    }

    Ok(())
}

/// Prints a test's buffered output: its progress line to stdout and its failures to stderr.
fn print_log(log: &str) {
    let (progress, failures) = log.split_once('\n').unwrap_or((log, ""));
    println!("{progress}");
    if !failures.is_empty() {
        let mut stderr = std::io::stderr().lock();
        let _ = stderr.write_all(failures.as_bytes());
    }
}

fn handle_test_file(
    name: &str,
    entry: &Path,
    failed: &mut bool,
    update: bool,
    is_dir: bool,
    log: &mut String,
) -> Result<()> {
    let parent = entry.parent().unwrap();
    let yaml_file = parent.join(format!("{name}.yaml"));

    writeln!(log, "Running {name}")?;

    let mut file: TestFile = if yaml_file.try_exists()? {
        read_snapshot(&yaml_file)?
    } else {
        TestFile::default()
    };

    let mut allocator = Allocator::new();

    let (mut ctx, mut debug_ctx) = compilers(&file)?;
    let target = ctx.target();

    let (unit, debug_unit, kind, base_path) = if is_dir {
        let rue_dir = parent.join(name);
        let unit = FileTree::compile_path(&mut ctx, &rue_dir, &mut HashMap::new())?;
        let debug_unit = FileTree::compile_path(&mut debug_ctx, &rue_dir, &mut HashMap::new())?;
        let main_file = rue_dir.join("main.rue");
        let kind = main_file
            .try_exists()?
            .then(|| normalize_path(&main_file))
            .transpose()?;
        (unit, debug_unit, kind, rue_dir.canonicalize()?)
    } else {
        let rue_file = parent.join(format!("{name}.rue"));
        let unit = FileTree::compile_file(&mut ctx, &rue_file)?;
        let debug_unit = FileTree::compile_file(&mut debug_ctx, &rue_file)?;
        let kind = Some(normalize_path(&rue_file)?);
        (unit, debug_unit, kind, parent.canonicalize()?)
    };

    file.diagnostics = Vec::new();

    let mut codegen = true;

    for diagnostic in ctx.take_diagnostics() {
        file.diagnostics.push(diagnostic.full_message(&base_path));

        if diagnostic.severity == DiagnosticSeverity::Error {
            codegen = false;
        }
    }

    // Every placeholder for code that couldn't be compiled must be covered by an error, which is
    // also checked before generating code, but not every test generates code
    if codegen {
        for ctx in [&ctx, &debug_ctx] {
            assert_eq!(
                ctx.first_placeholder(),
                None,
                "a placeholder was made without an error"
            );
        }
    }

    let main = kind
        .as_ref()
        .filter(|_| codegen)
        .map(|kind| unit.main(&mut ctx, &mut allocator, kind, base_path.clone()));
    let debug_main = kind
        .as_ref()
        .filter(|_| codegen)
        .map(|kind| debug_unit.main(&mut debug_ctx, &mut allocator, kind, base_path.clone()));
    let (main, debug_main) = main_programs(main, debug_main, &mut file.diagnostics)?;

    let mut main_sidecars = [None, None];

    if let Some(kind) = &kind {
        for ((ctx, unit, program), sidecar) in [
            (&mut ctx, &unit, main),
            (&mut debug_ctx, &debug_unit, debug_main),
        ]
        .into_iter()
        .zip(&mut main_sidecars)
        {
            if let Some(program) = program {
                *sidecar = Some(check_debug_metadata(
                    ctx,
                    unit,
                    &mut allocator,
                    kind,
                    &base_path,
                    program,
                )?);
            }
        }
    }

    let tests = codegen
        .then(|| unit.tests(&mut ctx, &mut allocator, None, None, &base_path, false))
        .transpose()?
        .unwrap_or_default();
    let debug_tests = codegen
        .then(|| {
            debug_unit.tests(
                &mut debug_ctx,
                &mut allocator,
                None,
                None,
                &base_path,
                false,
            )
        })
        .transpose()?
        .unwrap_or_default();

    handle_test_case(
        &mut allocator,
        target,
        &mut file.main,
        main,
        debug_main,
        [main_sidecars[0].as_deref(), main_sidecars[1].as_deref()],
    )?;

    file.puzzle_hashes = BTreeMap::new();

    if is_dir && codegen {
        for source in unit.all_files() {
            let SourceKind::File(path) = &source.source.kind else {
                continue;
            };

            if kind.as_ref() == Some(&source.source.kind) {
                continue;
            }

            let Some(ptr) = unit.main(
                &mut ctx,
                &mut allocator,
                &source.source.kind,
                base_path.clone(),
            )?
            else {
                continue;
            };

            let name = Path::new(path)
                .strip_prefix(&base_path)
                .unwrap_or(Path::new(path))
                .to_string_lossy()
                .to_string();

            file.puzzle_hashes
                .insert(name, tree_hash(&allocator, ptr).to_string());
        }
    }

    let sidecars = test_sidecars(&mut ctx, &mut allocator, &tests, &base_path)?;
    let debug_sidecars = test_sidecars(&mut debug_ctx, &mut allocator, &debug_tests, &base_path)?;

    handle_tests(
        &mut allocator,
        target,
        &mut file.tests,
        &tests,
        &debug_tests,
        [&sidecars, &debug_sidecars],
    )?;

    compare_snapshot(&yaml_file, &file, update, failed, log)?;

    Ok(())
}

/// Checks that compiling `main` along with a debug metadata sidecar doesn't change the program.
fn check_debug_metadata(
    ctx: &mut Compiler,
    unit: &FileTree,
    allocator: &mut Allocator,
    kind: &SourceKind,
    base_path: &Path,
    program: NodePtr,
) -> Result<Vec<u8>> {
    let (with_metadata, sidecar) = unit
        .main_with_debug_metadata(ctx, allocator, kind, base_path.to_path_buf())?
        .context("main has a program, so it has one with a sidecar")?;

    anyhow::ensure!(
        node_to_bytes(allocator, with_metadata)? == node_to_bytes(allocator, program)?,
        "compiling main with a debug metadata sidecar changed the program"
    );

    Ok(sidecar)
}

/// The programs for `main` and its debug build. Generated code that's nested too deeply is an
/// error that's only found once code is generated, so it's recorded with the diagnostics (the
/// debug build's too, if its message differs), and then neither build has a program.
fn main_programs(
    main: Option<Result<Option<NodePtr>, Error>>,
    debug_main: Option<Result<Option<NodePtr>, Error>>,
    diagnostics: &mut Vec<String>,
) -> Result<(Option<NodePtr>, Option<NodePtr>)> {
    let mut programs = [None, None];
    let mut too_deep = false;

    for (program, result) in programs.iter_mut().zip([main, debug_main]) {
        match result.transpose() {
            Ok(ptr) => *program = ptr.flatten(),
            Err(Error::LoweringTooDeep(message)) => {
                too_deep = true;

                if !diagnostics.contains(&message) {
                    diagnostics.push(message);
                }
            }
            Err(error) => return Err(error.into()),
        }
    }

    let [main, debug_main] = if too_deep { [None, None] } else { programs };

    Ok((main, debug_main))
}

/// The release and debug compilers for a snapshot, with the target it sets by hand.
fn compilers(file: &TestFile) -> Result<(Compiler, Compiler)> {
    let target = |options: CompilerOptions| CompilerOptions {
        operand_limits: file.operand_limits,
        hard_fork_2: file.hard_fork_2,
        ..options
    };
    let options = target(CompilerOptions::default());
    options.check_target()?;

    Ok((
        Compiler::new(options),
        Compiler::new(target(CompilerOptions::debug())),
    ))
}

/// Runs each test and its debug build, and records the results in the snapshot's test cases.
fn handle_tests(
    allocator: &mut Allocator,
    target: ClvmTarget,
    test_cases: &mut Vec<TestCase>,
    tests: &[CompiledTest],
    debug_tests: &[CompiledTest],
    sidecars: [&[Vec<u8>]; 2],
) -> Result<()> {
    assert_eq!(debug_tests.len(), tests.len());

    *test_cases = match_test_cases(
        std::mem::take(test_cases),
        tests.iter().map(|test| test.name.clone()),
    );

    for (index, ((test_case, test), debug_test)) in test_cases
        .iter_mut()
        .zip(tests)
        .zip(debug_tests)
        .enumerate()
    {
        handle_test_case(
            allocator,
            target,
            test_case,
            Some(test.ptr),
            Some(debug_test.ptr),
            [
                Some(sidecars[0][index].as_slice()),
                Some(sidecars[1][index].as_slice()),
            ],
        )?;
    }

    Ok(())
}

/// The debug metadata sidecar of each test, which is checked to be for the same program.
fn test_sidecars(
    ctx: &mut Compiler,
    allocator: &mut Allocator,
    tests: &[CompiledTest],
    base_path: &Path,
) -> Result<Vec<Vec<u8>>> {
    tests
        .iter()
        .map(|test| {
            let (ptr, sidecar) = test.with_debug_metadata(ctx, allocator, base_path)?;
            anyhow::ensure!(
                node_to_bytes(allocator, ptr)? == node_to_bytes(allocator, test.ptr)?,
                "compiling a test with a debug metadata sidecar changed the program"
            );
            Ok(sidecar)
        })
        .collect()
}

/// Checks that the program's explanation (see `rue_explain`) shows every node of the program and
/// means what the program does: running it with the solution gives the same result.
fn check_explanation<D: clvmr::dialect::Dialect>(
    allocator: &mut Allocator,
    dialect: &D,
    program: NodePtr,
    sidecar: &[u8],
    env: NodePtr,
) -> Result<()> {
    let explanation = Explanation::new(&node_to_bytes(allocator, program)?, Some(sidecar))?;

    let uncovered = explanation.uncovered();
    anyhow::ensure!(
        uncovered == 0,
        "the explanation of {} leaves out {uncovered} of its nodes",
        disassemble_program(allocator, program),
    );

    explanation
        .check(allocator, dialect, program, env, 100_000_000)
        .map_err(|error| {
            anyhow::anyhow!(
                "{error}, for {}:\n{}",
                disassemble_program(allocator, program),
                explanation.render()
            )
        })
}

/// Lines up the test cases of the old snapshot with the compiled tests, by name, so that a
/// `solution` set by hand stays with its test when tests are added, removed or reordered. A test
/// that isn't in the old snapshot gets a default test case.
fn match_test_cases(
    mut old: Vec<TestCase>,
    names: impl IntoIterator<Item = Option<String>>,
) -> Vec<TestCase> {
    names
        .into_iter()
        .map(|name| {
            old.iter()
                .position(|test_case| test_case.name == name)
                .map_or_else(
                    || TestCase {
                        name,
                        ..TestCase::default()
                    },
                    |index| old.remove(index),
                )
        })
        .collect()
}

/// Serializes `value` as snapshot YAML, with each single-line string on one line rather than
/// folded at 80 columns, so long CLVM programs stay greppable and diffs stay line-sized.
fn to_yaml<T: Serialize>(value: &T) -> Result<String> {
    let options = serde_saphyr::ser_options! {
        folded_wrap_chars: usize::MAX,
    };
    Ok(serde_saphyr::to_string_with_options(value, options)?)
}

fn read_snapshot(yaml_file: &Path) -> Result<TestFile> {
    serde_saphyr::from_str(&fs::read_to_string(yaml_file)?)
        .with_context(|| format!("couldn't parse {}", yaml_file.display()))
}

/// Reports how `actual` differs from the snapshot in `yaml_file`, and with `update`, writes
/// `actual` to it. Anything but the exact text `--bless` would write fails, including a missing
/// snapshot or a key the harness doesn't know, so a passing check means `--bless` changes
/// nothing.
fn compare_snapshot(
    yaml_file: &Path,
    actual: &TestFile,
    update: bool,
    failed: &mut bool,
    log: &mut String,
) -> Result<()> {
    let display = short_path(yaml_file).display();
    let text = to_yaml(actual)?;
    let existing = yaml_file
        .try_exists()?
        .then(|| fs::read_to_string(yaml_file))
        .transpose()?;

    if existing.as_deref() == Some(text.as_str()) {
        return Ok(());
    }

    *failed = true;

    let expected = match &existing {
        Some(existing) => serde_saphyr::from_str(existing)?,
        None => Value::Object(Map::new()),
    };
    let mut differences = Vec::new();
    diff_values(
        "",
        Some(&expected),
        Some(&serde_json::to_value(actual)?),
        &mut differences,
    );

    match (existing.is_some(), update) {
        (true, true) => writeln!(log, "Updated {display}:")?,
        (true, false) => writeln!(log, "Snapshot differs for {display}:")?,
        (false, true) => writeln!(log, "Created {display}:")?,
        (false, false) => writeln!(log, "Snapshot {display} is missing:")?,
    }

    if differences.is_empty() {
        writeln!(log, "  (only the YAML formatting differs)")?;
    }

    for difference in differences {
        writeln!(log, "{difference}")?;
    }

    if update {
        fs::write(yaml_file, text)?;
    }

    Ok(())
}

/// The path without a leading `./`, for messages.
fn short_path(path: &Path) -> &Path {
    path.strip_prefix(".").unwrap_or(path)
}

/// Describes each field that differs between two snapshots, by its path in the YAML file
/// (such as `tests[2].output`), with the expected value marked `-` and the actual one `+`.
/// A value that's `None` is absent: a key only on one side, or an index past the end of a list.
fn diff_values(
    path: &str,
    expected: Option<&Value>,
    actual: Option<&Value>,
    differences: &mut Vec<String>,
) {
    if expected == actual {
        return;
    }

    match (expected, actual) {
        (Some(Value::Object(expected)), Some(Value::Object(actual))) => {
            let keys = expected
                .keys()
                .chain(actual.keys().filter(|key| !expected.contains_key(*key)));

            for key in keys {
                let path = if path.is_empty() {
                    key.clone()
                } else {
                    format!("{path}.{key}")
                };
                diff_values(&path, expected.get(key), actual.get(key), differences);
            }
        }
        (Some(Value::Array(expected)), Some(Value::Array(actual))) => {
            for i in 0..expected.len().max(actual.len()) {
                let expected = expected.get(i);
                let actual = actual.get(i);
                let name = actual
                    .or(expected)
                    .and_then(|value| value.get("name"))
                    .and_then(Value::as_str)
                    .map_or_else(String::new, |name| format!(" ({name})"));
                diff_values(&format!("{path}[{i}]{name}"), expected, actual, differences);
            }
        }
        _ => {
            let path = if path.is_empty() { "(root)" } else { path };
            let mut difference = format!("  {path}:");
            for (marker, value) in [('-', expected), ('+', actual)] {
                if let Some(value) = value {
                    for line in format_value(value).lines() {
                        write!(difference, "\n    {marker} {line}").unwrap();
                    }
                }
            }
            differences.push(difference);
        }
    }
}

/// A single-line string as it is, and anything else (including an empty or multi-line string,
/// so its quoting and final newline show) as YAML.
fn format_value(value: &Value) -> String {
    match value {
        Value::String(string) if !string.is_empty() && !string.contains('\n') => string.clone(),
        _ => {
            to_yaml(value).map_or_else(|_| format!("{value:?}"), |yaml| yaml.trim_end().to_string())
        }
    }
}

/// Runs the program and its debug build with the flags of the target they're compiled for, and
/// records the results in the test case.
fn handle_test_case(
    allocator: &mut Allocator,
    target: ClvmTarget,
    test_case: &mut TestCase,
    program: Option<NodePtr>,
    debug_program: Option<NodePtr>,
    sidecars: [Option<&[u8]>; 2],
) -> Result<()> {
    let Some(ptr) = program else {
        test_case.program = None;
        test_case.debug_program = None;
        test_case.output = None;
        test_case.debug_output = None;
        test_case.clvm_error = None;
        test_case.debug_clvm_error = None;
        test_case.runtime_cost = None;
        test_case.byte_cost = None;
        test_case.total_cost = None;

        return Ok(());
    };

    let debug_ptr = debug_program.unwrap();

    let env = assemble(allocator, test_case.solution.as_deref().unwrap_or("()"))?;

    test_case.program = Some(disassemble_program(allocator, ptr));
    test_case.debug_program = Some(disassemble_program(allocator, debug_ptr));

    let release_dialect = ChiaDialect::new(target.flags());
    let response = run_program(allocator, &release_dialect, ptr, env, 100_000_000);

    let debug_dialect = DebugDialect::new(target.flags(), false);
    let debug_response = run_program(allocator, &debug_dialect, debug_ptr, env, 100_000_000);
    let print_messages = debug_dialect.log();

    if let Some(sidecar) = sidecars[0] {
        check_explanation(allocator, &release_dialect, ptr, sidecar, env)?;
    }
    if let Some(sidecar) = sidecars[1] {
        check_explanation(allocator, &debug_dialect, debug_ptr, sidecar, env)?;
    }

    let bytes = node_to_bytes(allocator, ptr)?.len();
    let byte_cost = bytes as u64 * COST_PER_BYTE;
    test_case.byte_cost = Some(byte_cost);

    test_case.print_messages = print_messages
        .into_iter()
        .map(|message| message.message)
        .collect();

    match response {
        Ok(output) => {
            test_case.output = Some(disassemble_value(allocator, output.1));
            test_case.runtime_cost = Some(output.0);
            test_case.total_cost = Some(output.0 + byte_cost);
            test_case.clvm_error = None;
        }
        Err(error) => {
            test_case.clvm_error = Some(match error {
                EvalErr::Raise(ptr) => format!("clvm raise {}", disassemble_value(allocator, ptr)),
                _ => error.to_string(),
            });
            test_case.runtime_cost = None;
            test_case.total_cost = None;
            test_case.output = None;
        }
    }

    match debug_response {
        Ok(output) => {
            test_case.debug_output = Some(disassemble_value(allocator, output.1));
            test_case.debug_clvm_error = None;
        }
        Err(error) => {
            test_case.debug_clvm_error = Some(match error {
                EvalErr::Raise(ptr) => format!("clvm raise {}", disassemble_value(allocator, ptr)),
                _ => error.to_string(),
            });
            test_case.debug_output = None;
        }
    }

    if test_case.debug_output == test_case.output {
        test_case.debug_output = None;
    }

    if test_case.debug_clvm_error == test_case.clvm_error {
        test_case.debug_clvm_error = None;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_cases_are_matched_by_name() {
        let test_case = |name: &str, solution: &str| TestCase {
            name: Some(name.to_string()),
            solution: Some(solution.to_string()),
            ..TestCase::default()
        };
        let names = ["c", "b", "d"].map(|name| Some(name.to_string()));

        let matched = match_test_cases(
            vec![
                test_case("a", "1"),
                test_case("b", "2"),
                test_case("c", "3"),
            ],
            names.clone(),
        );

        assert_eq!(
            matched,
            [
                test_case("c", "3"),
                test_case("b", "2"),
                TestCase {
                    name: names[2].clone(),
                    ..TestCase::default()
                },
            ]
        );
    }

    #[test]
    fn tests() -> Result<()> {
        let failed = run_tests(None, Path::new("../.."), false)?;

        assert!(!failed);

        Ok(())
    }

    /// A directory under the system temp directory, removed when dropped.
    fn temp_dir(name: &str) -> Result<TempDir> {
        Ok(tempfile::Builder::new()
            .prefix(&format!("rue-tests-{name}-"))
            .tempdir()?)
    }

    const MAIN: &str = "fn main() -> Int {\n    1\n}\n";

    fn args(args: &[&str]) -> Result<Args, String> {
        parse_args(args.iter().map(ToString::to_string))
    }

    #[test]
    fn parses_args() {
        assert_eq!(args(&[]), Ok(Args::default()));
        assert_eq!(
            args(&["--bless"]),
            Ok(Args {
                bless: true,
                ..Args::default()
            })
        );
        assert_eq!(
            args(&["casting"]),
            Ok(Args {
                filter: Some("casting".to_string()),
                ..Args::default()
            })
        );
        let both = Args {
            bless: true,
            filter: Some("casting".to_string()),
            help: false,
        };
        assert_eq!(args(&["--bless", "casting"]), Ok(both.clone()));
        assert_eq!(args(&["casting", "--bless"]), Ok(both));
        assert!(args(&["--help"]).unwrap().help);
        assert!(args(&["-h"]).unwrap().help);
    }

    #[test]
    fn rejects_bad_args() {
        assert!(args(&["--update"]).is_err());
        assert!(args(&["-b"]).is_err());
        assert!(args(&["one", "two"]).is_err());
    }

    #[test]
    fn diffs_changed_fields() -> Result<()> {
        let expected = TestFile {
            tests: vec![TestCase {
                name: Some("casting".to_string()),
                output: Some("()".to_string()),
                runtime_cost: Some(10),
                ..TestCase::default()
            }],
            diagnostics: vec!["Unused function `f` at main.rue:1:4".to_string()],
            ..TestFile::default()
        };
        let mut actual = expected.clone();
        actual.tests[0].output = Some("1".to_string());
        actual.tests[0].runtime_cost = None;
        actual.diagnostics.clear();
        actual.main.program = Some("(q . 1)".to_string());

        let mut differences = Vec::new();
        diff_values(
            "",
            Some(&serde_json::to_value(&expected)?),
            Some(&serde_json::to_value(&actual)?),
            &mut differences,
        );

        assert_eq!(
            differences,
            [
                "  tests[0] (casting).output:\n    - ()\n    + 1",
                "  tests[0] (casting).runtime_cost:\n    - 10",
                "  diagnostics:\n    - - Unused function `f` at main.rue:1:4",
                "  program:\n    + (q . 1)",
            ]
        );

        Ok(())
    }

    #[test]
    fn diffs_null_and_empty_values() -> Result<()> {
        let expected: Value = serde_saphyr::from_str("output: ''\nstale: null\n")?;
        let actual: Value = serde_saphyr::from_str("output: '1'\n")?;

        let mut differences = Vec::new();
        diff_values("", Some(&expected), Some(&actual), &mut differences);

        assert_eq!(
            differences,
            ["  output:\n    - \"\"\n    + 1", "  stale:\n    - null"]
        );

        Ok(())
    }

    /// A base directory laid out like the repo, with the directories `run_tests` reads.
    fn corpus(name: &str) -> Result<TempDir> {
        let dir = temp_dir(name)?;
        for path in ["tests", "examples", "crates/rue-compiler/src/std"] {
            fs::create_dir_all(dir.path().join(path))?;
        }
        Ok(dir)
    }

    /// Every file under `path`, with its contents.
    fn read_tree(path: &Path) -> Result<BTreeMap<PathBuf, Vec<u8>>> {
        let mut files = BTreeMap::new();
        for entry in fs::read_dir(path)? {
            let path = entry?.path();
            if path.is_dir() {
                files.extend(read_tree(&path)?);
            } else {
                files.insert(path.clone(), fs::read(&path)?);
            }
        }
        Ok(files)
    }

    #[test]
    fn hard_fork_2_compiles_and_runs_for_hard_fork_2() -> Result<()> {
        let dir = corpus("hard_fork_2")?;
        let tests = dir.path().join("tests");
        let source = "test fn hash(value: Bytes) -> Bytes32 {\n    keccak256(value)\n}\n";

        for hard_fork_2 in [false, true] {
            fs::write(tests.join("hash.rue"), source)?;
            fs::write(
                tests.join("hash.yaml"),
                format!(
                    "{}tests:\n- name: hash\n  solution: (\"hello\")\n",
                    if hard_fork_2 {
                        "hard_fork_2: true\n"
                    } else {
                        ""
                    }
                ),
            )?;

            run_tests(None, dir.path(), true)?;

            // The setting is kept, and `keccak256` is only allowed with it
            let file = read_snapshot(&tests.join("hash.yaml"))?;
            assert_eq!(file.hard_fork_2, hard_fork_2);

            if hard_fork_2 {
                assert!(file.diagnostics.is_empty(), "{:?}", file.diagnostics);
                assert_eq!(
                    file.tests[0].output.as_deref(),
                    Some("0x1c8aff950685c2ed4bc3174f3472287b56d9517b9c948127319a09a7a36deac8")
                );
            } else {
                assert!(
                    file.diagnostics
                        .iter()
                        .any(|diagnostic| diagnostic.contains("hard fork 2")),
                    "{:?}",
                    file.diagnostics
                );
            }
        }

        // `operand_limits` has no effect with it, so setting both is an error
        fs::write(
            tests.join("hash.yaml"),
            "operand_limits: true\nhard_fork_2: true\n",
        )?;
        assert!(run_tests(None, dir.path(), true).is_err());

        Ok(())
    }

    #[test]
    fn operand_limits_compile_and_run_with_limits() -> Result<()> {
        let dir = corpus("operand_limits")?;
        let tests = dir.path().join("tests");
        let source = "test fn product(x: Int, flag: Bool) -> Int {\n    let p = x * 3;\n    if flag {\n        p\n    } else {\n        0\n    }\n}\n";
        let solution = format!("(0x{} ())", "01".repeat(257));

        for operand_limits in [false, true] {
            fs::write(tests.join("product.rue"), source)?;
            fs::write(
                tests.join("product.yaml"),
                format!(
                    "{}tests:\n- name: product\n  solution: {solution}\n",
                    if operand_limits {
                        "operand_limits: true\n"
                    } else {
                        ""
                    }
                ),
            )?;

            run_tests(None, dir.path(), true)?;

            // The setting is kept, and with it, the product is kept and fails for the long operand
            let file = read_snapshot(&tests.join("product.yaml"))?;
            let test = &file.tests[0];
            assert_eq!(file.operand_limits, operand_limits);
            assert_eq!(test.solution.as_deref(), Some(solution.as_str()));
            assert_eq!(test.debug_clvm_error, None);
            assert_eq!(test.debug_output, None);

            if operand_limits {
                assert_eq!(test.clvm_error.as_deref(), Some("InvalidOperatorArg: *"));
            } else {
                assert_eq!(test.output.as_deref(), Some("()"));
            }
        }

        Ok(())
    }

    #[test]
    fn bless_keeps_solutions_with_reordered_tests() -> Result<()> {
        let dir = corpus("reordered")?;
        let tests = dir.path().join("tests");
        fs::write(
            tests.join("reordered.rue"),
            "test fn first() {\n    assert 1 == 1;\n}\n\ntest fn second() {\n    assert 2 == 2;\n}\n",
        )?;
        // The snapshot lists the tests in the other order, as if they had been swapped
        fs::write(
            tests.join("reordered.yaml"),
            "tests:\n- name: second\n  solution: (5)\n- name: first\n",
        )?;

        run_tests(None, dir.path(), true)?;

        let file = read_snapshot(&tests.join("reordered.yaml"))?;
        let solutions: Vec<_> = file
            .tests
            .iter()
            .map(|test| (test.name.as_deref(), test.solution.as_deref()))
            .collect();
        assert_eq!(
            solutions,
            [(Some("first"), None), (Some("second"), Some("(5)"))]
        );

        Ok(())
    }

    #[test]
    fn default_run_writes_nothing_and_bless_converges() -> Result<()> {
        let dir = corpus("bless")?;
        let tests = dir.path().join("tests");
        fs::write(tests.join("unformatted.rue"), "fn main()->Int{1}\n")?;
        // An unfinished function at the end of a file without a final newline, where the
        // formatter and the compiler report the syntax error at different positions
        fs::write(tests.join("broken.rue"), "fn main() -> Int {\n    1 +")?;

        let before = read_tree(dir.path())?;
        assert!(run_tests(None, dir.path(), false)?);
        assert_eq!(read_tree(dir.path())?, before);

        // The formatting pass hands files it can't parse on to be checked, not skipped
        let mut failed = false;
        let unparsed = check_formatting_corpus(dir.path(), None, false, &mut failed)?;
        assert!(failed);
        let unparsed: Vec<_> = unparsed.into_iter().map(|(path, _)| path).collect();
        assert_eq!(unparsed, [tests.join("broken.rue")]);

        assert!(run_tests(None, dir.path(), true)?);
        assert_eq!(fs::read_to_string(tests.join("unformatted.rue"))?, MAIN);
        assert!(tests.join("unformatted.yaml").exists());
        assert!(tests.join("broken.yaml").exists());
        assert!(dir.path().join("std_tests.yaml").exists());

        assert!(!run_tests(None, dir.path(), false)?);

        // A key the harness doesn't write fails the check, and `--bless` removes it
        let snapshot = tests.join("unformatted.yaml");
        let blessed = fs::read_to_string(&snapshot)?;
        fs::write(&snapshot, format!("{blessed}stale_field: 5\n"))?;
        let before = read_tree(dir.path())?;
        assert!(run_tests(None, dir.path(), false)?);
        assert_eq!(read_tree(dir.path())?, before);
        assert!(run_tests(None, dir.path(), true)?);
        assert_eq!(fs::read_to_string(&snapshot)?, blessed);
        assert!(!run_tests(None, dir.path(), false)?);

        Ok(())
    }

    #[test]
    fn filter_limits_what_bless_writes() -> Result<()> {
        let dir = corpus("filter")?;
        let tests = dir.path().join("tests");
        let unformatted = "fn main()->Int{1}\n";
        fs::write(tests.join("wanted.rue"), unformatted)?;
        // Without a snapshot
        fs::write(tests.join("other.rue"), unformatted)?;
        // With an out of date snapshot
        fs::write(tests.join("another.rue"), MAIN)?;
        fs::write(tests.join("another.yaml"), "program: (q . 2)\n")?;
        // A new syntax error test whose path matches the filter but whose name doesn't
        fs::create_dir(tests.join("wanted_group"))?;
        fs::write(
            tests.join("wanted_group/broken.rue"),
            "fn main() -> Int {\n",
        )?;

        assert!(run_tests(Some("wanted"), dir.path(), true)?);

        assert_eq!(fs::read_to_string(tests.join("wanted.rue"))?, MAIN);
        assert!(tests.join("wanted.yaml").exists());
        assert_eq!(fs::read_to_string(tests.join("other.rue"))?, unformatted);
        assert!(!tests.join("other.yaml").exists());
        assert_eq!(
            fs::read_to_string(tests.join("another.yaml"))?,
            "program: (q . 2)\n"
        );
        assert!(!tests.join("wanted_group/broken.yaml").exists());

        // The filtered run converges, without checking the syntax error test it didn't run
        assert!(!run_tests(Some("wanted"), dir.path(), false)?);

        Ok(())
    }

    #[test]
    fn names_the_test_that_compiles_a_file() -> Result<()> {
        let dir = temp_dir("test-name")?;
        let project = dir.path().join("group/project");
        fs::create_dir_all(project.join("nested"))?;
        fs::write(dir.path().join("group/project.yaml"), "")?;
        fs::write(dir.path().join("group/single.rue"), MAIN)?;

        assert_eq!(test_name(&dir.path().join("group/single.rue"))?, "single");
        assert_eq!(test_name(&project.join("main.rue"))?, "project");
        assert_eq!(test_name(&project.join("nested/helper.rue"))?, "project");

        Ok(())
    }

    #[test]
    fn parse_errors_need_a_snapshot_that_expects_them() -> Result<()> {
        let dir = temp_dir("parse-errors")?;
        let path = dir.path().join("broken.rue");
        fs::write(&path, "fn main() -> Int {\n")?;

        let Err(FormatError::Parse { diagnostics }) =
            format_source(&fs::read_to_string(&path)?, &FormatOptions::default())
        else {
            panic!("expected a parse error");
        };
        let unparsed = vec![(path, diagnostics)];

        // No snapshot at all
        let mut failed = false;
        check_expected_parse_errors(&unparsed, None, &mut failed)?;
        assert!(failed);

        // A snapshot without the syntax error
        fs::write(
            dir.path().join("broken.yaml"),
            "diagnostics:\n- Something else\n",
        )?;
        let mut failed = false;
        check_expected_parse_errors(&unparsed, None, &mut failed)?;
        assert!(failed);

        // A snapshot with the same syntax errors in another file
        let mut snapshot = TestFile::default();
        for diagnostic in &unparsed[0].1 {
            let message = format!("{} at other.rue:{}", diagnostic.kind, diagnostic.start());
            snapshot.diagnostics.push(message);
        }
        fs::write(dir.path().join("broken.yaml"), to_yaml(&snapshot)?)?;
        let mut failed = false;
        check_expected_parse_errors(&unparsed, None, &mut failed)?;
        assert!(failed);

        // The snapshot the harness writes, which records the syntax error
        let mut failed = false;
        walk_dir(dir.path(), None, true, &mut failed)?;
        let mut failed = false;
        check_expected_parse_errors(&unparsed, None, &mut failed)?;
        assert!(!failed);

        Ok(())
    }
}
