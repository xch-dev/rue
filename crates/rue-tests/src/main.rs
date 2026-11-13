use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::env;
use std::fmt::Write;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;

use anyhow::{Context, Result};
use clvm_utils::tree_hash;
use clvmr::error::EvalErr;
use clvmr::{Allocator, ChiaDialect, NodePtr, run_program, serde::node_to_bytes};
use indexmap::IndexMap;
use rue_clvm::{assemble, disassemble_program, disassemble_value};
use rue_compiler::normalize_path;
use rue_compiler::{Compiler, FileTree};
use rue_diagnostic::{Diagnostic, DiagnosticSeverity, SourceKind};
use rue_formatter::{FormatError, FormatOptions, format_source};
use rue_lir::{CLVM_FLAGS, COST_PER_BYTE, DebugDialect};
use rue_options::CompilerOptions;
use serde::{Deserialize, Serialize};
use serde_yml::{Mapping, Value};

const USAGE: &str = "\
Usage: rue-tests [--bless] [FILTER]

Checks the .yaml snapshots under tests/ and examples/, std_tests.yaml, and the formatting of
the .rue files in tests/, examples/ and crates/rue-compiler/src/std/. Reports each difference
and exits 1 if there are any, without changing files.

Arguments:
  [FILTER]  Only run the tests whose name contains FILTER, and only check the formatting of
            files whose path contains it

Options:
  --bless   Rewrite the snapshots and reformat the .rue files to match the current output
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
    walk_dir(&base_path.join("tests"), filter_arg, update, &mut failed)?;
    walk_dir(&base_path.join("examples"), filter_arg, update, &mut failed)?;
    check_expected_parse_errors(&unparsed, filter_arg, &mut failed)?;

    println!("Running std tests");

    let yaml_file = base_path.join("std_tests.yaml");

    let mut file: TestFile = if yaml_file.try_exists()? {
        read_snapshot(&yaml_file)?
    } else {
        TestFile::default()
    };

    let mut allocator = Allocator::new();

    let mut ctx = Compiler::new(CompilerOptions::default());
    let mut debug_ctx = Compiler::new(CompilerOptions::debug());

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

    if file.tests.len() > tests.len() {
        file.tests.truncate(tests.len());
    }

    for _ in file.tests.len()..tests.len() {
        file.tests.push(TestCase::default());
    }

    assert_eq!(debug_tests.len(), tests.len());

    for (i, test_case) in file.tests.iter_mut().enumerate() {
        test_case.name.clone_from(&tests[i].name);

        handle_test_case(
            &mut allocator,
            test_case,
            Some(tests[i].ptr),
            Some(debug_tests[i].ptr),
        )?;
    }

    compare_snapshot(&yaml_file, &file, update, &mut failed)?;

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

/// The name `walk_dir` filters the test that compiles a `.rue` file by: the name of the nearest
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

fn walk_dir(path: &Path, filter_arg: Option<&str>, update: bool, failed: &mut bool) -> Result<()> {
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
                handle_test_file(name, &path, failed, update, false)?;
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

        handle_test_file(name, &entry.path(), failed, update, is_dir)?;
    }

    for (name, path) in directories {
        if directories_to_exclude.contains(&name) {
            continue;
        }
        walk_dir(&path, filter_arg, update, failed)?;
    }

    Ok(())
}

fn handle_test_file(
    name: &str,
    entry: &Path,
    failed: &mut bool,
    update: bool,
    is_dir: bool,
) -> Result<()> {
    let parent = entry.parent().unwrap();
    let yaml_file = parent.join(format!("{name}.yaml"));

    println!("Running {name}");

    let mut file: TestFile = if yaml_file.try_exists()? {
        read_snapshot(&yaml_file)?
    } else {
        TestFile::default()
    };

    let mut allocator = Allocator::new();

    let mut ctx = Compiler::new(CompilerOptions::default());
    let mut debug_ctx = Compiler::new(CompilerOptions::debug());

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

    let main = kind
        .as_ref()
        .filter(|_| codegen)
        .map(|kind| unit.main(&mut ctx, &mut allocator, kind, base_path.clone()))
        .transpose()?
        .flatten();
    let debug_main = kind
        .as_ref()
        .filter(|_| codegen)
        .map(|kind| debug_unit.main(&mut debug_ctx, &mut allocator, kind, base_path.clone()))
        .transpose()?
        .flatten();

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

    handle_test_case(&mut allocator, &mut file.main, main, debug_main)?;

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

    if file.tests.len() > tests.len() {
        file.tests.truncate(tests.len());
    }

    for _ in file.tests.len()..tests.len() {
        file.tests.push(TestCase::default());
    }

    assert_eq!(debug_tests.len(), tests.len());

    for (i, test_case) in file.tests.iter_mut().enumerate() {
        test_case.name.clone_from(&tests[i].name);

        handle_test_case(
            &mut allocator,
            test_case,
            Some(tests[i].ptr),
            Some(debug_tests[i].ptr),
        )?;
    }

    compare_snapshot(&yaml_file, &file, update, failed)?;

    Ok(())
}

fn read_snapshot(yaml_file: &Path) -> Result<TestFile> {
    serde_yml::from_str(&fs::read_to_string(yaml_file)?)
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
) -> Result<()> {
    let display = short_path(yaml_file).display();
    let text = serde_yml::to_string(actual)?;
    let existing = yaml_file
        .try_exists()?
        .then(|| fs::read_to_string(yaml_file))
        .transpose()?;

    if existing.as_deref() == Some(text.as_str()) {
        return Ok(());
    }

    *failed = true;

    let expected = match &existing {
        Some(existing) => serde_yml::from_str(existing)?,
        None => Value::Mapping(Mapping::new()),
    };
    let mut differences = Vec::new();
    diff_values(
        "",
        Some(&expected),
        Some(&serde_yml::to_value(actual)?),
        &mut differences,
    );

    match (existing.is_some(), update) {
        (true, true) => eprintln!("Updated {display}:"),
        (true, false) => eprintln!("Snapshot differs for {display}:"),
        (false, true) => eprintln!("Created {display}:"),
        (false, false) => eprintln!("Snapshot {display} is missing:"),
    }

    if differences.is_empty() {
        eprintln!("  (only the YAML formatting differs)");
    }

    for difference in differences {
        eprintln!("{difference}");
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
        (Some(Value::Mapping(expected)), Some(Value::Mapping(actual))) => {
            let keys = expected
                .keys()
                .chain(actual.keys().filter(|key| !expected.contains_key(*key)));

            for key in keys {
                let key_name = format_value(key);
                let path = if path.is_empty() {
                    key_name
                } else {
                    format!("{path}.{key_name}")
                };
                diff_values(&path, expected.get(key), actual.get(key), differences);
            }
        }
        (Some(Value::Sequence(expected)), Some(Value::Sequence(actual))) => {
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
        _ => serde_yml::to_string(value)
            .map_or_else(|_| format!("{value:?}"), |yaml| yaml.trim_end().to_string()),
    }
}

fn handle_test_case(
    allocator: &mut Allocator,
    test_case: &mut TestCase,
    program: Option<NodePtr>,
    debug_program: Option<NodePtr>,
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

    let response = run_program(
        allocator,
        &ChiaDialect::new(CLVM_FLAGS),
        ptr,
        env,
        100_000_000,
    );

    let debug_dialect = DebugDialect::new(CLVM_FLAGS, false);
    let debug_response = run_program(allocator, &debug_dialect, debug_ptr, env, 100_000_000);
    let print_messages = debug_dialect.log();

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

    #[test]
    fn tests() -> Result<()> {
        let failed = run_tests(None, Path::new("../.."), false)?;

        assert!(!failed);

        Ok(())
    }

    /// A directory under the system temp directory, removed when dropped.
    struct TempDir(PathBuf);

    impl TempDir {
        fn new(name: &str) -> Result<Self> {
            let path = env::temp_dir().join(format!("rue-tests-{}-{name}", process::id()));
            if path.exists() {
                fs::remove_dir_all(&path)?;
            }
            fs::create_dir_all(&path)?;
            Ok(Self(path))
        }
    }

    impl Drop for TempDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
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
            Some(&serde_yml::to_value(&expected)?),
            Some(&serde_yml::to_value(&actual)?),
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
        let expected: Value = serde_yml::from_str("output: ''\nstale: null\n")?;
        let actual: Value = serde_yml::from_str("output: '1'\n")?;

        let mut differences = Vec::new();
        diff_values("", Some(&expected), Some(&actual), &mut differences);

        assert_eq!(
            differences,
            ["  output:\n    - ''\n    + 1", "  stale:\n    - null"]
        );

        Ok(())
    }

    /// A base directory laid out like the repo, with the directories `run_tests` reads.
    fn corpus(name: &str) -> Result<TempDir> {
        let dir = TempDir::new(name)?;
        for path in ["tests", "examples", "crates/rue-compiler/src/std"] {
            fs::create_dir_all(dir.0.join(path))?;
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
    fn default_run_writes_nothing_and_bless_converges() -> Result<()> {
        let dir = corpus("bless")?;
        let tests = dir.0.join("tests");
        fs::write(tests.join("unformatted.rue"), "fn main()->Int{1}\n")?;
        // An unfinished function at the end of a file without a final newline, where the
        // formatter and the compiler report the syntax error at different positions
        fs::write(tests.join("broken.rue"), "fn main() -> Int {\n    1 +")?;

        let before = read_tree(&dir.0)?;
        assert!(run_tests(None, &dir.0, false)?);
        assert_eq!(read_tree(&dir.0)?, before);

        // The formatting pass hands files it can't parse on to be checked, not skipped
        let mut failed = false;
        let unparsed = check_formatting_corpus(&dir.0, None, false, &mut failed)?;
        assert!(failed);
        let unparsed: Vec<_> = unparsed.into_iter().map(|(path, _)| path).collect();
        assert_eq!(unparsed, [tests.join("broken.rue")]);

        assert!(run_tests(None, &dir.0, true)?);
        assert_eq!(fs::read_to_string(tests.join("unformatted.rue"))?, MAIN);
        assert!(tests.join("unformatted.yaml").exists());
        assert!(tests.join("broken.yaml").exists());
        assert!(dir.0.join("std_tests.yaml").exists());

        assert!(!run_tests(None, &dir.0, false)?);

        // A key the harness doesn't write fails the check, and `--bless` removes it
        let snapshot = tests.join("unformatted.yaml");
        let blessed = fs::read_to_string(&snapshot)?;
        fs::write(&snapshot, format!("{blessed}stale_field: 5\n"))?;
        let before = read_tree(&dir.0)?;
        assert!(run_tests(None, &dir.0, false)?);
        assert_eq!(read_tree(&dir.0)?, before);
        assert!(run_tests(None, &dir.0, true)?);
        assert_eq!(fs::read_to_string(&snapshot)?, blessed);
        assert!(!run_tests(None, &dir.0, false)?);

        Ok(())
    }

    #[test]
    fn filter_limits_what_bless_writes() -> Result<()> {
        let dir = corpus("filter")?;
        let tests = dir.0.join("tests");
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

        assert!(run_tests(Some("wanted"), &dir.0, true)?);

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
        assert!(!run_tests(Some("wanted"), &dir.0, false)?);

        Ok(())
    }

    #[test]
    fn names_the_test_that_compiles_a_file() -> Result<()> {
        let dir = TempDir::new("test-name")?;
        let project = dir.0.join("group/project");
        fs::create_dir_all(project.join("nested"))?;
        fs::write(dir.0.join("group/project.yaml"), "")?;
        fs::write(dir.0.join("group/single.rue"), MAIN)?;

        assert_eq!(test_name(&dir.0.join("group/single.rue"))?, "single");
        assert_eq!(test_name(&project.join("main.rue"))?, "project");
        assert_eq!(test_name(&project.join("nested/helper.rue"))?, "project");

        Ok(())
    }

    #[test]
    fn parse_errors_need_a_snapshot_that_expects_them() -> Result<()> {
        let dir = TempDir::new("parse-errors")?;
        let path = dir.0.join("broken.rue");
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
            dir.0.join("broken.yaml"),
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
        fs::write(dir.0.join("broken.yaml"), serde_yml::to_string(&snapshot)?)?;
        let mut failed = false;
        check_expected_parse_errors(&unparsed, None, &mut failed)?;
        assert!(failed);

        // The snapshot the harness writes, which records the syntax error
        let mut failed = false;
        walk_dir(&dir.0, None, true, &mut failed)?;
        let mut failed = false;
        check_expected_parse_errors(&unparsed, None, &mut failed)?;
        assert!(!failed);

        Ok(())
    }
}
