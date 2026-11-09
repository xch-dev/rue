use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::env;
use std::fs;
use std::path::Path;
use std::process;

use anyhow::Result;
use clvm_utils::tree_hash;
use clvmr::error::EvalErr;
use clvmr::{Allocator, ChiaDialect, NodePtr, run_program, serde::node_to_bytes};
use indexmap::IndexMap;
use rue_clvm::{assemble, disassemble_program, disassemble_value};
use rue_compiler::normalize_path;
use rue_compiler::{Compiler, FileTree};
use rue_diagnostic::{DiagnosticSeverity, SourceKind};
use rue_formatter::{FormatError, FormatOptions, format_source};
use rue_lir::{CLVM_FLAGS, DebugDialect};
use rue_options::CompilerOptions;
use serde::{Deserialize, Serialize};

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

    let args: Vec<String> = env::args().collect();
    let filter_arg = args.get(1).cloned();

    let failed = run_tests(filter_arg.as_deref(), Path::new("."), true)?;

    if failed {
        process::exit(1);
    }

    Ok(())
}

fn run_tests(filter_arg: Option<&str>, base_path: &Path, update: bool) -> Result<bool> {
    let mut failed = false;

    check_formatting_corpus(base_path, filter_arg, update, &mut failed)?;
    walk_dir(&base_path.join("tests"), filter_arg, update, &mut failed)?;
    walk_dir(&base_path.join("examples"), filter_arg, update, &mut failed)?;

    println!("Running std tests");

    let yaml_file = base_path.join("std_tests.yaml");

    let mut file: TestFile = if yaml_file.try_exists()? {
        serde_yml::from_str(&fs::read_to_string(&yaml_file)?)?
    } else {
        TestFile::default()
    };

    let original = file.clone();

    let mut allocator = Allocator::new();

    let mut ctx = Compiler::new(CompilerOptions::default());
    let mut debug_ctx = Compiler::new(CompilerOptions::debug());

    let unit = FileTree::empty(&mut ctx)?;
    let debug_unit = FileTree::empty(&mut debug_ctx)?;

    file.diagnostics = Vec::new();

    let mut codegen = true;

    for diagnostic in ctx.take_diagnostics() {
        file.diagnostics.push(diagnostic.message(base_path));

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

    if file != original {
        if update {
            eprintln!("Test failed, updated yaml file");
        } else {
            eprintln!("Test failed, moving on");
        }
        failed = true;
    }

    if update {
        fs::write(yaml_file, serde_yml::to_string(&file)?)?;
    }

    Ok(failed)
}

fn check_formatting_corpus(
    base_path: &Path,
    filter_arg: Option<&str>,
    update: bool,
    failed: &mut bool,
) -> Result<()> {
    println!("Checking Rue formatting");
    let mut files = Vec::new();
    collect_rue_files(&base_path.join("tests"), &mut files)?;
    collect_rue_files(&base_path.join("examples"), &mut files)?;
    collect_rue_files(&base_path.join("crates/rue-compiler/src/std"), &mut files)?;
    files.sort();

    for path in files {
        let display = path.strip_prefix(base_path).unwrap_or(&path);
        if let Some(filter) = filter_arg
            && !display.to_string_lossy().contains(filter)
        {
            continue;
        }

        let original = fs::read_to_string(&path)?;
        let formatted = match format_source(&original, &FormatOptions::default()) {
            Ok(formatted) => formatted,
            Err(FormatError::Parse { .. }) => continue,
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

    Ok(())
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
            if !entry
                .path()
                .parent()
                .unwrap()
                .join(format!("{name}.yaml"))
                .try_exists()?
            {
                handle_test_file(name, &entry.path(), failed, update, false)?;
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

        // If a filter argument is provided, skip tests that don't contain it
        if let Some(ref filter) = filter_arg
            && !name.contains(filter)
        {
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
        serde_yml::from_str(&fs::read_to_string(&yaml_file)?)?
    } else {
        TestFile::default()
    };

    let original = file.clone();

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
        file.diagnostics.push(diagnostic.message(&base_path));

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

    if file != original {
        if update {
            eprintln!("Test failed, updated yaml file");
        } else {
            eprintln!("Test failed, moving on");
        }
        *failed = true;
    }

    if update {
        fs::write(yaml_file, serde_yml::to_string(&file)?)?;
    }

    Ok(())
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
    test_case.byte_cost = Some(bytes as u64 * 12_000);

    test_case.print_messages = print_messages
        .into_iter()
        .map(|message| message.message)
        .collect();

    match response {
        Ok(output) => {
            test_case.output = Some(disassemble_value(allocator, output.1));
            test_case.runtime_cost = Some(output.0);
            test_case.total_cost = Some(output.0 + bytes as u64 * 12_000);
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
}
