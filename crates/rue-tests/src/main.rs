use std::{
    env, fs,
    path::{Path, PathBuf},
};

use clap::Parser;
use clvm_tools_rs::classic::clvm_tools::binutils;
use clvm_utils::tree_hash;
use clvmr::{
    reduction::Reduction,
    run_program,
    serde::{node_from_bytes, node_to_bytes},
    Allocator, ChiaDialect,
};
use indexmap::{IndexMap, IndexSet};
use rue_compiler::{compile, DiagnosticKind};
use rue_parser::{line_col, LineCol};
use serde::{Deserialize, Serialize};
use walkdir::{DirEntry, WalkDir};

#[derive(Clone, Serialize, Deserialize)]
#[serde(untagged)]
enum Expected {
    Case(TestCase),
    Errs(TestErrors),
}

#[derive(Clone, Serialize, Deserialize)]
struct TestCase {
    bytes: usize,
    cost: u64,
    input: String,
    output: String,
    hash: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    error: Option<String>,
}

#[derive(Clone)]
struct TestOutput {
    bytes: Vec<u8>,
    cost: u64,
    output: Result<String, String>,
    hash: String,
}

#[derive(Clone, Serialize, Deserialize)]
struct TestErrors {
    parser_errors: Vec<String>,
    compiler_errors: Vec<String>,
}

/// Automatically updates test cases.
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Check only mode.
    #[arg(short, long)]
    update: bool,
}

fn iter_tests() -> impl Iterator<Item = PathBuf> {
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    WalkDir::new(manifest_dir.join("../../tests"))
        .into_iter()
        .chain(WalkDir::new(manifest_dir.join("../../examples")))
        .map(Result::unwrap)
        .filter(|file| {
            Path::new(file.path().to_str().unwrap())
                .extension()
                .map_or(false, |ext| ext.eq_ignore_ascii_case("rue"))
        })
        .map(DirEntry::into_path)
}

fn run_test(source: &str, input: &str) -> Result<TestOutput, TestErrors> {
    let (root, parser_errors) = rue_parser::parse(source);
    let mut allocator = Allocator::new();
    let output = compile(&mut allocator, &root, parser_errors.is_empty());

    let parser_errors: Vec<String> = parser_errors
        .into_iter()
        .map(|error| {
            let LineCol { line, col } = line_col(source, error.span().start);
            let line = line + 1;
            let col = col + 1;
            format!("Error: {} ({line}:{col})", error.kind())
        })
        .collect();

    let compiler_errors: Vec<String> = output
        .diagnostics()
        .iter()
        .map(|error| {
            let LineCol { line, col } = line_col(source, error.span().start);
            let line = line + 1;
            let col = col + 1;
            match error.kind() {
                DiagnosticKind::Error(kind) => format!("Error: {kind} ({line}:{col})"),
                DiagnosticKind::Warning(kind) => format!("Error: {kind} ({line}:{col})"),
            }
        })
        .collect();

    if !parser_errors.is_empty() || !compiler_errors.is_empty() {
        return Err(TestErrors {
            parser_errors,
            compiler_errors,
        });
    }

    let bytes = node_to_bytes(&allocator, output.node_ptr()).unwrap();
    let hash = hex::encode(tree_hash(&allocator, output.node_ptr()));

    let mut old_allocator = clvmr_old::Allocator::new();

    let input_ptr = binutils::assemble(&mut old_allocator, input).unwrap();
    let input_bytes = clvmr_old::serde::node_to_bytes(&old_allocator, input_ptr).unwrap();
    let input_ptr = node_from_bytes(&mut allocator, &input_bytes).unwrap();

    let output = run_program(
        &mut allocator,
        &ChiaDialect::new(0),
        output.node_ptr(),
        input_ptr,
        u64::MAX,
    );

    let (cost, output) = match output {
        Ok(Reduction(cost, node_ptr)) => {
            let output_bytes = node_to_bytes(&allocator, node_ptr).unwrap();
            let output_ptr =
                clvmr_old::serde::node_from_bytes(&mut old_allocator, &output_bytes).unwrap();
            let output = binutils::disassemble(&old_allocator, output_ptr, None);
            (cost, Ok(output))
        }
        Err(error) => {
            let output_bytes = node_to_bytes(&allocator, error.0).unwrap();
            let output_ptr =
                clvmr_old::serde::node_from_bytes(&mut old_allocator, &output_bytes).unwrap();
            let output = binutils::disassemble(&old_allocator, output_ptr, None);
            (0, Err(output))
        }
    };

    Ok(TestOutput {
        bytes,
        cost,
        output,
        hash,
    })
}

fn main() {
    let args = Args::parse();

    let failed = run_tests(args.update);
    if failed > 0 {
        println!("\n{failed} tests failed");
        std::process::exit(1);
    }
}

fn run_tests(update: bool) -> usize {
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let test_case_path = manifest_dir.join("../../tests.toml");
    let text = fs::read_to_string(test_case_path.as_path())
        .ok()
        .unwrap_or_default();

    let mut test_cases: IndexMap<String, Expected> = toml::from_str(&text).unwrap();
    let mut visited_names = IndexSet::new();

    let mut failed_count = 0;
    let mut failed_tests = IndexMap::new();

    for test in iter_tests() {
        let name = test
            .file_name()
            .unwrap()
            .to_str()
            .unwrap()
            .strip_suffix(".rue")
            .unwrap();

        if !visited_names.insert(name.to_string()) {
            println!("duplicate test name: {name}");
            failed_count += 1;
            continue;
        }

        println!(
            "running test {}: {}",
            name,
            test.canonicalize().unwrap().to_str().unwrap()
        );
        let mut failed = false;
        let mut lines = Vec::new();

        let source = fs::read_to_string(&test).unwrap();

        let expected = test_cases.get(name).cloned();

        if expected.is_none() {
            lines.push("missing toml entry".to_string());
            failed = true;
        }

        let output = run_test(
            &source,
            &expected
                .as_ref()
                .and_then(|expected| {
                    if let Expected::Case(case) = expected {
                        Some(case.input.clone())
                    } else {
                        None
                    }
                })
                .unwrap_or("()".to_string()),
        );

        if let Some(expected) = expected.clone() {
            match (expected, &output) {
                (Expected::Errs(expected_test_errors), Err(test_errors)) => {
                    if expected_test_errors.parser_errors != test_errors.parser_errors {
                        lines.push("expected parser errors:".to_string());
                        for error in expected_test_errors.parser_errors {
                            lines.push(format!("  {error}"));
                        }
                        lines.push("actual parser errors:".to_string());
                        for error in &test_errors.parser_errors {
                            lines.push(format!("  {error}"));
                        }
                        failed = true;
                    }

                    if expected_test_errors.compiler_errors != test_errors.compiler_errors {
                        lines.push("expected compiler errors:".to_string());
                        for error in expected_test_errors.compiler_errors {
                            lines.push(format!("  {error}"));
                        }
                        lines.push("actual compiler errors:".to_string());
                        for error in &test_errors.compiler_errors {
                            lines.push(format!("  {error}"));
                        }
                        failed = true;
                    }
                }
                (Expected::Errs(expected_test_errors), Ok(..)) => {
                    lines.push("expected parser errors:".to_string());
                    for error in expected_test_errors.parser_errors {
                        lines.push(format!("  {error}"));
                    }
                    lines.push("expected compiler errors:".to_string());
                    for error in expected_test_errors.compiler_errors {
                        lines.push(format!("  {error}"));
                    }
                    failed = true;
                }
                (Expected::Case(_expected), Err(test_errors)) => {
                    lines.push("unexpected parser errors:".to_string());
                    for error in &test_errors.parser_errors {
                        lines.push(format!("  {error}"));
                    }
                    lines.push("unexpected compiler errors:".to_string());
                    for error in &test_errors.compiler_errors {
                        lines.push(format!("  {error}"));
                    }
                    failed = true;
                }
                (Expected::Case(expected), Ok(actual)) => {
                    if expected.bytes != actual.bytes.len() {
                        lines.push(format!(
                            "expected bytes: {}, actual bytes: {}",
                            expected.bytes,
                            actual.bytes.len()
                        ));
                        failed = true;
                    }

                    if expected.cost != actual.cost {
                        lines.push(format!(
                            "expected cost: {}, actual cost: {}",
                            expected.cost, actual.cost
                        ));
                        failed = true;
                    }

                    if expected.hash != actual.hash {
                        lines.push(format!("expected hash: {}", expected.hash));
                        lines.push(format!("actual hash: {}", actual.hash));
                        failed = true;
                    }

                    let mut output_failed = false;

                    if let Ok(actual_output) = actual.output.as_ref() {
                        if &expected.output != actual_output {
                            lines.push(format!("expected output: {}", expected.output));
                            lines.push(format!("actual output: {actual_output}"));
                            failed = true;
                            output_failed = true;
                        }
                    }

                    if expected.error.as_ref() != actual.output.as_ref().err() {
                        lines.push(format!("expected error: {:?}", expected.error.as_ref()));
                        lines.push(format!("actual error: {:?}", actual.output.as_ref().err()));
                        failed = true;
                        output_failed = true;
                    }

                    if output_failed {
                        lines.push(format!("compiled program: {}", hex::encode(&actual.bytes)));
                    }
                }
            }
        } else {
            match &output {
                Ok(output) => {
                    lines.push(format!("bytes: {}", output.bytes.len()));
                    lines.push(format!("cost: {}", output.cost));
                    lines.push(format!("hash: {}", output.hash));
                    match &output.output {
                        Ok(output) => {
                            lines.push(format!("output: {output}"));
                        }
                        Err(error) => {
                            lines.push(format!("error: {error}"));
                        }
                    }
                }
                Err(errors) => {
                    lines.push("parser errors:".to_string());
                    for error in &errors.parser_errors {
                        lines.push(format!("  {error}"));
                    }
                    lines.push("compiler errors:".to_string());
                    for error in &errors.compiler_errors {
                        lines.push(format!("  {error}"));
                    }
                }
            }
        }

        if failed {
            failed_count += 1;

            let new_expected = match output {
                Ok(output) => Expected::Case(TestCase {
                    bytes: output.bytes.len(),
                    cost: output.cost,
                    input: expected
                        .and_then(|expected| {
                            if let Expected::Case(case) = expected {
                                Some(case.input.clone())
                            } else {
                                None
                            }
                        })
                        .unwrap_or("()".to_string()),
                    output: output.output.clone().unwrap_or("()".to_string()),
                    hash: output.hash,
                    error: output.output.err().map(|error| error.to_string()),
                }),
                Err(errors) => Expected::Errs(errors),
            };
            test_cases.insert(name.to_string(), new_expected);

            failed_tests.insert(
                test.canonicalize().unwrap().to_str().unwrap().to_string(),
                lines,
            );
        }
    }

    for (test, lines) in failed_tests {
        println!("\ntest failed: {test}");
        for line in lines {
            println!("  {line}");
        }
    }

    if update {
        let text = toml::to_string_pretty(&test_cases).unwrap();
        fs::write(test_case_path.as_path(), text).unwrap();
    }

    failed_count
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_regressions() {
        assert_eq!(run_tests(false), 0, "one or more tests failed");
    }
}
