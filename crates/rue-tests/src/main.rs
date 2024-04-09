use std::{env, fs, path::PathBuf};

use clap::Parser;
use clvm_tools_rs::classic::clvm_tools::binutils;
use clvm_utils::tree_hash;
use clvmr::{
    reduction::Reduction,
    run_program,
    serde::{node_from_bytes, node_to_bytes},
    Allocator, ChiaDialect,
};
use rue_compiler::compile;
use serde::{Deserialize, Serialize};
use walkdir::WalkDir;

#[derive(Clone, Serialize, Deserialize)]
struct TestCase {
    bytes: usize,
    cost: u64,
    input: String,
    output: String,
    hash: String,
}

#[derive(Clone)]
struct TestOutput {
    bytes: usize,
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
        .filter(|file| file.path().to_str().unwrap().ends_with(".rue"))
        .map(|file| file.into_path())
}

fn line_col(source: &str, index: usize) -> (usize, usize) {
    let mut line = 1;
    let mut column = 1;

    for (i, character) in source.chars().enumerate() {
        if i == index {
            break;
        }

        if character == '\n' {
            line += 1;
            column = 1;
        } else {
            column += 1;
        }
    }

    (line, column)
}

fn run_test(source: &str, input: &str) -> Result<TestOutput, TestErrors> {
    let (root, parser_errors) = rue_parser::parse(source);
    let mut allocator = Allocator::new();
    let output = compile(&mut allocator, root);

    let parser_errors: Vec<String> = parser_errors
        .into_iter()
        .map(|error| {
            let (line, column) = line_col(source, error.span().start);
            format!("{} at {line}:{column}", error.kind())
        })
        .collect();

    let compiler_errors: Vec<String> = output
        .diagnostics()
        .iter()
        .map(|error| {
            let (line, column) = line_col(source, error.span().start);
            format!("{} at {line}:{column}", error.info())
        })
        .collect();

    if !parser_errors.is_empty() || !compiler_errors.is_empty() {
        return Err(TestErrors {
            parser_errors,
            compiler_errors,
        });
    }

    let bytes = node_to_bytes(&allocator, output.node_ptr()).unwrap().len();
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
        Err(error) => (0, Err(error.to_string())),
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
        eprintln!("{} tests failed", failed);
        std::process::exit(1);
    }
}

fn run_tests(update: bool) -> usize {
    let mut failed_count = 0;

    for test in iter_tests() {
        println!(
            "running test: {}",
            test.canonicalize().unwrap().to_str().unwrap()
        );
        let mut failed = false;

        let source = fs::read_to_string(&test).unwrap();
        let toml_path = test.with_extension("toml");

        let toml_text = match fs::read_to_string(toml_path.as_path()) {
            Ok(text) => text,
            Err(_) => {
                eprintln!("missing toml file: {}", toml_path.display());
                failed = true;
                String::new()
            }
        };

        let parsed: Option<Result<TestCase, TestErrors>> =
            match toml::from_str::<TestCase>(&toml_text) {
                Ok(parsed) => Some(Ok(parsed)),
                Err(..) => match toml::from_str::<TestErrors>(&toml_text) {
                    Ok(parsed) => Some(Err(parsed)),
                    Err(error) => {
                        eprintln!("failed to parse toml: {error}");
                        None
                    }
                },
            };

        let output = run_test(
            &source,
            &parsed
                .as_ref()
                .and_then(|parsed| parsed.as_ref().ok())
                .map(|parsed| parsed.input.clone())
                .unwrap_or("()".to_string()),
        );

        if let Some(parsed) = parsed.clone() {
            match (parsed, &output) {
                (Err(expected_test_errors), Err(test_errors)) => {
                    if expected_test_errors.parser_errors != test_errors.parser_errors {
                        println!("expected parser errors:");
                        for error in expected_test_errors.parser_errors {
                            println!("  {}", error);
                        }
                        println!("actual parser errors:");
                        for error in test_errors.parser_errors.iter() {
                            println!("  {}", error);
                        }
                        failed = true;
                    }

                    if expected_test_errors.compiler_errors != test_errors.compiler_errors {
                        println!("expected compiler errors:");
                        for error in expected_test_errors.compiler_errors {
                            println!("  {}", error);
                        }
                        println!("actual compiler errors:");
                        for error in test_errors.compiler_errors.iter() {
                            println!("  {}", error);
                        }
                        failed = true;
                    }
                }
                (Err(expected_test_errors), Ok(..)) => {
                    println!("expected parser errors:");
                    for error in expected_test_errors.parser_errors {
                        println!("  {}", error);
                    }
                    println!("expected compiler errors:");
                    for error in expected_test_errors.compiler_errors {
                        println!("  {}", error);
                    }
                    failed = true;
                }
                (Ok(_expected), Err(test_errors)) => {
                    println!("unexpected parser errors:");
                    for error in test_errors.parser_errors.iter() {
                        println!("  {}", error);
                    }
                    println!("unexpected compiler errors:");
                    for error in test_errors.compiler_errors.iter() {
                        println!("  {}", error);
                    }
                    failed = true;
                }
                (Ok(expected), Ok(actual)) => {
                    if expected.bytes != actual.bytes {
                        println!(
                            "expected bytes: {}, actual bytes: {}",
                            expected.bytes, actual.bytes
                        );
                        failed = true;
                    }

                    if expected.cost != actual.cost {
                        println!(
                            "expected cost: {}, actual cost: {}",
                            expected.cost, actual.cost
                        );
                        failed = true;
                    }

                    if expected.hash != actual.hash {
                        println!("expected hash: {}", expected.hash);
                        println!("actual hash: {}", actual.hash);
                        failed = true;
                    }

                    match &actual.output {
                        Ok(output) => {
                            if &expected.output != output {
                                println!("expected output: {}", expected.output);
                                println!("actual output: {}", output);
                                failed = true;
                            }
                        }
                        Err(error) => {
                            println!("unexpected clvm error: {}", error);
                            failed = true;
                        }
                    }
                }
            }
        }

        if failed {
            failed_count += 1;
            eprintln!("test failed");

            if update {
                let toml_text = match output {
                    Ok(output) => toml::to_string_pretty(&TestCase {
                        bytes: output.bytes,
                        cost: output.cost,
                        input: parsed
                            .and_then(|parsed| parsed.ok())
                            .map(|parsed| parsed.input)
                            .unwrap_or("()".to_string()),
                        output: output.output.unwrap_or("()".to_string()),
                        hash: output.hash,
                    })
                    .unwrap(),
                    Err(errors) => toml::to_string_pretty(&errors).unwrap(),
                };
                fs::write(toml_path, toml_text).unwrap();
            }
        } else {
            println!("test passed");
        }
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
