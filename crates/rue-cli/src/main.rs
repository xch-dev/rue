#![allow(clippy::option_option)]

use std::fs;

use clap::Parser;
use clvmr::{serde::node_to_bytes, Allocator, NodePtr};
use rue_clvm::{parse_clvm, run_clvm, stringify_clvm};
use rue_compiler::{compile_raw, Diagnostic, DiagnosticKind};
use rue_parser::{line_col, parse, LineCol};

/// CLI tools for working with the Rue compiler.
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
enum Command {
    /// Compile a Rue source file.
    Build {
        /// The source file to compile.
        file: String,

        /// A list of parameters to run the compiled program with.
        #[clap(long, short = 'r')]
        run: Option<Option<String>>,

        /// Whether to exclude the standard library.
        #[clap(long, short = 'n')]
        no_std: bool,
    },

    /// Check a Rue source file for errors.
    Check {
        /// The source file to check.
        file: String,

        /// Whether to exclude the standard library.
        #[clap(long, short = 'n')]
        no_std: bool,
    },
}

fn main() {
    match Command::parse() {
        Command::Build { file, run, no_std } => build(file, true, &run, no_std),
        Command::Check { file, no_std } => build(file, false, &None, no_std),
    }
}

fn build(file: String, should_compile: bool, run: &Option<Option<String>>, no_std: bool) {
    let source = fs::read_to_string(file).expect("could not read source file");
    let (ast, errors) = parse(&source);

    for error in &errors {
        let LineCol { line, col } = line_col(&source, error.span().start);
        let line = line + 1;
        let col = col + 1;

        eprintln!("Error: {} ({line}:{col})", error.kind());
    }

    let mut allocator = Allocator::new();
    let output = compile_raw(
        &mut allocator,
        &ast,
        should_compile && errors.is_empty(),
        !no_std,
    );

    if print_diagnostics(&source, &output.diagnostics) {
        return;
    }

    if should_compile {
        let bytes = node_to_bytes(&allocator, output.node_ptr).unwrap();
        println!("{}", hex::encode(bytes));
    } else {
        println!("No errors found.");
    }

    if let Some(run) = run {
        let environment = run.as_ref().map_or(NodePtr::NIL, |run| {
            parse_clvm(&mut allocator, run).expect("could not parse input")
        });

        match run_clvm(&mut allocator, output.node_ptr, environment, u64::MAX) {
            Ok((result, cost)) => {
                eprintln!("Result: {}", stringify_clvm(&allocator, result).unwrap());
                eprintln!("Cost: {cost}");
            }
            Err(error) => {
                eprintln!("Error: {}", stringify_clvm(&allocator, error.0).unwrap());
            }
        }
    }
}

fn print_diagnostics(source: &str, diagnostics: &[Diagnostic]) -> bool {
    let mut has_error = false;

    for error in diagnostics {
        let LineCol { line, col } = line_col(source, error.span().start);
        let line = line + 1;
        let col = col + 1;

        match error.kind() {
            DiagnosticKind::Error(kind) => {
                has_error = true;
                eprintln!("Error: {kind} ({line}:{col})");
            }
            DiagnosticKind::Warning(kind) => {
                eprintln!("Warning: {kind} ({line}:{col})");
            }
        }
    }

    has_error
}
