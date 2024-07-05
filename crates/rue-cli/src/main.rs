use std::fs;

use clap::Parser;
use clvmr::{serde::node_to_bytes, Allocator};
use rue_compiler::{compile, Diagnostic, DiagnosticKind};
use rue_parser::{line_col, parse, LineCol};

/// CLI tools for working with the Rue compiler.
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
enum Command {
    /// Compile a Rue source file.
    Build {
        /// The source file to compile.
        file: String,
    },

    /// Check a Rue source file for errors.
    Check {
        /// The source file to check.
        file: String,
    },
}

fn main() {
    match Command::parse() {
        Command::Build { file } => build(file, true),
        Command::Check { file } => build(file, false),
    }
}

fn build(file: String, should_compile: bool) {
    let source = fs::read_to_string(file).expect("could not read source file");
    let (ast, errors) = parse(&source);

    for error in &errors {
        let LineCol { line, col } = line_col(&source, error.span().start);
        let line = line + 1;
        let col = col + 1;

        eprintln!("Error: {} ({line}:{col})", error.kind());
    }

    let mut allocator = Allocator::new();
    let output = compile(&mut allocator, &ast, should_compile && errors.is_empty());

    if print_diagnostics(&source, &output.diagnostics) {
        return;
    }

    if should_compile {
        let bytes = node_to_bytes(&allocator, output.node_ptr).unwrap();
        println!("{}", hex::encode(bytes));
    } else {
        println!("No errors found.");
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
