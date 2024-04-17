use std::fs;

use clap::Parser;
use clvmr::{run_program, serde::node_to_bytes, Allocator, ChiaDialect, NodePtr};
use rue_compiler::{compile, DiagnosticKind};
use rue_parser::{line_col, parse, LineCol};

/// The Rue language compiler and toolchain.
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// The source file to compile.
    file: String,
}

fn main() {
    let args = Args::parse();

    let source = fs::read_to_string(args.file).expect("could not read source file");
    let (ast, errors) = parse(&source);

    for error in &errors {
        let LineCol { line, col } = line_col(&source, error.span().start);
        let line = line + 1;
        let col = col + 1;

        eprintln!("{} at {line}:{col}", error.kind());
    }

    let mut allocator = Allocator::new();
    let output = compile(&mut allocator, ast, errors.is_empty());

    if !output.diagnostics().is_empty() {
        for error in output.diagnostics() {
            let LineCol { line, col } = line_col(&source, error.span().start);
            let line = line + 1;
            let col = col + 1;

            match error.kind() {
                DiagnosticKind::Error(kind) => {
                    eprintln!("Error: {} at {line}:{col}", kind)
                }
                DiagnosticKind::Warning(kind) => {
                    eprintln!("Warning: {} at {line}:{col}", kind)
                }
            }
        }
        return;
    }

    let bytes = node_to_bytes(&allocator, output.node_ptr()).unwrap();
    println!("{}", hex::encode(bytes));
    match run_program(
        &mut allocator,
        &ChiaDialect::new(0),
        output.node_ptr(),
        NodePtr::NIL,
        0,
    ) {
        Ok(output) => println!(
            "Serialized output: {}",
            hex::encode(node_to_bytes(&allocator, output.1).unwrap())
        ),
        Err(error) => eprintln!("Error: {:?}", error),
    }
}
