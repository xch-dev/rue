use std::fs;

use clap::Parser;
use clvmr::{run_program, serde::node_to_bytes, Allocator, ChiaDialect, NodePtr};
use rue_compiler::compile;
use rue_parser::parse;

/// The Rue language compiler and toolchain.
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// The source file to compile.
    file: String,
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

fn main() {
    let args = Args::parse();

    let source = fs::read_to_string(args.file).expect("could not read source file");
    let (ast, errors) = parse(&source);

    for error in errors {
        let (line, column) = line_col(&source, error.span().start);

        eprintln!("{} at {line}:{column}", error.kind());
    }

    let mut allocator = Allocator::new();
    let output = compile(&mut allocator, ast);

    if !output.diagnostics().is_empty() {
        for error in output.diagnostics() {
            let (line, column) = line_col(&source, error.span().start);
            eprintln!("{} at {line}:{column}", error.info());
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
