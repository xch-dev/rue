use std::fs;

use clap::Parser;
use clvmr::{run_program, serde::node_to_bytes, Allocator, ChiaDialect, NodePtr};
use rue_compiler::{analyze, compile, Diagnostic, DiagnosticKind};
use rue_parser::{line_col, parse, LineCol};

/// The Rue language compiler and toolchain.
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// The source file to compile.
    file: String,

    /// Whether to only analyze.
    #[clap(long, short)]
    analyze: bool,
}

fn main() {
    let args = Args::parse();

    let source = fs::read_to_string(args.file).expect("could not read source file");
    let (ast, errors) = parse(&source);

    for error in &errors {
        let LineCol { line, col } = line_col(&source, error.span().start);
        let line = line + 1;
        let col = col + 1;

        eprintln!("Error: {} ({line}:{col})", error.kind());
    }

    if args.analyze {
        let diagnostics = analyze(&ast);
        print_diagnostics(&source, &diagnostics);
    } else {
        let mut allocator = Allocator::new();
        let output = compile(&mut allocator, &ast, errors.is_empty());

        if print_diagnostics(&source, output.diagnostics()) {
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
            Ok(output) => eprintln!(
                "Serialized output: {}",
                hex::encode(node_to_bytes(&allocator, output.1).unwrap())
            ),
            Err(error) => eprintln!("error: {error:?}"),
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
