use std::{fs, process, sync::Arc};

use anyhow::Result;
use clap::Parser;
use clvm_tools_rs::classic::clvm_tools::binutils::disassemble;
use clvm_utils::tree_hash;
use clvmr::{
    Allocator, ChiaDialect, ENABLE_KECCAK_OPS_OUTSIDE_GUARD, MEMPOOL_MODE, NodePtr, SExp,
    error::EvalErr, run_program, serde::node_to_bytes,
};
use colored::Colorize;
use rue_compiler::compile_file;
use rue_diagnostic::{DiagnosticSeverity, Source, SourceKind};
use rue_options::CompilerOptions;

#[derive(Debug, Parser)]
pub enum Command {
    Build(BuildArgs),
    Test { file: String },
}

#[derive(Debug, Parser)]
pub struct BuildArgs {
    file: String,
    #[clap(short, long)]
    export: Option<String>,
    #[clap(short, long)]
    debug: bool,
    #[clap(short = 'x', long)]
    hex: bool,
    #[clap(long)]
    hash: bool,
}

fn main() -> Result<()> {
    let args = Command::parse();

    match args {
        Command::Build(args) => build(args),
        Command::Test { file } => test(file),
    }
}

fn build(args: BuildArgs) -> Result<()> {
    let source = fs::read_to_string(&args.file)?;

    let mut allocator = Allocator::new();

    let result = compile_file(
        &mut allocator,
        Source::new(Arc::from(source), SourceKind::File(args.file)),
        if args.debug {
            CompilerOptions::debug()
        } else {
            CompilerOptions::default()
        },
    )?;

    println!("{:?}", result.syntax_map);

    for diagnostic in &result.diagnostics {
        let message = diagnostic.message();
        let severity = diagnostic.kind.severity();

        if severity == DiagnosticSeverity::Error {
            eprintln!("{}", format!("Error: {message}").red().bold());
        } else {
            eprintln!("{}", format!("Warning: {message}").yellow().bold());
        }
    }

    if result
        .diagnostics
        .iter()
        .any(|d| d.kind.severity() == DiagnosticSeverity::Error)
    {
        process::exit(1);
    }

    let program = if let Some(export) = args.export {
        let Some(program) = result.exports.get(&export).copied() else {
            eprintln!("{}", format!("Export `{export}` not found").red().bold());
            process::exit(1);
        };

        program
    } else if let Some(program) = result.main {
        program
    } else if result.exports.is_empty() {
        eprintln!(
            "{}",
            "No `main` function or exported functions found"
                .red()
                .bold()
        );
        process::exit(1);
    } else {
        eprintln!(
            "{}",
            "No `main` function found (you can specify an entrypoint with `--export`)"
                .red()
                .bold()
        );
        process::exit(1);
    };

    if args.hex && args.hash {
        eprintln!("{}", "Cannot use both `--hex` and `--hash`".red().bold());
        process::exit(1);
    }

    if args.hex {
        println!("{}", hex::encode(node_to_bytes(&allocator, program)?));
    } else if args.hash {
        println!("0x{}", tree_hash(&allocator, program));
    } else {
        println!("{}", disassemble(&allocator, program, None));
    }

    Ok(())
}

fn test(file: String) -> Result<()> {
    let source = fs::read_to_string(&file)?;

    let mut allocator = Allocator::new();

    let result = compile_file(
        &mut allocator,
        Source::new(Arc::from(source), SourceKind::File(file)),
        CompilerOptions::debug(),
    )?;

    for diagnostic in &result.diagnostics {
        let message = diagnostic.message();
        let severity = diagnostic.kind.severity();

        if severity == DiagnosticSeverity::Error {
            eprintln!("{}", format!("Error: {message}").red().bold());
        } else {
            eprintln!("{}", format!("Warning: {message}").yellow().bold());
        }
    }

    if result
        .diagnostics
        .iter()
        .any(|d| d.kind.severity() == DiagnosticSeverity::Error)
    {
        process::exit(1);
    }

    let len = result.tests.len();

    let mut failed = false;

    for (i, test) in result.tests.iter().enumerate() {
        println!(
            "{}",
            format!("Running test `{}` ({}/{})", test.name, i + 1, len)
                .cyan()
                .bold()
        );

        match run_program(
            &mut allocator,
            &ChiaDialect::new(ENABLE_KECCAK_OPS_OUTSIDE_GUARD | MEMPOOL_MODE),
            test.program,
            NodePtr::NIL,
            u64::MAX,
        ) {
            Ok(output) => {
                if let SExp::Atom = allocator.sexp(output.1)
                    && allocator.atom(output.1).is_empty()
                {
                    continue;
                }
                eprintln!("{}", "Test failed due to non-nil output".red().bold());
                failed = true;
            }
            Err(error) => {
                match error {
                    EvalErr::Raise(error) => {
                        eprintln!(
                            "{}",
                            format!(
                                "Test failed due to raise: {}",
                                disassemble(&allocator, error, None)
                            )
                            .red()
                            .bold()
                        );
                    }
                    _ => {
                        eprintln!(
                            "{}",
                            format!("Test failed due to error: {error}").red().bold()
                        );
                    }
                }
                failed = true;
            }
        }
    }

    if failed {
        process::exit(1);
    }

    Ok(())
}
