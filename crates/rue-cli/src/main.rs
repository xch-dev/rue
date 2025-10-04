use std::{fs, sync::Arc};

use anyhow::Result;
use clap::Parser;
use clvm_tools_rs::classic::clvm_tools::binutils::disassemble;
use clvmr::{
    Allocator, ChiaDialect, ENABLE_KECCAK_OPS_OUTSIDE_GUARD, MEMPOOL_MODE, NodePtr, SExp,
    error::EvalErr, run_program,
};
use rue_compiler::compile_file;
use rue_diagnostic::{Source, SourceKind};
use rue_options::CompilerOptions;

#[derive(Debug, Parser)]
pub enum Command {
    Build { file: String },
    Test { file: String },
}

fn main() -> Result<()> {
    let args = Command::parse();

    match args {
        Command::Build { file } => build(file),
        Command::Test { file } => test(file),
    }
}

fn build(file: String) -> Result<()> {
    let source = fs::read_to_string(&file)?;

    let mut allocator = Allocator::new();

    let result = compile_file(
        &mut allocator,
        Source::new(Arc::from(source), SourceKind::File(file)),
        CompilerOptions::default(),
    )?;

    for diagnostic in result.diagnostics {
        eprintln!("{}", diagnostic.message());
    }

    if let Some(program) = result.program {
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

    for diagnostic in result.diagnostics {
        eprintln!("{}", diagnostic.message());
    }

    let len = result.tests.len();

    for (i, test) in result.tests.iter().enumerate() {
        println!("Running test {} {}/{}", test.name, i + 1, len);

        match run_program(
            &mut allocator,
            &ChiaDialect::new(ENABLE_KECCAK_OPS_OUTSIDE_GUARD | MEMPOOL_MODE),
            test.program,
            NodePtr::NIL,
            100_000_000,
        ) {
            Ok(output) => {
                if let SExp::Atom = allocator.sexp(output.1)
                    && allocator.atom(output.1).is_empty()
                {
                    continue;
                }
                eprintln!("Test failed due to non-nil output");
            }
            Err(error) => match error {
                EvalErr::Raise(error) => {
                    eprintln!(
                        "Test failed due to raise: {}",
                        disassemble(&allocator, error, None)
                    );
                }
                _ => {
                    eprintln!("Test failed due to error: {error}");
                }
            },
        }
    }

    Ok(())
}
