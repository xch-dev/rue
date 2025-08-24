use std::fs;

use anyhow::Result;
use clap::Parser;
use clvm_tools_rs::classic::clvm_tools::binutils::disassemble;
use clvmr::Allocator;
use rue_compiler::compile_file;
use rue_options::CompilerOptions;

#[derive(Debug, Parser)]
pub enum Command {
    Build(BuildCommand),
}

#[derive(Debug, Parser)]
pub struct BuildCommand {
    file: String,
}

fn main() -> Result<()> {
    let args = Command::parse();

    match args {
        Command::Build(cmd) => build(cmd),
    }
}

fn build(cmd: BuildCommand) -> Result<()> {
    let source = fs::read_to_string(cmd.file)?;

    let mut allocator = Allocator::new();

    let result = compile_file(&mut allocator, &source, CompilerOptions::default())?;

    for diagnostic in result.diagnostics {
        eprintln!("{}", diagnostic.message(&source));
    }

    if let Some(program) = result.program {
        println!("{}", disassemble(&allocator, program, None));
    }

    Ok(())
}
