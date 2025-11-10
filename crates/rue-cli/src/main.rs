use std::{collections::HashMap, path::Path, process};

use anyhow::Result;
use chialisp::classic::clvm_tools::binutils::{assemble, disassemble};
use clap::Parser;
use clvm_utils::tree_hash;
use clvmr::{
    Allocator, ENABLE_KECCAK_OPS_OUTSIDE_GUARD, MEMPOOL_MODE, NodePtr, SExp,
    error::EvalErr,
    run_program,
    serde::{node_from_bytes, node_to_bytes},
};
use colored::Colorize;
use rue_compiler::{Compiler, FileTree, normalize_path};
use rue_diagnostic::DiagnosticSeverity;
use rue_lir::DebugDialect;
use rue_options::CompilerOptions;

#[derive(Debug, Parser)]
pub enum Command {
    Build(BuildArgs),
    Test(TestArgs),
    Debug(DebugArgs),
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

#[derive(Debug, Parser)]
pub struct TestArgs {
    file: String,
}

#[derive(Debug, Parser)]
pub struct DebugArgs {
    program: String,
    solution: Option<String>,
    #[clap(short = 'x', long)]
    hex: bool,
}

fn main() -> Result<()> {
    let args = Command::parse();

    match args {
        Command::Build(args) => build(args),
        Command::Test(args) => test(args),
        Command::Debug(args) => debug(args),
    }
}

fn build(args: BuildArgs) -> Result<()> {
    let mut allocator = Allocator::new();

    let mut ctx = Compiler::new(if args.debug {
        CompilerOptions::debug()
    } else {
        CompilerOptions::default()
    });

    let path = Path::new(&args.file);
    let tree = FileTree::compile_path(
        &mut ctx,
        if path.is_file()
            && let Some(parent) = path.parent()
        {
            parent
        } else {
            path
        },
        &mut HashMap::new(),
    )?;
    let path = normalize_path(path)?;

    let mut codegen = true;

    for diagnostic in ctx.take_diagnostics() {
        let message = diagnostic.message();
        let severity = diagnostic.kind.severity();

        if severity == DiagnosticSeverity::Error {
            eprintln!("{}", format!("Error: {message}").red().bold());
            codegen = false;
        } else {
            eprintln!("{}", format!("Warning: {message}").yellow().bold());
        }
    }

    if !codegen {
        process::exit(1);
    }

    let program = if let Some(export) = args.export {
        let Some(program) = tree
            .exports(&mut ctx, &mut allocator, &path, Some(&export))?
            .into_iter()
            .next()
        else {
            eprintln!("{}", format!("Export `{export}` not found").red().bold());
            process::exit(1);
        };

        program.ptr
    } else if let Some(ptr) = tree.main(&mut ctx, &mut allocator, &path)? {
        ptr
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

#[allow(clippy::needless_pass_by_value)]
fn test(args: TestArgs) -> Result<()> {
    let mut allocator = Allocator::new();
    let mut ctx = Compiler::new(CompilerOptions::debug());

    let path = Path::new(&args.file);
    let tree = FileTree::compile_path(&mut ctx, path, &mut HashMap::new())?;

    let mut codegen = true;

    for diagnostic in ctx.take_diagnostics() {
        let message = diagnostic.message();
        let severity = diagnostic.kind.severity();

        if severity == DiagnosticSeverity::Error {
            eprintln!("{}", format!("Error: {message}").red().bold());
            codegen = false;
        } else {
            eprintln!("{}", format!("Warning: {message}").yellow().bold());
        }
    }

    if !codegen {
        process::exit(1);
    }

    let tests = tree.tests(&mut ctx, &mut allocator, None, None)?;

    let len = tests.len();

    let mut failed = false;

    for (i, test) in tests.iter().enumerate() {
        let Some(name) = &test.name else {
            continue;
        };

        println!(
            "{}",
            format!("Running test `{}` ({}/{})", name, i + 1, len)
                .cyan()
                .bold()
        );

        match run_program(
            &mut allocator,
            &DebugDialect::new(ENABLE_KECCAK_OPS_OUTSIDE_GUARD | MEMPOOL_MODE, true),
            test.ptr,
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

fn debug(args: DebugArgs) -> Result<()> {
    let mut allocator = Allocator::new();

    let program = if args.hex {
        let bytes = hex::decode(args.program)?;
        node_from_bytes(&mut allocator, &bytes)?
    } else {
        assemble(&mut allocator, &args.program)?
    };

    let solution = if let Some(solution) = args.solution {
        if args.hex {
            let bytes = hex::decode(solution)?;
            node_from_bytes(&mut allocator, &bytes)?
        } else {
            assemble(&mut allocator, &solution)?
        }
    } else {
        NodePtr::NIL
    };

    let output = run_program(
        &mut allocator,
        &DebugDialect::new(ENABLE_KECCAK_OPS_OUTSIDE_GUARD | MEMPOOL_MODE, true),
        program,
        solution,
        u64::MAX,
    )?
    .1;

    let output = disassemble(&allocator, output, None);

    println!("{output}");

    Ok(())
}
