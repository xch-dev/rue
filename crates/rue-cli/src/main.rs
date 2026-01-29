use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
    process,
};

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
use rue_options::{Manifest, find_project};

#[derive(Debug, Parser)]
pub enum Command {
    Init(InitArgs),
    Build(BuildArgs),
    Test(TestArgs),
    Debug(DebugArgs),
}

#[derive(Debug, Parser)]
pub struct InitArgs {
    path: Option<String>,
}

#[derive(Debug, Parser)]
pub struct BuildArgs {
    file: Option<String>,
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
    file: Option<String>,
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
        Command::Init(args) => init(args),
        Command::Build(args) => build(args),
        Command::Test(args) => test(args),
        Command::Debug(args) => debug(args),
    }
}

fn init(args: InitArgs) -> Result<()> {
    let path = PathBuf::from(args.path.unwrap_or_else(|| ".".to_string()));
    let manifest = Manifest::default();
    let entrypoint_dir = path.join(&manifest.compiler.entrypoint);
    let manifest_path = path.join("Rue.toml");
    let main_path = entrypoint_dir.join("main.rue");

    if manifest_path.try_exists()? {
        eprintln!("{}", "Rue.toml already exists".red().bold());
        process::exit(1);
    }

    if main_path.try_exists()? {
        eprintln!("{}", "main.rue already exists".red().bold());
        process::exit(1);
    }

    fs::create_dir_all(entrypoint_dir)?;
    fs::write(manifest_path, toml::to_string(&manifest)?)?;
    fs::write(
        main_path,
        "fn main() -> String {\n    \"Hello, world!\"\n}\n",
    )?;

    Ok(())
}

fn build(args: BuildArgs) -> Result<()> {
    let mut allocator = Allocator::new();

    let search_path = &Path::new(args.file.as_deref().unwrap_or(".")).canonicalize()?;
    let project = find_project(search_path, args.debug)?;

    let Some(project) = project else {
        eprintln!("{}", "No project found".red().bold());
        process::exit(1);
    };

    if project.manifest.is_some_and(|manifest| {
        manifest
            .compiler
            .version
            .is_some_and(|version| version != env!("CARGO_PKG_VERSION"))
    }) {
        eprintln!("{}", "Project version mismatch".red().bold());
        process::exit(1);
    }

    let file_kind = args
        .file
        .map(|file| normalize_path(Path::new(&file)))
        .transpose()?;

    let main_kind = if let Some(file_kind) = &file_kind {
        Some(file_kind.clone())
    } else if project.entrypoint.join("main.rue").exists() {
        Some(normalize_path(&project.entrypoint.join("main.rue"))?)
    } else {
        None
    };

    let mut ctx = Compiler::new(project.options);

    let tree = FileTree::compile_path(&mut ctx, &project.entrypoint, &mut HashMap::new())?;
    let base_path = if project.entrypoint.is_file() {
        project.entrypoint.parent().unwrap().canonicalize()?
    } else {
        project.entrypoint.canonicalize()?
    };

    let mut codegen = true;

    for diagnostic in ctx.take_diagnostics() {
        let message = diagnostic.message(&base_path);
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
            .exports(
                &mut ctx,
                &mut allocator,
                file_kind.as_ref(),
                Some(&export),
                &base_path,
            )?
            .into_iter()
            .next()
        else {
            eprintln!("{}", format!("Export `{export}` not found").red().bold());
            process::exit(1);
        };

        program.ptr
    } else if let Some(main_kind) = main_kind
        && let Some(ptr) = tree.main(&mut ctx, &mut allocator, &main_kind, base_path)?
    {
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
        println!("{}", hex::encode(node_to_bytes(&allocator, program)?));
        println!();
        println!("{}", tree_hash(&allocator, program));
    } else if args.hex {
        println!("{}", hex::encode(node_to_bytes(&allocator, program)?));
    } else if args.hash {
        println!("{}", tree_hash(&allocator, program));
    } else {
        println!("{}", disassemble(&allocator, program, None));
    }

    Ok(())
}

#[allow(clippy::needless_pass_by_value)]
fn test(args: TestArgs) -> Result<()> {
    let mut allocator = Allocator::new();

    let search_path = &Path::new(args.file.as_deref().unwrap_or(".")).canonicalize()?;
    let project = find_project(search_path, true)?;

    let Some(project) = project else {
        eprintln!("{}", "No project found".red().bold());
        process::exit(1);
    };

    let mut ctx = Compiler::new(project.options);

    let tree = FileTree::compile_path(&mut ctx, &project.entrypoint, &mut HashMap::new())?;
    let base_path = if project.entrypoint.is_file() {
        project.entrypoint.parent().unwrap().canonicalize()?
    } else {
        project.entrypoint.canonicalize()?
    };

    let mut codegen = true;

    for diagnostic in ctx.take_diagnostics() {
        let message = diagnostic.message(&base_path);
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

    let tests = tree.tests(&mut ctx, &mut allocator, None, None, &base_path, false)?;

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
