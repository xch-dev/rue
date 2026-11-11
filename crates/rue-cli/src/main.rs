use std::{
    collections::{HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
    process,
};

use anyhow::Result;
use clap::{Arg, ArgAction, ArgMatches, Args, FromArgMatches, Parser, value_parser};
use clvm_utils::tree_hash;
use clvmr::{
    Allocator, NodePtr, SExp,
    error::EvalErr,
    run_program,
    serde::{node_from_bytes, node_to_bytes},
};
use colored::Colorize;
use rue_clvm::{assemble, disassemble_program, disassemble_value};
use rue_compiler::{Compiler, FileTree, normalize_path};
use rue_diagnostic::{
    Diagnostic, DiagnosticKind, DiagnosticSeverity, ForbiddenBy, LineCol, Lint, LintLevel,
    LintLevels, SourceKind,
};
use rue_lir::{CLVM_FLAGS, DebugDialect};
use rue_options::{Manifest, Project, find_project};

mod fmt;

use fmt::*;

#[derive(Debug, Parser)]
pub enum Command {
    Init(InitArgs),
    Build(BuildArgs),
    Test(TestArgs),
    Debug(DebugArgs),
    Fmt(FmtArgs),
    /// List every lint, along with its default level
    Lints,
}

#[derive(Debug, Parser)]
pub struct InitArgs {
    path: Option<String>,
}

#[derive(Debug, Parser)]
#[allow(clippy::struct_excessive_bools)]
#[clap(disable_help_flag = true)]
pub struct BuildArgs {
    file: Option<String>,
    #[clap(short, long)]
    export: Option<String>,
    #[clap(short, long)]
    debug: bool,
    #[clap(short = 'x', long)]
    hex: bool,
    #[clap(short, long)]
    hash: bool,
    #[clap(short, long)]
    split: bool,
    #[clap(short, long)]
    all: bool,
    #[clap(flatten)]
    lints: LintArgs,
    /// Print help
    #[clap(long, action = clap::ArgAction::Help)]
    help: Option<bool>,
}

#[derive(Debug, Parser)]
pub struct TestArgs {
    file: Option<String>,
    #[clap(flatten)]
    lints: LintArgs,
}

/// Overrides for lint levels, which take precedence over the `[lints]` section in `Rue.toml`.
/// If a lint is passed more than once, the last flag wins, unless the lint is already forbidden.
#[derive(Debug, Clone, Default)]
pub struct LintArgs {
    overrides: Vec<(Lint, LintLevel)>,
}

const LINT_FLAGS: [(&str, char, LintLevel, &str); 4] = [
    (
        "allow",
        'A',
        LintLevel::Allow,
        "Allow a lint, so that it isn't reported (can be repeated)",
    ),
    (
        "warn",
        'W',
        LintLevel::Warn,
        "Report a lint as a warning (can be repeated)",
    ),
    (
        "deny",
        'D',
        LintLevel::Deny,
        "Report a lint as an error, which fails compilation (can be repeated)",
    ),
    (
        "forbid",
        'F',
        LintLevel::Forbid,
        "Report a lint as an error, and prevent it from being set to any other level (can be repeated)",
    ),
];

impl LintArgs {
    /// Applies the overrides, in the order they were passed. Overrides of lints that are already
    /// forbidden (by `Rue.toml` or an earlier flag) have no effect, and are returned instead.
    fn apply(&self, levels: &mut LintLevels) -> Vec<(Lint, LintLevel, ForbiddenBy)> {
        let manifest = *levels;
        let mut rejected = Vec::new();

        for &(lint, level) in &self.overrides {
            if !levels.try_set(lint, level) {
                let forbidden_by = if manifest.get(lint) == LintLevel::Forbid {
                    ForbiddenBy::Manifest
                } else {
                    ForbiddenBy::Flag
                };

                rejected.push((lint, level, forbidden_by));
            }
        }

        rejected
    }
}

// Implemented by hand rather than derived, since the derive doesn't keep track of the order of
// values across different flags.
impl Args for LintArgs {
    fn augment_args(mut cmd: clap::Command) -> clap::Command {
        for (name, short, _, help) in LINT_FLAGS {
            cmd = cmd.arg(
                Arg::new(name)
                    .short(short)
                    .long(name)
                    .value_name("LINT")
                    .value_parser(value_parser!(Lint))
                    .action(ArgAction::Append)
                    .help(help),
            );
        }

        cmd
    }

    fn augment_args_for_update(cmd: clap::Command) -> clap::Command {
        Self::augment_args(cmd)
    }
}

impl FromArgMatches for LintArgs {
    fn from_arg_matches(matches: &ArgMatches) -> Result<Self, clap::Error> {
        let mut overrides = Vec::new();

        for (name, _, level, _) in LINT_FLAGS {
            if let (Some(lints), Some(indices)) =
                (matches.get_many::<Lint>(name), matches.indices_of(name))
            {
                overrides.extend(
                    indices
                        .zip(lints)
                        .map(|(index, &lint)| (index, lint, level)),
                );
            }
        }

        overrides.sort_by_key(|&(index, ..)| index);

        Ok(Self {
            overrides: overrides
                .into_iter()
                .map(|(_, lint, level)| (lint, level))
                .collect(),
        })
    }

    fn update_from_arg_matches(&mut self, matches: &ArgMatches) -> Result<(), clap::Error> {
        *self = Self::from_arg_matches(matches)?;
        Ok(())
    }
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
        Command::Fmt(args) => format(&args),
        Command::Lints => {
            lints();
            Ok(())
        }
    }
}

fn lints() {
    let width = Lint::ALL
        .iter()
        .map(|lint| lint.name().len())
        .max()
        .unwrap_or_default();

    for &lint in Lint::ALL {
        println!(
            "{:width$}  {:5}  {}",
            lint.name(),
            lint.default_level(),
            lint.description()
        );
    }

    println!();
    println!("Levels:");
    println!("  allow   The lint isn't reported");
    println!("  warn    The lint is reported as a warning");
    println!("  deny    The lint is reported as an error, which fails compilation");
    println!("  forbid  Like deny, but attributes and later flags can't change the level");
}

/// Applies the CLI lint overrides to the project, and warns about unknown lints in `Rue.toml`.
/// Returns whether any errors were reported, such as an override that tries to change the level
/// of a forbidden lint.
fn configure_lints(project: &mut Project, args: &LintArgs) -> bool {
    let manifest = project
        .manifest_path
        .as_deref()
        .and_then(|path| fs::read_to_string(path).ok())
        .unwrap_or_default();

    for lint in &project.unknown_lints {
        let location = LineCol::new(&manifest, lint.span.start);

        eprintln!(
            "{}",
            format!(
                "Warning: Unknown lint `{}` at Rue.toml:{location}",
                lint.name
            )
            .yellow()
            .bold()
        );
    }

    let rejected = args.apply(&mut project.options.lints);

    for &(lint, level, forbidden_by) in &rejected {
        let kind = DiagnosticKind::ForbiddenLintOverride {
            lint,
            level,
            forbidden_by,
        };

        eprintln!("{}", format!("Error: {kind}").red().bold());
    }

    !rejected.is_empty()
}

/// Prints the diagnostics, and returns whether any of them are errors.
fn report_diagnostics(diagnostics: Vec<Diagnostic>, base_path: &Path) -> bool {
    let mut has_errors = false;

    for diagnostic in diagnostics {
        let message = diagnostic.message(base_path);
        let lint = diagnostic
            .lint()
            .map(|lint| format!("[{lint}]"))
            .unwrap_or_default();

        if diagnostic.severity == DiagnosticSeverity::Error {
            eprintln!("{}", format!("Error{lint}: {message}").red().bold());
            has_errors = true;
        } else {
            eprintln!("{}", format!("Warning{lint}: {message}").yellow().bold());
        }
    }

    has_errors
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

    let Some(mut project) = project else {
        eprintln!("{}", "No project found".red().bold());
        process::exit(1);
    };

    let lint_errors = configure_lints(&mut project, &args.lints);

    if project.manifest.as_ref().is_some_and(|manifest| {
        manifest
            .compiler
            .version
            .as_ref()
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

    // Errors in the lint flags are reported before compiling, but don't stop it, so that every
    // error is reported at once
    if report_diagnostics(ctx.take_diagnostics(), &base_path) || lint_errors {
        process::exit(1);
    }

    if args.all {
        if args.export.is_some() {
            eprintln!("{}", "Cannot use `--export` with `--all`".red().bold());
            process::exit(1);
        }

        let dist_dir = project
            .manifest
            .as_ref()
            .and_then(|manifest| manifest.compiler.dist_dir.clone())
            .map(PathBuf::from);

        if let Some(dist_dir) = &dist_dir
            && !dist_dir.exists()
        {
            fs::create_dir_all(dist_dir)?;
        }

        let mut file_names = HashSet::new();
        let mut outputs = HashMap::new();

        for file in tree.all_files() {
            let SourceKind::File(file_path) = &file.source.kind else {
                continue;
            };
            let file_path = Path::new(file_path);
            let file_name = file_path.file_name().unwrap().to_string_lossy().to_string();

            let exports = tree.exports(
                &mut ctx,
                &mut allocator,
                Some(&file.source.kind),
                None,
                &base_path,
            )?;

            let main = tree.main(
                &mut ctx,
                &mut allocator,
                &file.source.kind,
                base_path.clone(),
            )?;

            let mut exports = if main_kind
                .as_ref()
                .is_some_and(|main| main == &file.source.kind)
            {
                exports
                    .into_iter()
                    .map(|export| {
                        (
                            if dist_dir.is_some() {
                                export.name
                            } else {
                                format!("{file_name}_{}", export.name)
                            },
                            export.ptr,
                        )
                    })
                    .collect::<Vec<_>>()
            } else {
                vec![]
            };

            if let Some(main) = main {
                exports.push((
                    file_name
                        .strip_suffix(".rue")
                        .unwrap_or(&file_name)
                        .to_string(),
                    main,
                ));
            }

            for (name, ptr) in exports {
                if dist_dir.is_some() && !file_names.insert(name.clone()) {
                    eprintln!(
                        "{}",
                        format!("Duplicate export with name `{name}`, aborting")
                            .red()
                            .bold()
                    );
                    process::exit(1);
                }

                let hex = hex::encode(node_to_bytes(&allocator, ptr)?);

                let mut output = String::new();

                if args.split {
                    let chars = hex.chars().collect::<Vec<_>>();
                    let chunks = chars.chunks(64);
                    for chunk in chunks {
                        output.push_str(&chunk.iter().collect::<String>());
                        output.push('\n');
                    }
                } else {
                    output.push_str(&hex);
                    output.push('\n');
                }

                let path = if let Some(dist_dir) = &dist_dir {
                    dist_dir
                        .join(format!("{name}.rue.hex"))
                        .to_string_lossy()
                        .to_string()
                } else {
                    file_path
                        .parent()
                        .unwrap()
                        .join(format!("{name}.rue.hex"))
                        .to_string_lossy()
                        .to_string()
                };

                outputs.insert(PathBuf::from(path), output);

                if args.hash {
                    let hash = tree_hash(&allocator, ptr).to_string();

                    let path = if let Some(dist_dir) = &dist_dir {
                        dist_dir
                            .join(format!("{name}.rue.hash"))
                            .to_string_lossy()
                            .to_string()
                    } else {
                        file_path
                            .parent()
                            .unwrap()
                            .join(format!("{name}.rue.hash"))
                            .to_string_lossy()
                            .to_string()
                    };

                    outputs.insert(PathBuf::from(path), format!("{hash}\n"));
                }
            }
        }

        for (path, output) in outputs {
            fs::write(path, output)?;
        }
    } else {
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

        if args.hex {
            let hex = hex::encode(node_to_bytes(&allocator, program)?);

            if args.split {
                let chars = hex.chars().collect::<Vec<_>>();
                let chunks = chars.chunks(64);
                for chunk in chunks {
                    println!("{}", chunk.iter().collect::<String>());
                }
            } else {
                println!("{hex}");
            }
        } else if args.split {
            eprintln!("{}", "Cannot use `--split` without `--hex`".red().bold());
            process::exit(1);
        }

        if args.hex && args.hash {
            println!();
        }

        if args.hash {
            println!("{}", tree_hash(&allocator, program));
        }

        if !args.hex && !args.hash {
            println!("{}", disassemble_program(&allocator, program));
        }
    }

    Ok(())
}

#[allow(clippy::needless_pass_by_value)]
fn test(args: TestArgs) -> Result<()> {
    let mut allocator = Allocator::new();

    let search_path = &Path::new(args.file.as_deref().unwrap_or(".")).canonicalize()?;
    let project = find_project(search_path, true)?;

    let Some(mut project) = project else {
        eprintln!("{}", "No project found".red().bold());
        process::exit(1);
    };

    let lint_errors = configure_lints(&mut project, &args.lints);

    let mut ctx = Compiler::new(project.options);

    let tree = FileTree::compile_path(&mut ctx, &project.entrypoint, &mut HashMap::new())?;
    let base_path = if project.entrypoint.is_file() {
        project.entrypoint.parent().unwrap().canonicalize()?
    } else {
        project.entrypoint.canonicalize()?
    };

    // Errors in the lint flags are reported before compiling, but don't stop it, so that every
    // error is reported at once
    if report_diagnostics(ctx.take_diagnostics(), &base_path) || lint_errors {
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
            &DebugDialect::new(CLVM_FLAGS, true),
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
                                disassemble_value(&allocator, error)
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
        &DebugDialect::new(CLVM_FLAGS, true),
        program,
        solution,
        u64::MAX,
    )?
    .1;

    let output = disassemble_value(&allocator, output);

    println!("{output}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::Command;
    use clap::{CommandFactory, Parser, error::ErrorKind};
    use rue_diagnostic::{ForbiddenBy, Lint, LintLevel, LintLevels};

    #[test]
    fn cli_debug_asserts() {
        Command::command().debug_assert();
    }

    #[test]
    fn build_short_flags_combine() {
        let Ok(Command::Build(args)) = Command::try_parse_from(["rue", "build", "-sha"]) else {
            panic!("expected build command");
        };
        assert!(args.split && args.hash && args.all);
    }

    #[test]
    fn lint_flags() {
        let Ok(Command::Build(args)) = Command::try_parse_from([
            "rue",
            "build",
            "-A",
            "unused_binding",
            "--deny",
            "unused_function",
            "-W",
            "unnecessary_cast",
            "-D",
            "unused_binding",
        ]) else {
            panic!("expected build command");
        };

        let mut levels = LintLevels::default();
        levels.set(Lint::UnnecessaryCast, LintLevel::Allow);
        assert!(args.lints.apply(&mut levels).is_empty());

        assert_eq!(levels.get(Lint::UnusedFunction), LintLevel::Deny);
        assert_eq!(levels.get(Lint::UnnecessaryCast), LintLevel::Warn);
        // The last flag wins when a lint is passed more than once
        assert_eq!(levels.get(Lint::UnusedBinding), LintLevel::Deny);

        let Ok(Command::Build(args)) = Command::try_parse_from([
            "rue",
            "build",
            "-D",
            "unused_binding",
            "-A",
            "unused_binding",
        ]) else {
            panic!("expected build command");
        };

        let mut levels = LintLevels::default();
        assert!(args.lints.apply(&mut levels).is_empty());
        assert_eq!(levels.get(Lint::UnusedBinding), LintLevel::Allow);

        let Ok(Command::Test(args)) =
            Command::try_parse_from(["rue", "test", "-A", "unused_import"])
        else {
            panic!("expected test command");
        };

        let mut levels = LintLevels::default();
        assert!(args.lints.apply(&mut levels).is_empty());
        assert_eq!(levels.get(Lint::UnusedImport), LintLevel::Allow);
    }

    #[test]
    fn forbid_flag() {
        let Ok(Command::Build(args)) = Command::try_parse_from([
            "rue",
            "build",
            "-A",
            "unused_binding",
            "--forbid",
            "unused_binding",
            "-W",
            "unused_binding",
            "-F",
            "unused_function",
            "-D",
            "unused_function",
            "-F",
            "unused_function",
        ]) else {
            panic!("expected build command");
        };

        // A lint can be forbidden after another flag, but no other flag can change it afterwards
        let mut levels = LintLevels::default();
        assert_eq!(
            args.lints.apply(&mut levels),
            vec![
                (Lint::UnusedBinding, LintLevel::Warn, ForbiddenBy::Flag),
                (Lint::UnusedFunction, LintLevel::Deny, ForbiddenBy::Flag),
            ]
        );
        assert_eq!(levels.get(Lint::UnusedBinding), LintLevel::Forbid);
        assert_eq!(levels.get(Lint::UnusedFunction), LintLevel::Forbid);

        // Lints forbidden in `Rue.toml` can't be changed by flags either
        let Ok(Command::Test(args)) =
            Command::try_parse_from(["rue", "test", "-A", "unused_import", "-A", "unused_binding"])
        else {
            panic!("expected test command");
        };

        let mut levels = LintLevels::default();
        levels.set(Lint::UnusedImport, LintLevel::Forbid);
        assert_eq!(
            args.lints.apply(&mut levels),
            vec![(Lint::UnusedImport, LintLevel::Allow, ForbiddenBy::Manifest)]
        );
        assert_eq!(levels.get(Lint::UnusedImport), LintLevel::Forbid);
        assert_eq!(levels.get(Lint::UnusedBinding), LintLevel::Allow);
    }

    #[test]
    fn unknown_lint_flag() {
        let err = Command::try_parse_from(["rue", "build", "-D", "not_a_lint"]).unwrap_err();
        assert_eq!(err.kind(), ErrorKind::ValueValidation);
    }

    #[test]
    fn build_long_help() {
        let err = Command::try_parse_from(["rue", "build", "--help"]).unwrap_err();
        assert_eq!(err.kind(), ErrorKind::DisplayHelp);
    }
}
