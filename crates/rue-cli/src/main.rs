use std::{
    collections::{HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
    process,
};

use anyhow::{Context, Result};
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
use rue_compiler::{CompiledExport, Compiler, FileTree, normalize_path};
use rue_diagnostic::{
    Diagnostic, DiagnosticSeverity, ForbiddenBy, LineIndex, Lint, LintGroup, LintLevel, LintLevels,
    LintName, SourceKind,
};
use rue_lir::{CLVM_FLAGS, DebugDialect};
use rue_options::{Manifest, Project, find_project};

mod fmt;
mod trace;

use fmt::*;
use trace::{Sidecars, run_with_trace};

#[derive(Debug, Parser)]
pub enum Command {
    Init(InitArgs),
    Build(BuildArgs),
    Test(TestArgs),
    Debug(DebugArgs),
    Fmt(FmtArgs),
    /// List every lint and lint group, along with each lint's default level
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
    /// Write a debug metadata sidecar for each program, which debuggers use to show where a failing
    /// program was in the source. With `--all`, it's written next to each program, and otherwise
    /// to the path given (`--debug-metadata=<PATH>`), or next to the source file. The programs are
    /// the same as without it.
    #[clap(long, value_name = "PATH", num_args = 0..=1, require_equals = true)]
    // Clap's way of a flag with an optional value: whether it's passed, and the value if it's given
    #[allow(clippy::option_option)]
    debug_metadata: Option<Option<PathBuf>>,
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
    #[clap(flatten)]
    cost: CostArgs,
}

/// The default cost limit for running a program, which is the same as Chia's block cost limit.
const DEFAULT_MAX_COST: u64 = 11_000_000_000;

#[derive(Debug, Clone, Copy, Args)]
pub struct CostArgs {
    /// The maximum cost of running a program, after which it fails
    #[clap(
        long,
        value_name = "COST",
        default_value_t = DEFAULT_MAX_COST,
        // clvmr treats a limit of 0 as no limit at all
        value_parser = value_parser!(u64).range(1..)
    )]
    max_cost: u64,
}

impl CostArgs {
    /// Describes why a program failed, and how to raise the limit if it ran out of cost.
    fn describe_error(self, error: &EvalErr) -> String {
        match error {
            EvalErr::CostExceeded => format!(
                "{error} (the cost limit is {}, which `--max-cost` can change)",
                self.max_cost
            ),
            _ => error.to_string(),
        }
    }
}

/// Overrides for lint levels, which take precedence over the `[lints]` section in `Rue.toml`.
/// A lint's own flag takes precedence over the flag for any group it's in, unless the group is
/// forbidden. Otherwise, if a lint is passed more than once, the last flag wins, unless the lint is
/// already forbidden.
#[derive(Debug, Clone, Default)]
pub struct LintArgs {
    overrides: Vec<(LintName, LintLevel)>,
}

const LINT_FLAGS: [(&str, char, LintLevel, &str); 4] = [
    (
        "allow",
        'A',
        LintLevel::Allow,
        "Allow a lint or lint group, so that it isn't reported (can be repeated)",
    ),
    (
        "warn",
        'W',
        LintLevel::Warn,
        "Report a lint or lint group as a warning (can be repeated)",
    ),
    (
        "deny",
        'D',
        LintLevel::Deny,
        "Report a lint or lint group as an error, which fails compilation (can be repeated)",
    ),
    (
        "forbid",
        'F',
        LintLevel::Forbid,
        "Report a lint or lint group as an error, and prevent it from being set to any other level (can be repeated)",
    ),
];

impl LintArgs {
    /// Applies the overrides, in the order they were passed. A group's flag doesn't change the lints
    /// in it that have their own flag, unless it forbids them. Overrides of lints that are already
    /// forbidden (by `Rue.toml` or an earlier flag) have no effect, and are returned instead, along
    /// with the first forbidden lint for each flag.
    fn apply(&self, levels: &mut LintLevels) -> Vec<(LintName, Lint, LintLevel, ForbiddenBy)> {
        let manifest = *levels;
        let mut rejected = Vec::new();

        let named = self
            .overrides
            .iter()
            .filter_map(|&(name, _)| match name {
                LintName::Lint(lint) => Some(lint),
                LintName::Group(_) => None,
            })
            .collect::<HashSet<_>>();

        for &(name, level) in &self.overrides {
            let mut forbidden = None;

            for &lint in name.lints() {
                if matches!(name, LintName::Group(_))
                    && level != LintLevel::Forbid
                    && named.contains(&lint)
                {
                    continue;
                }

                if !levels.try_set(lint, level) {
                    let forbidden_by = if manifest.get(lint) == LintLevel::Forbid {
                        ForbiddenBy::Manifest
                    } else {
                        ForbiddenBy::Flag
                    };

                    forbidden.get_or_insert((name, lint, level, forbidden_by));
                }
            }

            rejected.extend(forbidden);
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
                    .value_parser(value_parser!(LintName))
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
                (matches.get_many::<LintName>(name), matches.indices_of(name))
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
    /// A debug metadata sidecar (from `rue build --debug-metadata`), which is used to show where in
    /// the source the program was if it fails (can be repeated)
    #[clap(long, value_name = "PATH")]
    debug_metadata: Vec<PathBuf>,
    #[clap(flatten)]
    cost: CostArgs,
}

fn main() -> Result<()> {
    let args = Command::parse();

    match args {
        Command::Init(args) => init(args),
        Command::Build(args) => build(args),
        Command::Test(args) => test(args),
        Command::Debug(args) => {
            println!("{}", debug(args)?);
            Ok(())
        }
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
        .map(|lint| lint.name())
        .chain(LintGroup::ALL.iter().map(|group| group.name()))
        .map(str::len)
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
    println!();
    println!("Groups:");

    for &group in LintGroup::ALL {
        println!("{:width$}  {}", group.name(), group.description());

        for &lint in group.lints() {
            println!("{:width$}  - {lint}", "");
        }
    }
}

/// Applies the CLI lint overrides to the project, and reports problems with the lints in
/// `Rue.toml`. Returns whether any errors were reported, such as an override that tries to change
/// the level of a forbidden lint.
fn configure_lints(project: &mut Project, args: &LintArgs) -> bool {
    let manifest = project
        .manifest_path
        .as_deref()
        .and_then(|path| fs::read_to_string(path).ok())
        .unwrap_or_default();
    let lines = LineIndex::new(&manifest);

    for lint in &project.forbidden_lints {
        let location = lines.line_col(&manifest, lint.span.start);

        eprintln!(
            "{}",
            format!("Error: {} at Rue.toml:{location}", lint.kind())
                .red()
                .bold()
        );
    }

    let rejected = args.apply(&mut project.options.lints);

    for &(name, lint, level, forbidden_by) in &rejected {
        let kind = name.forbidden_override(lint, level, forbidden_by);
        eprintln!("{}", format!("Error: {kind}").red().bold());
    }

    // Unknown names are reported by the `unknown_lint` lint, at the level set by `Rue.toml` and the
    // command line
    let unknown_severity = project.options.lints.get(Lint::UnknownLint).severity();

    if let Some(severity) = unknown_severity {
        for lint in &project.unknown_lints {
            let location = lines.line_col(&manifest, lint.span.start);
            let message = format!(
                "[{}]: Unknown lint `{}` at Rue.toml:{location}",
                Lint::UnknownLint,
                lint.name
            );

            if severity == DiagnosticSeverity::Error {
                eprintln!("{}", format!("Error{message}").red().bold());
            } else {
                eprintln!("{}", format!("Warning{message}").yellow().bold());
            }
        }
    }

    let denied_unknown =
        unknown_severity == Some(DiagnosticSeverity::Error) && !project.unknown_lints.is_empty();

    !rejected.is_empty() || !project.forbidden_lints.is_empty() || denied_unknown
}

/// Prints the diagnostics, and returns whether any of them are errors.
fn report_diagnostics(diagnostics: Vec<Diagnostic>, base_path: &Path) -> bool {
    let mut has_errors = false;

    for diagnostic in diagnostics {
        let message = diagnostic.full_message(base_path);
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

    let debug_metadata = args.debug_metadata.is_some();

    if args.all {
        if args.export.is_some() {
            eprintln!("{}", "Cannot use `--export` with `--all`".red().bold());
            process::exit(1);
        }

        if let Some(Some(_)) = args.debug_metadata {
            eprintln!(
                "{}",
                "Cannot give `--debug-metadata` a path with `--all`, since each program has its own"
                    .red()
                    .bold()
            );
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

            let is_main = main_kind
                .as_ref()
                .is_some_and(|main| main == &file.source.kind);

            // Only the exports of the main file are written, so the others don't need sidecars
            let exports = compile_exports(
                &tree,
                &mut ctx,
                &mut allocator,
                Some(&file.source.kind),
                None,
                &base_path,
                debug_metadata && is_main,
            )?;

            let main = compile_main(
                &tree,
                &mut ctx,
                &mut allocator,
                &file.source.kind,
                &base_path,
                debug_metadata,
            )?;

            let mut exports = if is_main {
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
                            export.debug_metadata,
                        )
                    })
                    .collect::<Vec<_>>()
            } else {
                vec![]
            };

            if let Some((main, metadata)) = main {
                exports.push((
                    file_name
                        .strip_suffix(".rue")
                        .unwrap_or(&file_name)
                        .to_string(),
                    main,
                    metadata,
                ));
            }

            for (name, ptr, metadata) in exports {
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

                if let Some(metadata) = metadata {
                    outputs.insert(PathBuf::from(debug_metadata_path(&path)), metadata);
                }

                outputs.insert(PathBuf::from(path), output.into_bytes());

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

                    outputs.insert(PathBuf::from(path), format!("{hash}\n").into_bytes());
                }
            }
        }

        for (path, output) in outputs {
            fs::write(path, output)?;
        }
    } else {
        let (program, metadata) = if let Some(export) = &args.export {
            let exports = compile_exports(
                &tree,
                &mut ctx,
                &mut allocator,
                file_kind.as_ref(),
                Some(export),
                &base_path,
                debug_metadata,
            )?;

            let Some(program) = exports.into_iter().next() else {
                eprintln!("{}", format!("Export `{export}` not found").red().bold());
                process::exit(1);
            };

            (program.ptr, program.debug_metadata)
        } else if let Some(main_kind) = &main_kind
            && let Some(program) = compile_main(
                &tree,
                &mut ctx,
                &mut allocator,
                main_kind,
                &base_path,
                debug_metadata,
            )?
        {
            program
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

        if let Some(metadata) = metadata {
            let path = args.debug_metadata.flatten().unwrap_or_else(|| {
                single_debug_metadata_path(
                    file_kind.as_ref().or(main_kind.as_ref()),
                    args.export.as_deref(),
                    &base_path,
                )
            });

            fs::write(path, metadata)?;
        }
    }

    Ok(())
}

/// Compiles `main` in the file, along with its debug metadata sidecar if it's requested.
fn compile_main(
    tree: &FileTree,
    ctx: &mut Compiler,
    allocator: &mut Allocator,
    path: &SourceKind,
    base_path: &Path,
    debug_metadata: bool,
) -> Result<Option<(NodePtr, Option<Vec<u8>>)>> {
    Ok(if debug_metadata {
        tree.main_with_debug_metadata(ctx, allocator, path, base_path.to_path_buf())?
            .map(|(ptr, metadata)| (ptr, Some(metadata)))
    } else {
        tree.main(ctx, allocator, path, base_path.to_path_buf())?
            .map(|ptr| (ptr, None))
    })
}

/// Compiles the exports (of the file, or of every file), along with their debug metadata sidecars
/// if they're requested.
fn compile_exports(
    tree: &FileTree,
    ctx: &mut Compiler,
    allocator: &mut Allocator,
    path: Option<&SourceKind>,
    export: Option<&str>,
    base_path: &Path,
    debug_metadata: bool,
) -> Result<Vec<CompiledExport>> {
    Ok(if debug_metadata {
        tree.exports_with_debug_metadata(ctx, allocator, path, export, base_path)?
    } else {
        tree.exports(ctx, allocator, path, export, base_path)?
    })
}

/// The path of the debug metadata sidecar of a program, which is the path of the program with
/// its `.hex` extension replaced, as Chialisp names them (`foo.rue.hex` has `foo.rue.debug.clvm.bin`).
fn debug_metadata_path(program_path: &str) -> String {
    let base = program_path.strip_suffix(".hex").unwrap_or(program_path);
    format!("{base}.debug.clvm.bin")
}

/// Where the sidecar of a single program is written when no path is given: next to the file that's
/// built (the main file, if an export is looked for in every file), named like the program would
/// be with `--all` (after the export, or the file for `main`).
fn single_debug_metadata_path(
    source: Option<&SourceKind>,
    export: Option<&str>,
    base_path: &Path,
) -> PathBuf {
    let source = match source {
        Some(SourceKind::File(path)) => Some(Path::new(path)),
        _ => None,
    };

    let dir = source.and_then(Path::parent).unwrap_or(base_path);

    let name = export.map_or_else(
        || {
            source
                .and_then(Path::file_stem)
                .map_or("main".to_string(), |stem| {
                    stem.to_string_lossy().to_string()
                })
        },
        str::to_string,
    );

    dir.join(format!("{name}.rue.debug.clvm.bin"))
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

    // Each test has a sidecar, so that a failure shows where in the source it happened
    let tests =
        tree.tests_with_debug_metadata(&mut ctx, &mut allocator, None, None, &base_path, false)?;

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

        let mut sidecars = Sidecars::default();

        if let Some(metadata) = &test.debug_metadata {
            sidecars.insert(metadata)?;
        }

        if let Err(failure) = run_test(&mut allocator, test.ptr, args.cost, &sidecars)? {
            eprintln!(
                "{}",
                format!("Test failed due to {}", failure.message)
                    .red()
                    .bold()
            );

            if let Some(trace) = failure.trace {
                eprintln!("{trace}");
            }

            failed = true;
        }
    }

    if failed {
        process::exit(1);
    }

    Ok(())
}

/// Why a test failed.
#[derive(Debug)]
struct TestFailure {
    message: String,
    /// Where the test was when it failed, if it raised and has a sidecar.
    trace: Option<String>,
}

/// Runs a test, which passes if it returns nil, and otherwise returns why it failed. If the test
/// has a sidecar, a failure is shown with where in the source it happened.
fn run_test(
    allocator: &mut Allocator,
    program: NodePtr,
    cost: CostArgs,
    sidecars: &Sidecars,
) -> Result<Result<(), TestFailure>> {
    let (error, trace) = match run_with_trace(
        allocator,
        &DebugDialect::new(CLVM_FLAGS, true),
        program,
        NodePtr::NIL,
        cost.max_cost,
        sidecars,
    )? {
        Ok(output) => {
            if let SExp::Atom = allocator.sexp(output.1)
                && allocator.atom(output.1).is_empty()
            {
                return Ok(Ok(()));
            }

            return Ok(Err(TestFailure {
                message: "non-nil output".to_string(),
                trace: None,
            }));
        }
        Err(failure) => (failure.error, failure.trace),
    };

    let message = match error {
        EvalErr::Raise(value) => format!("raise: {}", disassemble_value(allocator, value)),
        error => format!("error: {}", cost.describe_error(&error)),
    };

    Ok(Err(TestFailure {
        message,
        trace: (!sidecars.is_empty()).then_some(trace),
    }))
}

/// Runs the program, and returns its output.
fn debug(args: DebugArgs) -> Result<String> {
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

    let dialect = DebugDialect::new(CLVM_FLAGS, true);

    if args.debug_metadata.is_empty() {
        let output = run_program(
            &mut allocator,
            &dialect,
            program,
            solution,
            args.cost.max_cost,
        )
        .map_err(|error| anyhow::anyhow!(args.cost.describe_error(&error)))?
        .1;

        return Ok(disassemble_value(&allocator, output));
    }

    let mut sidecars = Sidecars::default();

    for path in &args.debug_metadata {
        let metadata =
            fs::read(path).with_context(|| format!("Couldn't read {}", path.display()))?;
        sidecars
            .insert(&metadata)
            .with_context(|| format!("Couldn't load {}", path.display()))?;
    }

    match run_with_trace(
        &mut allocator,
        &dialect,
        program,
        solution,
        args.cost.max_cost,
        &sidecars,
    )? {
        Ok(output) => Ok(disassemble_value(&allocator, output.1)),
        Err(failure) => Err(anyhow::anyhow!(
            "{}\n{}",
            args.cost.describe_error(&failure.error),
            failure.trace
        )),
    }
}

#[cfg(test)]
mod tests {
    use std::{fs, path::Path};

    use super::{
        Command, CostArgs, DEFAULT_MAX_COST, Sidecars, build, debug, debug_metadata_path, run_test,
        single_debug_metadata_path,
    };
    use clap::{CommandFactory, Parser, error::ErrorKind};
    use clvmr::{Allocator, SExp, serde::node_from_bytes};
    use rue_clvm::assemble;
    use rue_diagnostic::SourceKind;
    use rue_diagnostic::{ForbiddenBy, Lint, LintGroup, LintLevel, LintLevels, LintName};
    use sha2::{Digest, Sha256};

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
                (
                    LintName::Lint(Lint::UnusedBinding),
                    Lint::UnusedBinding,
                    LintLevel::Warn,
                    ForbiddenBy::Flag
                ),
                (
                    LintName::Lint(Lint::UnusedFunction),
                    Lint::UnusedFunction,
                    LintLevel::Deny,
                    ForbiddenBy::Flag
                ),
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
            vec![(
                LintName::Lint(Lint::UnusedImport),
                Lint::UnusedImport,
                LintLevel::Allow,
                ForbiddenBy::Manifest
            )]
        );
        assert_eq!(levels.get(Lint::UnusedImport), LintLevel::Forbid);
        assert_eq!(levels.get(Lint::UnusedBinding), LintLevel::Allow);
    }

    #[test]
    fn lint_group_flags() {
        // A lint's own flag wins over its group's, in either order
        for flags in [
            ["-A", "unused_binding", "-D", "unused"],
            ["--deny", "unused", "--allow", "unused_binding"],
        ] {
            let Ok(Command::Build(args)) =
                Command::try_parse_from(["rue", "build"].into_iter().chain(flags))
            else {
                panic!("expected build command");
            };

            let mut levels = LintLevels::default();
            assert!(args.lints.apply(&mut levels).is_empty());

            assert_eq!(levels.get(Lint::UnusedBinding), LintLevel::Allow);
            assert_eq!(levels.get(Lint::UnusedFunction), LintLevel::Deny);
            assert_eq!(levels.get(Lint::UnusedValue), LintLevel::Deny);
            assert_eq!(
                levels.get(Lint::DoubleNegation),
                Lint::DoubleNegation.default_level()
            );
        }

        // Flags take precedence over `Rue.toml`, including an individual lint set there
        let Ok(Command::Test(args)) = Command::try_parse_from(["rue", "test", "-A", "unused"])
        else {
            panic!("expected test command");
        };

        let mut levels = LintLevels::default();
        levels.set(Lint::UnusedImport, LintLevel::Deny);
        assert!(args.lints.apply(&mut levels).is_empty());
        assert_eq!(levels.get(Lint::UnusedImport), LintLevel::Allow);
    }

    #[test]
    fn forbid_group_flags() {
        // A group can forbid a lint after its own flag, but its own flag can't change it afterwards
        for (flags, rejected) in [
            (["-A", "unused_binding", "-F", "unused"], Vec::new()),
            (
                ["-F", "unused", "-A", "unused_binding"],
                vec![(
                    LintName::Lint(Lint::UnusedBinding),
                    Lint::UnusedBinding,
                    LintLevel::Allow,
                    ForbiddenBy::Flag,
                )],
            ),
        ] {
            let Ok(Command::Build(args)) =
                Command::try_parse_from(["rue", "build"].into_iter().chain(flags))
            else {
                panic!("expected build command");
            };

            let mut levels = LintLevels::default();
            assert_eq!(args.lints.apply(&mut levels), rejected);
            assert_eq!(levels.get(Lint::UnusedBinding), LintLevel::Forbid);
            assert_eq!(levels.get(Lint::UnusedImport), LintLevel::Forbid);
        }

        // A group can't change lints forbidden in `Rue.toml`, but still sets the others
        let Ok(Command::Test(args)) = Command::try_parse_from(["rue", "test", "-A", "unused"])
        else {
            panic!("expected test command");
        };

        let mut levels = LintLevels::default();
        levels.set(Lint::UnusedImport, LintLevel::Forbid);
        assert_eq!(
            args.lints.apply(&mut levels),
            vec![(
                LintName::Group(LintGroup::Unused),
                Lint::UnusedImport,
                LintLevel::Allow,
                ForbiddenBy::Manifest
            )]
        );
        assert_eq!(levels.get(Lint::UnusedImport), LintLevel::Forbid);
        assert_eq!(levels.get(Lint::UnusedBinding), LintLevel::Allow);

        // A group is rejected once, rather than once for each lint in it
        let Ok(Command::Test(args)) =
            Command::try_parse_from(["rue", "test", "-F", "unused", "-W", "unused"])
        else {
            panic!("expected test command");
        };

        let mut levels = LintLevels::default();
        assert_eq!(
            args.lints.apply(&mut levels),
            vec![(
                LintName::Group(LintGroup::Unused),
                Lint::UnusedFunction,
                LintLevel::Warn,
                ForbiddenBy::Flag
            )]
        );
        assert_eq!(levels.get(Lint::UnusedValue), LintLevel::Forbid);
    }

    #[test]
    fn unknown_lint_flag() {
        let err = Command::try_parse_from(["rue", "build", "-D", "not_a_lint"]).unwrap_err();
        assert_eq!(err.kind(), ErrorKind::ValueValidation);

        // Unknown group names are reported the same way as unknown lints
        for flag in ["-A", "-W", "-D", "-F"] {
            let err = Command::try_parse_from(["rue", "test", flag, "unuse"]).unwrap_err();
            assert_eq!(err.kind(), ErrorKind::ValueValidation);
            assert!(err.to_string().contains("Unknown lint `unuse`"), "{err}");
        }
    }

    #[test]
    fn debug_metadata_flag() {
        let parse = |args: &[&str]| {
            let Ok(Command::Build(args)) =
                Command::try_parse_from(["rue", "build"].iter().chain(args))
            else {
                panic!("expected build command");
            };
            args
        };

        assert_eq!(parse(&[]).debug_metadata, None);
        assert_eq!(parse(&["--debug-metadata"]).debug_metadata, Some(None));

        // The path has to be given with `=`, so that the file to build isn't taken as the path
        let args = parse(&["--debug-metadata", "main.rue"]);
        assert_eq!(args.debug_metadata, Some(None));
        assert_eq!(args.file.as_deref(), Some("main.rue"));

        let args = parse(&["--debug-metadata=out.bin", "main.rue"]);
        assert_eq!(args.debug_metadata, Some(Some("out.bin".into())));
        assert_eq!(args.file.as_deref(), Some("main.rue"));
    }

    #[test]
    fn debug_metadata_paths() {
        assert_eq!(
            debug_metadata_path("dist/foo.rue.hex"),
            "dist/foo.rue.debug.clvm.bin"
        );
        assert_eq!(debug_metadata_path("foo"), "foo.debug.clvm.bin");

        // A single program's sidecar is next to its source, named after the file or the export
        let source = SourceKind::File("/project/src/main.rue".to_string());
        let base = Path::new("/project");
        assert_eq!(
            single_debug_metadata_path(Some(&source), None, base),
            Path::new("/project/src/main.rue.debug.clvm.bin")
        );
        assert_eq!(
            single_debug_metadata_path(Some(&source), Some("double"), base),
            Path::new("/project/src/double.rue.debug.clvm.bin")
        );
        // Without a file, it's next to the project's sources
        assert_eq!(
            single_debug_metadata_path(None, Some("double"), base),
            Path::new("/project/double.rue.debug.clvm.bin")
        );
    }

    /// A directory with a `main.rue` that has the source, which is removed when it's dropped.
    struct TempProject(std::path::PathBuf);

    impl TempProject {
        fn new(name: &str, source: &str) -> Self {
            let dir = std::env::temp_dir().join(format!("rue-cli-{name}-{}", std::process::id()));
            fs::create_dir_all(&dir).unwrap();
            let dir = dir.canonicalize().unwrap();
            fs::write(dir.join("main.rue"), source).unwrap();
            Self(dir)
        }
    }

    impl Drop for TempProject {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }

    /// Builds the programs in the directory with the arguments.
    fn run_build(dir: &Path, args: &[&str]) {
        let file = dir.join("main.rue");
        let Ok(Command::Build(args)) = Command::try_parse_from(
            ["rue", "build", file.to_str().unwrap()]
                .into_iter()
                .chain(args.iter().copied()),
        ) else {
            panic!("expected build command");
        };
        build(args).unwrap();
    }

    /// The program hash that a sidecar is for.
    fn sidecar_program_hash(sidecar: &[u8]) -> Vec<u8> {
        let mut allocator = Allocator::new();
        let mut node = node_from_bytes(&mut allocator, sidecar).unwrap();
        let mut fields = Vec::new();
        while let SExp::Pair(first, rest) = allocator.sexp(node) {
            fields.push(first);
            node = rest;
        }
        assert_eq!(allocator.atom(fields[0]).as_ref(), b"CHIALISP_DEBUG");
        allocator.atom(fields[3]).as_ref().to_vec()
    }

    #[test]
    fn build_writes_debug_metadata() {
        let project = TempProject::new(
            "build",
            "fn main(value: Int) -> Int {\n    double(value) + 1\n}\n\nexport fn double(value: Int) -> Int {\n    value * 2\n}\n",
        );
        let dir = &project.0;

        // Without the flag, no sidecars are written
        run_build(dir, &["--all"]);
        run_build(dir, &[]);
        assert!(
            fs::read_dir(dir).unwrap().all(|entry| !entry
                .unwrap()
                .path()
                .to_string_lossy()
                .ends_with(".bin"))
        );

        // With `--all`, each program's sidecar is next to it, and is for the program's bytes
        run_build(dir, &["--all", "--debug-metadata"]);

        for name in ["main", "main.rue_double"] {
            let hex = fs::read_to_string(dir.join(format!("{name}.rue.hex"))).unwrap();
            let program = hex::decode(hex.trim()).unwrap();
            let sidecar = fs::read(dir.join(format!("{name}.rue.debug.clvm.bin"))).unwrap();
            assert_eq!(
                sidecar_program_hash(&sidecar),
                Sha256::digest(&program).to_vec(),
                "{name}"
            );
        }

        let all = fs::read(dir.join("main.rue.debug.clvm.bin")).unwrap();
        fs::remove_file(dir.join("main.rue.debug.clvm.bin")).unwrap();

        // A single program's sidecar is next to its source, or where it's asked for
        run_build(dir, &["--debug-metadata"]);
        assert_eq!(fs::read(dir.join("main.rue.debug.clvm.bin")).unwrap(), all);

        let path = dir.join("custom.bin");
        run_build(
            dir,
            &[
                "--export",
                "double",
                &format!("--debug-metadata={}", path.display()),
            ],
        );
        assert_eq!(
            fs::read(&path).unwrap(),
            fs::read(dir.join("main.rue_double.rue.debug.clvm.bin")).unwrap()
        );
    }

    #[test]
    fn build_long_help() {
        let err = Command::try_parse_from(["rue", "build", "--help"]).unwrap_err();
        assert_eq!(err.kind(), ErrorKind::DisplayHelp);
    }

    /// Parses the cost flags for both `rue test` and `rue debug`.
    fn parse_cost_args(args: &[&str]) -> [CostArgs; 2] {
        let Ok(Command::Test(test)) = Command::try_parse_from(["rue", "test"].iter().chain(args))
        else {
            panic!("expected test command");
        };
        let Ok(Command::Debug(debug)) =
            Command::try_parse_from(["rue", "debug", "()"].iter().chain(args))
        else {
            panic!("expected debug command");
        };
        [test.cost, debug.cost]
    }

    #[test]
    fn max_cost_flag() {
        for args in parse_cost_args(&[]) {
            assert_eq!(args.max_cost, 11_000_000_000);
        }
        for args in parse_cost_args(&["--max-cost", "1000"]) {
            assert_eq!(args.max_cost, 1000);
        }

        // clvmr treats a cost limit of 0 as no limit at all
        for command in [&["rue", "test"][..], &["rue", "debug", "()"]] {
            let args = command.iter().chain(&["--max-cost", "0"]);
            assert!(Command::try_parse_from(args).is_err());
        }
    }

    /// Runs `rue debug` with the arguments, and returns its output or error message.
    fn run_debug(args: &[&str]) -> Result<String, String> {
        let Ok(Command::Debug(args)) = Command::try_parse_from(["rue", "debug"].iter().chain(args))
        else {
            panic!("expected debug command");
        };
        debug(args).map_err(|error| error.to_string())
    }

    #[test]
    fn debug_enforces_the_cost_limit() {
        let program = "(+ 2 5)";
        assert_eq!(run_debug(&[program, "(1 2)"]).unwrap(), "3");
        let error = run_debug(&[program, "(1 2)", "--max-cost", "10"]).unwrap_err();
        assert!(error.contains("the cost limit is 10"), "{error}");

        // A program that never ends fails instead of hanging
        let error = run_debug(&["(a 2 1)", "((a 2 1))", "--max-cost", "1000000"]).unwrap_err();
        assert!(error.contains("`--max-cost`"), "{error}");
    }

    /// Runs a test program with the cost limit.
    fn run_test_with(program: &str, max_cost: u64) -> Result<(), String> {
        let mut allocator = Allocator::new();
        let program = assemble(&mut allocator, program).unwrap();
        run_test(
            &mut allocator,
            program,
            CostArgs { max_cost },
            &Sidecars::default(),
        )
        .unwrap()
        .map_err(|failure| {
            assert!(failure.trace.is_none(), "there's no sidecar to trace with");
            failure.message
        })
    }

    #[test]
    fn tests_enforce_the_cost_limit() {
        let program = "(i (= (sha256 (q . 1)) ()) (q . 1) ())";
        assert_eq!(run_test_with(program, DEFAULT_MAX_COST), Ok(()));
        let error = run_test_with(program, 100).unwrap_err();
        assert!(error.contains("the cost limit is 100"), "{error}");

        // A test that never ends fails instead of hanging
        let forever = "(a (q 2 2 1) (q (a 2 1)))";
        let error = run_test_with(forever, 1_000_000).unwrap_err();
        assert!(error.contains("`--max-cost`"), "{error}");
    }

    #[test]
    fn other_failures_are_reported_as_before() {
        let fail = |program| run_test_with(program, DEFAULT_MAX_COST).unwrap_err();
        assert_eq!(fail("(x (q . 5))"), "raise: 5");
        assert_eq!(fail("(q . 1)"), "non-nil output");

        // Only running out of cost mentions the limit
        assert_eq!(
            fail("(f (q . 1))"),
            "error: InvalidOperatorArg: first of non-cons"
        );
        assert_eq!(
            run_debug(&["(f (q . 1))"]).unwrap_err(),
            "InvalidOperatorArg: first of non-cons"
        );
    }
}
