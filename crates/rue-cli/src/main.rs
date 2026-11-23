use std::{
    collections::{HashMap, HashSet},
    env, fs,
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
use rue_compiler::{CompiledExport, CompiledTest, Compiler, FileTree, clvm_target, normalize_path};
use rue_diagnostic::{
    Diagnostic, DiagnosticSeverity, ForbiddenBy, LineIndex, Lint, LintGroup, LintLevel, LintLevels,
    LintName, SourceKind,
};
use rue_lir::{ClvmTarget, DebugDialect};
use rue_options::{CompilerOptions, Manifest, Project, find_project};

mod compact;
mod explain;
mod fmt;
mod trace;

use explain::*;
use fmt::*;
use trace::{Sidecars, run_with_trace};

#[derive(Debug, Parser)]
pub enum Command {
    /// Create a new project, with a `Rue.toml` and a `main.rue`
    Init(InitArgs),
    /// Compile a file, or the project
    Build(BuildArgs),
    /// Run the tests of a file, or of the project
    Test(TestArgs),
    /// Run a compiled program with a solution, and print its output
    Debug(DebugArgs),
    /// Show a compiled program as pseudocode, with where each line is in the source
    Explain(ExplainArgs),
    /// Format source files, or check that they're formatted
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
    target: TargetArgs,
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
    target: TargetArgs,
    #[clap(flatten)]
    lints: LintArgs,
    #[clap(flatten)]
    cost: CostArgs,
}

// What programs are compiled for and run with, which takes precedence over `Rue.toml`. This isn't
// a doc comment, since clap would use it as the description of every command that flattens it.
#[derive(Debug, Clone, Copy, Args)]
pub struct TargetArgs {
    /// Target CLVM with the `LIMITS` flag that mainnet enforces until hard fork 2, which rejects
    /// some long operands of `*`, `/`, `%`, `divmod`, `modpow`, `g1_multiply` and `g2_multiply`.
    /// This is the same as `operand_limits = true` in `Rue.toml`, and turns it on if it's off there
    #[clap(long)]
    operand_limits: bool,
    /// Target mainnet as it will be after hard fork 2, which allows `modpow` and `keccak256`, and
    /// uses the new cost model. This is the same as `hard_fork_2 = true` in `Rue.toml`, and turns
    /// it on if it's off there
    #[clap(long)]
    hard_fork_2: bool,
}

impl TargetArgs {
    /// Applies the flags to the options, and returns an error if they set options that conflict.
    fn apply(self, options: &mut CompilerOptions) -> Result<()> {
        options.operand_limits |= self.operand_limits;
        options.hard_fork_2 |= self.hard_fork_2;
        Ok(options.check_target()?)
    }
}

/// The default cost limit for running a program, which is the same as Chia's block cost limit.
const DEFAULT_MAX_COST: u64 = rue_lir::MAX_BLOCK_COST;

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
    /// The program is run with the flags of the target, which is also set by the `Rue.toml` of the
    /// project it's run inside of (found from the current directory or its parents)
    #[clap(flatten)]
    target: TargetArgs,
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
        Command::Explain(args) => {
            println!("{}", explain(&args)?);
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

/// A project that `rue build` or `rue test` compiled without errors.
struct LoadedProject {
    manifest: Option<Manifest>,
    entrypoint: PathBuf,
    ctx: Compiler,
    tree: FileTree,
    /// The directory that the project's sources are in, which paths are shown relative to.
    base_path: PathBuf,
}

/// Finds the project that `file` (or the current directory) is in, applies the lint and target
/// flags to it, and compiles it. It exits if there's no project, the project is pinned to another
/// compiler version, or there are errors, which it reports.
fn load_project(
    file: Option<&str>,
    debug: bool,
    lints: &LintArgs,
    target: TargetArgs,
) -> Result<LoadedProject> {
    let search_path = &Path::new(file.unwrap_or(".")).canonicalize()?;
    let project = find_project(search_path, debug)?;

    let Some(mut project) = project else {
        eprintln!("{}", "No project found".red().bold());
        process::exit(1);
    };

    let lint_errors = configure_lints(&mut project, lints);
    target.apply(&mut project.options)?;

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

    Ok(LoadedProject {
        manifest: project.manifest,
        entrypoint: project.entrypoint,
        ctx,
        tree,
        base_path,
    })
}

/// The hex split into lines of 64 characters, as `--split` prints it.
fn split_hex(hex: &str) -> impl Iterator<Item = &str> {
    // Hex is ASCII, so every chunk is on a character boundary
    hex.as_bytes()
        .chunks(64)
        .map(|chunk| std::str::from_utf8(chunk).expect("hex is ASCII"))
}

/// Where `rue build --all` writes an output of the program `name` with the extension: in the
/// project's `dist_dir`, if it has one, and otherwise next to its source file.
fn output_path(dist_dir: Option<&Path>, file_path: &Path, name: &str, extension: &str) -> String {
    dist_dir
        .unwrap_or_else(|| file_path.parent().unwrap())
        .join(format!("{name}.{extension}"))
        .to_string_lossy()
        .to_string()
}

fn build(args: BuildArgs) -> Result<()> {
    let mut allocator = Allocator::new();

    let LoadedProject {
        manifest,
        entrypoint,
        mut ctx,
        tree,
        base_path,
    } = load_project(args.file.as_deref(), args.debug, &args.lints, args.target)?;

    let file_kind = args
        .file
        .map(|file| normalize_path(Path::new(&file)))
        .transpose()?;

    let main_kind = if let Some(file_kind) = &file_kind {
        Some(file_kind.clone())
    } else if entrypoint.join("main.rue").exists() {
        Some(normalize_path(&entrypoint.join("main.rue"))?)
    } else {
        None
    };

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

        let dist_dir = manifest
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
                    for line in split_hex(&hex) {
                        output.push_str(line);
                        output.push('\n');
                    }
                } else {
                    output.push_str(&hex);
                    output.push('\n');
                }

                let path = output_path(dist_dir.as_deref(), file_path, &name, "rue.hex");

                if let Some(metadata) = metadata {
                    outputs.insert(PathBuf::from(debug_metadata_path(&path)), metadata);
                }

                outputs.insert(PathBuf::from(path), output.into_bytes());

                if args.hash {
                    let hash = tree_hash(&allocator, ptr).to_string();

                    let path = output_path(dist_dir.as_deref(), file_path, &name, "rue.hash");

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
                for line in split_hex(&hex) {
                    println!("{line}");
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

    let LoadedProject {
        mut ctx,
        tree,
        base_path,
        ..
    } = load_project(args.file.as_deref(), true, &args.lints, args.target)?;

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

        if let Err((failure, trace)) =
            run_and_trace_test(&mut ctx, &mut allocator, test, &base_path, args.cost)
        {
            eprintln!(
                "{}",
                format!("Test failed due to {}", failure.message)
                    .red()
                    .bold()
            );

            match trace {
                Some(Ok(trace)) => eprintln!("{trace}"),
                Some(Err(reason)) => eprintln!(
                    "{}",
                    format!("Couldn't show where the test failed: {reason}").yellow()
                ),
                None => {}
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
    /// Whether the program failed while it was running, rather than returning something other than
    /// nil, so there's a stack to show.
    raised: bool,
}

/// Runs a test, which passes if it returns nil, and otherwise returns why it failed. It's run
/// with the flags of the target it's compiled for.
fn run_test(
    allocator: &mut Allocator,
    program: NodePtr,
    target: ClvmTarget,
    cost: CostArgs,
) -> Result<(), TestFailure> {
    let message = match run_program(
        allocator,
        &DebugDialect::new(target.flags(), true),
        program,
        NodePtr::NIL,
        cost.max_cost,
    ) {
        Ok(output) => {
            if let SExp::Atom = allocator.sexp(output.1)
                && allocator.atom(output.1).is_empty()
            {
                return Ok(());
            }

            return Err(TestFailure {
                message: "non-nil output".to_string(),
                raised: false,
            });
        }
        Err(EvalErr::Raise(value)) => format!("raise: {}", disassemble_value(allocator, value)),
        Err(error) => format!("error: {}", cost.describe_error(&error)),
    };

    Err(TestFailure {
        message,
        raised: true,
    })
}

/// Runs a test, and if it fails, returns why, along with where it was when it failed if it raised
/// (see [`trace_test`]).
fn run_and_trace_test(
    ctx: &mut Compiler,
    allocator: &mut Allocator,
    test: &CompiledTest,
    base_path: &Path,
    cost: CostArgs,
) -> Result<(), (TestFailure, Option<Result<String, String>>)> {
    let checkpoint = allocator.checkpoint();

    let failure = match run_test(allocator, test.ptr, ctx.target(), cost) {
        Ok(()) => return Ok(()),
        Err(failure) => failure,
    };

    if !failure.raised {
        return Err((failure, None));
    }

    // The test is run again with as much room as it had the first time, and what that allocates
    // is freed afterwards, so that it doesn't affect the tests after it
    allocator.restore_checkpoint(&checkpoint);
    let trace = trace_test(ctx, allocator, test, base_path, cost);
    allocator.restore_checkpoint(&checkpoint);

    Err((failure, Some(trace)))
}

/// Where a test that failed was when it did, which is found by compiling it again with a debug
/// metadata sidecar, and running it again with the frames kept. Programs run the same way each
/// time, so it fails the same way, and tests that pass don't pay for the sidecar or the frames.
fn trace_test(
    ctx: &mut Compiler,
    allocator: &mut Allocator,
    test: &CompiledTest,
    base_path: &Path,
    cost: CostArgs,
) -> Result<String, String> {
    let (program, metadata) = test
        .with_debug_metadata(ctx, allocator, base_path)
        .map_err(|error| error.to_string())?;

    let mut sidecars = Sidecars::default();
    sidecars.insert(&metadata)?;

    // What the test prints has already been shown, so it isn't printed again
    match run_with_trace(
        allocator,
        &DebugDialect::new(ctx.target().flags(), false),
        program,
        NodePtr::NIL,
        cost.max_cost,
        &sidecars,
    ) {
        Ok(_) => Err("it didn't fail when it was run again".to_string()),
        Err(failure) => failure.trace,
    }
}

/// Runs the program, and returns its output.
fn debug(args: DebugArgs) -> Result<String> {
    debug_in(args, &env::current_dir()?)
}

/// Runs the program like [`debug`], with the flags of the target that's asked for, or that the
/// project the directory is in is compiled for.
fn debug_in(args: DebugArgs, dir: &Path) -> Result<String> {
    let mut options =
        find_project(dir, true)?.map_or_else(CompilerOptions::debug, |project| project.options);
    args.target.apply(&mut options)?;
    let target = clvm_target(&options);

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

    let dialect = DebugDialect::new(target.flags(), true);

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
        sidecars.insert(&metadata).map_err(|error| {
            anyhow::anyhow!("{} isn't valid debug metadata: {error}", path.display())
        })?;
    }

    match run_with_trace(
        &mut allocator,
        &dialect,
        program,
        solution,
        args.cost.max_cost,
        &sidecars,
    ) {
        Ok(output) => Ok(disassemble_value(&allocator, output.1)),
        Err(failure) => {
            let trace = failure
                .trace
                .unwrap_or_else(|reason| format!("Couldn't show where it failed: {reason}"));
            Err(anyhow::anyhow!(
                "{}\n{trace}",
                args.cost.describe_error(&failure.error)
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{fs, path::Path};

    use super::{
        Command, CostArgs, DEFAULT_MAX_COST, build, debug, debug_in, debug_metadata_path,
        run_and_trace_test, run_test, single_debug_metadata_path, trace_test,
    };
    use clap::{CommandFactory, Parser, error::ErrorKind};
    use clvm_utils::tree_hash;
    use clvmr::{Allocator, SExp, serde::node_from_bytes};
    use rue_clvm::assemble;
    use rue_compiler::{CompiledTest, Compiler, FileTree};
    use rue_diagnostic::SourceKind;
    use rue_diagnostic::{ForbiddenBy, Lint, LintGroup, LintLevel, LintLevels, LintName};
    use rue_lir::ClvmTarget;
    use rue_options::CompilerOptions;
    use sha2::{Digest, Sha256};

    #[test]
    fn cli_debug_asserts() {
        Command::command().debug_assert();
    }

    #[test]
    fn commands_have_their_own_descriptions() {
        let command = Command::command();
        let abouts: Vec<_> = command
            .get_subcommands()
            .map(|subcommand| {
                let about = subcommand.get_about().map(ToString::to_string);
                (subcommand.get_name().to_string(), about)
            })
            .collect();

        for (name, about) in &abouts {
            let about = about
                .as_ref()
                .unwrap_or_else(|| panic!("`{name}` has no description"));
            let shared = abouts
                .iter()
                .filter(|(_, other)| other.as_ref() == Some(about))
                .count();
            assert_eq!(shared, 1, "`{name}` shares its description: {about}");
        }
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
    struct TempProject {
        /// The directory, without symbolic links in its path (such as macOS's `/var`), since
        /// it's compared with paths the compiler resolved.
        path: std::path::PathBuf,
        _dir: tempfile::TempDir,
    }

    impl TempProject {
        fn new(name: &str, source: &str) -> Self {
            let dir = tempfile::Builder::new()
                .prefix(&format!("rue-cli-{name}-"))
                .tempdir()
                .unwrap();
            let path = dir.path().canonicalize().unwrap();
            fs::write(path.join("main.rue"), source).unwrap();
            Self { path, _dir: dir }
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
        let dir = &project.path;

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

    /// With `--all`, each program is written next to its source, or to the manifest's `dist_dir`
    /// if it has one, with its tree hash beside it for `--hash`, and with its hex split into lines
    /// of 64 characters for `--split`.
    #[test]
    fn build_all_writes_programs_and_hashes() {
        let project = TempProject::new(
            "build-all",
            "fn main() -> String {\n    \"a string that's long enough for its program to take more than one line\"\n}\n",
        );
        let dir = &project.path;

        let check = |hex_path: &Path, hash_path: &Path| {
            let hex = fs::read_to_string(hex_path).unwrap();
            let lines: Vec<&str> = hex.lines().collect();
            assert!(lines.len() > 1, "{hex}");
            assert!(lines[..lines.len() - 1].iter().all(|line| line.len() == 64));
            assert!(!lines[lines.len() - 1].is_empty() && lines[lines.len() - 1].len() <= 64);

            let mut allocator = Allocator::new();
            let program =
                node_from_bytes(&mut allocator, &hex::decode(lines.concat()).unwrap()).unwrap();
            assert_eq!(
                fs::read_to_string(hash_path).unwrap(),
                format!("{}\n", tree_hash(&allocator, program))
            );
        };

        run_build(dir, &["--all", "--hash", "--split"]);
        check(&dir.join("main.rue.hex"), &dir.join("main.rue.hash"));

        let dist = dir.join("dist");
        fs::write(
            dir.join("Rue.toml"),
            format!(
                "[compiler]\nentrypoint = \".\"\ndist_dir = \"{}\"\n",
                dist.display()
            ),
        )
        .unwrap();

        run_build(dir, &["--all", "--hash", "--split"]);
        check(&dist.join("main.rue.hex"), &dist.join("main.rue.hash"));
    }

    /// A failing assertion shows its message after its location, and the trace shows the statement.
    #[test]
    fn failing_assert_shows_its_message() {
        let project = TempProject::new(
            "assert-message-test",
            "test fn fails() {\n    check(3);\n}\n\nfn check(value: Int) -> Int {\n    assert value > 10, [\"too small\", value];\n    value\n}\n",
        );
        let path = project.path.join("main.rue");

        let mut ctx = Compiler::new(CompilerOptions::debug());
        let tree = FileTree::compile_file(&mut ctx, &path).unwrap();
        let mut allocator = Allocator::new();
        let tests = tree
            .tests(&mut ctx, &mut allocator, None, None, &project.path, false)
            .unwrap();
        let cost = CostArgs {
            max_cost: DEFAULT_MAX_COST,
        };

        let (failure, trace) =
            run_and_trace_test(&mut ctx, &mut allocator, &tests[0], &project.path, cost)
                .unwrap_err();
        assert_eq!(
            failure.message,
            "raise: (\"assertion failed at main.rue:6:5\" (\"too small\" 3))"
        );
        assert_eq!(
            trace,
            Some(Ok(
                "  main.rue:2:5, in fails\n    check(3);\n  main.rue:6:5, in check\n    assert value > 10, [\"too small\", value];\n    value = 3"
                    .to_string()
            ))
        );
    }

    const FAILING_SOURCE: &str = "fn main(value: Int) -> Int {\n    check(value)\n}\n\ntest fn fails() {\n    check(30);\n}\n\ntest fn fails_too() {\n    check(3);\n}\n\nfn check(value: Int) -> Int {\n    assert value > 10;\n    value\n}\n\ntest fn divides() {\n    divide(1, 0);\n}\n\nfn divide(a: Int, b: Int) -> Int {\n    a / b\n}\n\nexport fn double(value: Int) -> Int {\n    value * 2\n}\n";

    #[test]
    fn failing_tests_are_traced() {
        let project = TempProject::new("trace-test", FAILING_SOURCE);
        let path = project.path.join("main.rue");

        let mut ctx = Compiler::new(CompilerOptions::debug());
        let tree = FileTree::compile_file(&mut ctx, &path).unwrap();
        let mut allocator = Allocator::new();
        let tests = tree
            .tests(&mut ctx, &mut allocator, None, None, &project.path, false)
            .unwrap();
        let cost = CostArgs {
            max_cost: DEFAULT_MAX_COST,
        };

        // The second test fails, and its name contains the first's, which passes
        assert_eq!(tests[1].name.as_deref(), Some("fails_too"));
        assert!(run_test(&mut allocator, tests[0].ptr, ClvmTarget::default(), cost).is_ok());
        let failure =
            run_test(&mut allocator, tests[1].ptr, ClvmTarget::default(), cost).unwrap_err();
        assert!(failure.raised);

        // The trace shows the test at its call to `check`, and the assertion with its argument
        let trace = trace_test(&mut ctx, &mut allocator, &tests[1], &project.path, cost).unwrap();
        assert_eq!(
            trace,
            "  main.rue:10:5, in fails_too\n    check(3);\n  main.rue:14:5, in check\n    assert value > 10;\n    value = 3"
        );

        // When a failure is traced, what tracing allocates is freed, along with what the test did
        let pairs = allocator.pair_count();
        let (failure, traced) =
            run_and_trace_test(&mut ctx, &mut allocator, &tests[1], &project.path, cost)
                .unwrap_err();
        assert!(failure.raised);
        assert_eq!(traced, Some(Ok(trace.clone())));
        assert_eq!(allocator.pair_count(), pairs);
        assert!(
            run_and_trace_test(&mut ctx, &mut allocator, &tests[0], &project.path, cost).is_ok()
        );

        // A failure that isn't a raise is traced too
        assert_eq!(tests[2].name.as_deref(), Some("divides"));
        let (failure, traced) =
            run_and_trace_test(&mut ctx, &mut allocator, &tests[2], &project.path, cost)
                .unwrap_err();
        assert!(failure.raised);
        let traced = traced.unwrap().unwrap();
        assert!(traced.contains("main.rue:23:5, in divide"), "{traced}");
        assert!(traced.contains("b = ()"), "{traced}");

        // The test that passes doesn't fail when it's run again
        assert_eq!(
            trace_test(&mut ctx, &mut allocator, &tests[0], &project.path, cost),
            Err("it didn't fail when it was run again".to_string())
        );

        // A test that returns something other than nil has no stack to show
        let program = assemble(&mut allocator, "(q . 1)").unwrap();
        assert!(
            !run_test(&mut allocator, program, ClvmTarget::default(), cost)
                .unwrap_err()
                .raised
        );
        let returns_one = CompiledTest {
            ptr: program,
            ..tests[0].clone()
        };
        let (_, traced) =
            run_and_trace_test(&mut ctx, &mut allocator, &returns_one, &project.path, cost)
                .unwrap_err();
        assert_eq!(traced, None);
    }

    /// Runs `rue explain` with the arguments, and returns its output or error message.
    fn run_explain(args: &[&str]) -> Result<String, String> {
        let Ok(Command::Explain(args)) =
            Command::try_parse_from(["rue", "explain"].iter().chain(args))
        else {
            panic!("expected explain command");
        };
        crate::explain(&args).map_err(|error| error.to_string())
    }

    #[test]
    fn explain_shows_a_program() {
        let project = TempProject::new(
            "explain",
            "fn main(amount: Int, skip: Bool) -> Int {\n    let doubled = amount + amount;\n    if skip { 0 } else { doubled }\n}\n",
        );
        run_build(&project.0, &["--all", "--debug-metadata"]);
        let program = project.0.join("main.rue.hex");
        let program = program.to_str().unwrap();
        let hex = fs::read_to_string(program).unwrap();
        let sidecar = project.0.join("main.rue.debug.clvm.bin");

        // A program file is explained with the sidecar next to it
        let named = run_explain(&[program]).unwrap();
        assert!(
            named.contains("fn main(amount: Int, skip: Atom) {"),
            "{named}"
        );
        assert!(named.contains("amount + amount"), "{named}");
        assert!(named.contains("main.rue:3:5-35"), "{named}");

        // As is the hex of the program, with the sidecar given
        assert_eq!(
            run_explain(&[
                "-x",
                hex.trim(),
                "--debug-metadata",
                sidecar.to_str().unwrap()
            ]),
            Ok(named)
        );

        // Without a sidecar, nothing is named
        let unnamed = run_explain(&["-x", hex.trim()]).unwrap();
        assert!(unnamed.contains("solution@2 + solution@2"), "{unnamed}");

        // A sidecar for another program is rejected
        let error =
            run_explain(&["(q . 1)", "--debug-metadata", sidecar.to_str().unwrap()]).unwrap_err();
        assert!(error.contains("isn't valid for the program"), "{error}");
    }

    #[test]
    fn debug_shows_where_a_program_failed() {
        let project = TempProject::new("trace-debug", FAILING_SOURCE);
        let path = project.path.join("main.rue");
        let sidecar = project.path.join("main.bin");

        run_build(&project.path, &["--all", "--debug-metadata"]);
        fs::rename(project.path.join("main.rue.debug.clvm.bin"), &sidecar).unwrap();

        let hex = fs::read_to_string(project.path.join("main.rue.hex")).unwrap();
        let hex = hex.trim();
        let sidecar = sidecar.to_str().unwrap();

        // The solution `(3)` fails the assertion, and `(30)` doesn't
        let error = run_debug(&["-x", hex, "ff0380", "--debug-metadata", sidecar]).unwrap_err();
        assert!(error.starts_with("clvm raise\n"), "{error}");
        // `check` is inlined into `main` in a release build, where it's all of `main`'s body
        assert!(error.contains("main.rue:14:5, in main"), "{error}");
        assert!(error.contains("value = 3"), "{error}");
        assert_eq!(
            run_debug(&["-x", hex, "ff1e80", "--debug-metadata", sidecar]),
            Ok("30".to_string())
        );

        // Sidecars for other programs don't get in the way
        let error = run_debug(&[
            "-x",
            hex,
            "ff0380",
            "--debug-metadata",
            project
                .path
                .join("main.rue_double.rue.debug.clvm.bin")
                .to_str()
                .unwrap(),
            "--debug-metadata",
            sidecar,
        ])
        .unwrap_err();
        assert!(error.contains("main.rue:14:5, in main"), "{error}");

        // Without the sidecar, the failure is reported as before
        assert_eq!(run_debug(&["-x", hex, "ff0380"]).unwrap_err(), "clvm raise");

        // A file that isn't a sidecar is an error
        let error = run_debug(&[
            "-x",
            hex,
            "ff0380",
            "--debug-metadata",
            path.to_str().unwrap(),
        ])
        .unwrap_err();
        assert!(error.contains("isn't valid debug metadata"), "{error}");
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
    fn debug_runs_with_operand_limits_when_asked() {
        // `%` limits its dividend to 256 bytes with `LIMITS`, and `logand` doesn't
        let solution = format!("(0x01{})", "00".repeat(299));

        for program in ["(% 2 (q . 2))", "(logand 2 (q . 1))"] {
            assert_eq!(run_debug(&[program, &solution]).unwrap(), "()");
        }

        let error = run_debug(&["(% 2 (q . 2))", &solution, "--operand-limits"]).unwrap_err();
        assert!(error.contains("mod"), "{error}");
        assert_eq!(
            run_debug(&["(logand 2 (q . 1))", &solution, "--operand-limits"]).unwrap(),
            "()"
        );

        // In a project with `operand_limits` on, it's used without the flag
        let project = TempProject::new("debug-operand-limits", "fn main() -> Int {\n    1\n}\n");
        let parse = |args: &[&str]| {
            let Ok(Command::Debug(args)) =
                Command::try_parse_from(["rue", "debug"].iter().chain(args))
            else {
                panic!("expected debug command");
            };
            args
        };

        let sub = project.path.join("sub");
        fs::create_dir_all(&sub).unwrap();

        // The project is found from any directory inside it, and the flag still turns `LIMITS` on
        // when the project leaves it off
        for (manifest, flag, fails) in [
            ("[compiler]\n", false, false),
            ("[compiler]\noperand_limits = false\n", true, true),
            ("[compiler]\noperand_limits = true\n", false, true),
        ] {
            fs::write(project.path.join("Rue.toml"), manifest).unwrap();
            let mut args = vec!["(% 2 (q . 2))", solution.as_str()];

            if flag {
                args.push("--operand-limits");
            }

            for dir in [&project.path, &sub] {
                let result = debug_in(parse(&args), dir);
                assert_eq!(
                    result.is_err(),
                    fails,
                    "{manifest:?} in {dir:?}: {result:?}"
                );
            }
        }

        // A manifest that can't be parsed is reported, along with where it is
        fs::write(project.path.join("Rue.toml"), "[compiler\n").unwrap();
        let error = debug_in(parse(&["(q . 1)"]), &sub).unwrap_err().to_string();
        assert!(error.contains("Rue.toml"), "{error}");
    }

    #[test]
    fn debug_runs_with_hard_fork_2_when_asked() {
        // `keccak256` is unknown outside of the softfork guard until hard fork 2
        let program = "(keccak256 (q . \"hello\"))";
        let hash = "0x1c8aff950685c2ed4bc3174f3472287b56d9517b9c948127319a09a7a36deac8";

        assert!(run_debug(&[program]).is_err());
        assert_eq!(run_debug(&[program, "()", "--hard-fork-2"]).unwrap(), hash);

        // A project with `hard_fork_2` on is run with it, and it can't be combined with
        // `operand_limits`, which it removes
        let project = TempProject::new("debug-hard-fork-2", "fn main() -> Int {\n    1\n}\n");
        let parse = |args: &[&str]| {
            let Ok(Command::Debug(args)) =
                Command::try_parse_from(["rue", "debug"].iter().chain(args))
            else {
                panic!("expected debug command");
            };
            args
        };

        fs::write(
            project.path.join("Rue.toml"),
            "[compiler]\nhard_fork_2 = true\n",
        )
        .unwrap();
        assert_eq!(debug_in(parse(&[program]), &project.path).unwrap(), hash);

        let error = debug_in(parse(&[program, "()", "--operand-limits"]), &project.path)
            .unwrap_err()
            .to_string();
        assert!(error.contains("hard_fork_2"), "{error}");
    }

    #[test]
    fn tests_run_with_hard_fork_2_when_compiled_with_it() {
        // The recursion keeps `keccak256` from being folded, so the tests run it
        let source = "fn chain(value: Bytes, times: Int) -> Bytes {\n    if times == 0 {\n        value\n    } else {\n        chain(keccak256(value), times - 1)\n    }\n}\n\ntest fn hashes() {\n    assert chain(\"hello\", 1) == 0x1c8aff950685c2ed4bc3174f3472287b56d9517b9c948127319a09a7a36deac8;\n}\n\ntest fn fails_after_hashing() {\n    assert chain(\"hello\", 1) == 0x00;\n}\n";
        let project = TempProject::new("hard-fork-2-test", source);
        let path = project.path.join("main.rue");
        let cost = CostArgs {
            max_cost: DEFAULT_MAX_COST,
        };

        let mut ctx = Compiler::new(CompilerOptions {
            hard_fork_2: true,
            ..CompilerOptions::debug()
        });
        let tree = FileTree::compile_file(&mut ctx, &path).unwrap();
        let mut allocator = Allocator::new();
        let tests = tree
            .tests(&mut ctx, &mut allocator, None, None, &project.path, false)
            .unwrap();

        assert!(
            run_and_trace_test(&mut ctx, &mut allocator, &tests[0], &project.path, cost).is_ok()
        );

        // The trace runs it again with the same flags, so it fails at the assertion rather than at
        // `keccak256`
        let (failure, trace) =
            run_and_trace_test(&mut ctx, &mut allocator, &tests[1], &project.path, cost)
                .unwrap_err();
        assert!(
            failure.message.contains("assertion failed"),
            "{}",
            failure.message
        );
        let trace = trace.unwrap().unwrap();
        assert!(
            trace.contains("main.rue:14:5, in fails_after_hashing"),
            "{trace}"
        );
        assert!(!trace.contains("in chain"), "{trace}");
    }

    #[test]
    fn target_flags_conflict_when_both_are_set() {
        let Ok(Command::Build(args)) =
            Command::try_parse_from(["rue", "build", "--hard-fork-2", "--operand-limits"])
        else {
            panic!("expected build command");
        };

        let mut options = CompilerOptions::default();
        assert!(args.target.apply(&mut options).is_err());

        // A flag combines with the option set elsewhere, such as in `Rue.toml`
        let Ok(Command::Test(args)) = Command::try_parse_from(["rue", "test", "--hard-fork-2"])
        else {
            panic!("expected test command");
        };

        let mut options = CompilerOptions::default();
        args.target.apply(&mut options).unwrap();
        assert!(options.hard_fork_2 && !options.operand_limits);

        options = CompilerOptions {
            operand_limits: true,
            ..CompilerOptions::default()
        };
        assert!(args.target.apply(&mut options).is_err());
    }

    #[test]
    fn tests_run_with_operand_limits_when_compiled_with_them() {
        // The test passes without `LIMITS`, but `%` fails for the long dividend with them
        let source = format!(
            "test fn odd() {{\n    assert !is_odd(0x01{} as Int);\n}}\n\nfn is_odd(value: Int) -> Bool {{\n    value % 2 == 1\n}}\n",
            "00".repeat(299)
        );
        let project = TempProject::new("operand-limits-test", &source);
        let path = project.path.join("main.rue");
        let cost = CostArgs {
            max_cost: DEFAULT_MAX_COST,
        };

        for operand_limits in [false, true] {
            let mut ctx = Compiler::new(CompilerOptions {
                operand_limits,
                ..CompilerOptions::debug()
            });
            let tree = FileTree::compile_file(&mut ctx, &path).unwrap();
            let mut allocator = Allocator::new();
            let tests = tree
                .tests(&mut ctx, &mut allocator, None, None, &project.path, false)
                .unwrap();
            let result =
                run_and_trace_test(&mut ctx, &mut allocator, &tests[0], &project.path, cost);

            if operand_limits {
                // The trace runs it again with `LIMITS`, so it fails in the same place
                let (failure, trace) = result.unwrap_err();
                assert!(failure.message.contains("mod"), "{}", failure.message);
                let trace = trace.unwrap().unwrap();
                assert!(trace.contains("is_odd"), "{trace}");
            } else {
                assert!(result.is_ok());
            }
        }

        // The flag turns the option on for `rue build` and `rue test`
        let mut project = rue_options::find_project(&project.path, true)
            .unwrap()
            .unwrap();
        assert!(!project.options.operand_limits);
        let Ok(Command::Build(args)) =
            Command::try_parse_from(["rue", "build", "--operand-limits"])
        else {
            panic!("expected build command");
        };
        args.target.apply(&mut project.options).unwrap();
        assert!(project.options.operand_limits);
        let Ok(Command::Test(args)) = Command::try_parse_from(["rue", "test", "--operand-limits"])
        else {
            panic!("expected test command");
        };
        assert!(args.target.operand_limits);
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
            ClvmTarget::default(),
            CostArgs { max_cost },
        )
        .map_err(|failure| failure.message)
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
