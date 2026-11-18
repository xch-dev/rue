//! Compiles a program, runs its `main`, and classifies what inference did with a variant.

use std::cell::Cell;
use std::fmt::Write as _;
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::path::PathBuf;
use std::sync::Arc;
use std::time::{Duration, Instant};

use clvmr::error::EvalErr;
use clvmr::{Allocator, ChiaDialect, NodePtr, run_program};
use rue_clvm::{disassemble_program, disassemble_value};
use rue_compiler::{Compiler, File, FileTree};
use rue_diagnostic::{Diagnostic, DiagnosticSeverity, Source, SourceKind};
use rue_lir::CLVM_FLAGS;
use rue_options::CompilerOptions;
use serde::{Deserialize, Serialize};

/// The cost limit for running `main`. Generated programs are small, and recursion is bounded
/// by a depth of at most 2, so this is only reached by a miscompilation.
const MAX_COST: u64 = 10_000_000;

/// A compile is slow if it takes this many times as long as the explicit program's...
pub const SLOW_FACTOR: f64 = 10.0;
/// ...and at least this long, so that noise in compiles that take a few milliseconds isn't
/// counted.
pub const SLOW_FLOOR: Duration = Duration::from_millis(250);

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Diag {
    /// The name of the `DiagnosticKind` variant, such as `CannotInferGeneric`.
    pub kind: String,
    /// The message, without its location.
    pub message: String,
    pub line_col: String,
}

impl Diag {
    /// The message with the contents of each backticked span replaced by `_`, so that failures
    /// that differ only in the types or names involved are grouped together.
    pub fn template(&self) -> String {
        let mut out = String::new();
        for (index, part) in self.message.split('`').enumerate() {
            out.push_str(if index % 2 == 1 { "`_`" } else { part });
        }
        out
    }

    /// The message and where it is, such as `... at 3:5`.
    pub fn located(&self) -> String {
        if self.line_col.is_empty() {
            self.message.clone()
        } else {
            format!("{} at {}", self.message, self.line_col)
        }
    }

    /// The key that groups failures: the template and the kind, such as
    /// ``Cannot infer generic `_` for `_`; ... (CannotInferGeneric)``.
    pub fn key(&self) -> String {
        format!("{} ({})", self.template(), self.kind)
    }

    /// Whether a shrunk program fails in the same way. The types in a message change as a
    /// program shrinks, so they're compared by template. A `CannotInferGeneric` names a generic
    /// and a function, which shrinking never renames, so it must name the same ones, or shrinking
    /// could swap one failing call for another.
    pub fn same_failure(&self, other: &Diag) -> bool {
        self.kind == other.kind
            && if self.kind == "CannotInferGeneric" {
                self.message == other.message
            } else {
                self.template() == other.template()
            }
    }
}

/// The result of running `main`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Run {
    Output(String),
    Error(String),
}

#[derive(Debug, Clone)]
pub enum Compiled {
    Ok {
        clvm: String,
        run: Run,
        time: Duration,
        /// See `fingerprint`.
        fingerprint: String,
    },
    Rejected {
        diag: Diag,
        time: Duration,
        /// See `fingerprint`.
        fingerprint: String,
    },
    /// The compiler panicked.
    Panicked { message: String },
}

impl Compiled {
    pub fn time(&self) -> Duration {
        match self {
            Compiled::Ok { time, .. } | Compiled::Rejected { time, .. } => *time,
            Compiled::Panicked { .. } => Duration::ZERO,
        }
    }
}

thread_local! {
    static COMPILING: Cell<bool> = const { Cell::new(false) };
}

/// Whether this thread is compiling, so that a panic is the compiler's.
pub fn is_compiling() -> bool {
    COMPILING.get()
}

/// Compiles the source without the standard library, and runs `main` if there are no errors.
pub fn compile(source: &str) -> Compiled {
    let start = Instant::now();
    COMPILING.set(true);
    let result = catch_unwind(AssertUnwindSafe(|| compile_inner(source)));
    COMPILING.set(false);
    let time = start.elapsed();

    match result {
        Ok((Ok((clvm, run)), fingerprint)) => Compiled::Ok {
            clvm,
            run,
            time,
            fingerprint,
        },
        Ok((Err(diag), fingerprint)) => Compiled::Rejected {
            diag,
            time,
            fingerprint,
        },
        Err(payload) => {
            let message = payload
                .downcast_ref::<&str>()
                .map(ToString::to_string)
                .or_else(|| payload.downcast_ref::<String>().cloned())
                .unwrap_or_else(|| "unknown panic".to_string());
            Compiled::Panicked { message }
        }
    }
}

/// A hash of everything a compile reports: every diagnostic (warnings included, with its
/// severity, message and position) and, if it compiled, the CLVM. Two compiles with the same
/// fingerprint behaved the same as far as a user can see, so a change that should keep inference
/// as it was can be checked variant by variant (`compare.rs`). It's FNV-1a, which is stable across
/// builds and toolchains.
pub fn fingerprint(diagnostics: &[Diagnostic], clvm: Option<&str>) -> String {
    let mut text = String::new();

    for diagnostic in diagnostics {
        let _ = writeln!(
            text,
            "{:?} {} at {}",
            diagnostic.severity,
            diagnostic.kind,
            diagnostic.start()
        );
    }

    if let Some(clvm) = clvm {
        text.push_str(clvm);
    }

    let mut hash: u64 = 0xcbf2_9ce4_8422_2325;

    for byte in text.bytes() {
        hash ^= u64::from(byte);
        hash = hash.wrapping_mul(0x0100_0000_01b3);
    }

    format!("{hash:016x}")
}

fn compile_inner(source: &str) -> (Result<(String, Run), Diag>, String) {
    let mut ctx = Compiler::new(CompilerOptions {
        std: false,
        ..CompilerOptions::default()
    });

    let kind = SourceKind::File("main.rue".to_string());
    let file = File::new(
        &mut ctx,
        "main".to_string(),
        Source::new(Arc::from(source), kind.clone()),
    );
    let tree = FileTree::File(file);
    tree.compile(&mut ctx);

    let diagnostics = ctx.take_diagnostics();

    let result = compile_checked(&mut ctx, &tree, &kind, &diagnostics);
    let clvm = result.as_ref().ok().map(|(clvm, _)| clvm.as_str());
    let fingerprint = fingerprint(&diagnostics, clvm);
    (result, fingerprint)
}

fn compile_checked(
    ctx: &mut Compiler,
    tree: &FileTree,
    kind: &SourceKind,
    diagnostics: &[Diagnostic],
) -> Result<(String, Run), Diag> {
    if let Some(error) = diagnostics
        .iter()
        .find(|diagnostic| diagnostic.severity == DiagnosticSeverity::Error)
    {
        let debug = format!("{:?}", error.kind);
        let kind_name = debug
            .split(|c: char| !c.is_alphanumeric())
            .next()
            .unwrap_or_default()
            .to_string();
        return Err(Diag {
            kind: kind_name,
            message: error.kind.to_string(),
            line_col: error.start().to_string(),
        });
    }

    let mut allocator = Allocator::new();

    let ptr = match tree.main(ctx, &mut allocator, kind, PathBuf::from(".")) {
        Ok(Some(ptr)) => ptr,
        Ok(None) => unreachable!("generated programs have a `main`"),
        Err(error) => {
            return Err(Diag {
                kind: "Codegen".to_string(),
                message: error.to_string(),
                line_col: String::new(),
            });
        }
    };

    let clvm = disassemble_program(&allocator, ptr);
    let run = run(&mut allocator, ptr);
    Ok((clvm, run))
}

fn run(allocator: &mut Allocator, ptr: NodePtr) -> Run {
    let env = allocator.nil();
    match run_program(allocator, &ChiaDialect::new(CLVM_FLAGS), ptr, env, MAX_COST) {
        Ok(reduction) => Run::Output(disassemble_value(allocator, reduction.1)),
        Err(EvalErr::Raise(ptr)) => {
            Run::Error(format!("raise {}", disassemble_value(allocator, ptr)))
        }
        Err(error) => Run::Error(error.to_string()),
    }
}

/// The four result categories from #341, and compiler panics.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum Category {
    /// It compiles, and runs the same as the explicit program.
    Inferred,
    /// It doesn't compile, although the explicit arguments are a valid instantiation.
    Rejected,
    /// It compiles, but runs differently from the explicit program.
    Unsound,
    /// Compiling it takes much longer than the explicit program, or times out.
    Slow,
    /// The compiler panicked, or the worker process died.
    Crashed,
}

impl Category {
    pub const ALL: [Category; 5] = [
        Category::Inferred,
        Category::Rejected,
        Category::Unsound,
        Category::Slow,
        Category::Crashed,
    ];
}

/// What a variant did, compared with the explicit program.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Outcome {
    pub category: Category,
    pub diag: Option<Diag>,
    /// Whether the CLVM is the same as the explicit program's, for variants that were inferred.
    pub same_clvm: bool,
    /// The outputs, the panic message, or the times, depending on the category.
    pub detail: Option<String>,
    pub time_ms: f64,
    /// What `main` gave, for a variant that compiled. Runs from before this was recorded don't
    /// have it.
    #[serde(default)]
    pub output: Option<Run>,
    /// The stripped program's `fingerprint`, unless it crashed or hung. Runs from before this was
    /// recorded don't have it.
    #[serde(default)]
    pub fingerprint: Option<String>,
}

impl Outcome {
    /// A variant that hung or killed the worker, which the driver records.
    pub fn failed(category: Category, detail: String, time_ms: f64) -> Self {
        Self {
            category,
            diag: None,
            same_clvm: false,
            detail: Some(detail),
            time_ms,
            output: None,
            fingerprint: None,
        }
    }
}

pub fn millis(time: Duration) -> f64 {
    time.as_secs_f64() * 1000.0
}

/// Classifies a variant. `recompile` compiles both programs again, to check that a slow compile
/// wasn't noise.
pub fn classify(
    explicit: &Compiled,
    stripped: &Compiled,
    recompile: impl FnOnce() -> (Compiled, Compiled),
) -> Outcome {
    let Compiled::Ok {
        clvm: explicit_clvm,
        run: explicit_run,
        time: explicit_time,
        ..
    } = explicit
    else {
        unreachable!("only valid programs are classified");
    };

    let output = match stripped {
        Compiled::Ok { run, .. } => Some(run.clone()),
        _ => None,
    };
    let fingerprint = match stripped {
        Compiled::Ok { fingerprint, .. } | Compiled::Rejected { fingerprint, .. } => {
            Some(fingerprint.clone())
        }
        Compiled::Panicked { .. } => None,
    };
    let outcome = |category, diag, same_clvm, detail| Outcome {
        category,
        diag,
        same_clvm,
        detail,
        time_ms: millis(stripped.time()),
        output: output.clone(),
        fingerprint: fingerprint.clone(),
    };

    if let Compiled::Panicked { message } = stripped {
        return outcome(Category::Crashed, None, false, Some(message.clone()));
    }

    if is_slow(*explicit_time, stripped.time()) {
        let (explicit_again, stripped_again) = recompile();
        if is_slow(explicit_again.time(), stripped_again.time()) {
            let detail = format!(
                "{:.1} ms, explicit {:.1} ms",
                millis(stripped_again.time()),
                millis(explicit_again.time())
            );
            let diag = match stripped {
                Compiled::Rejected { diag, .. } => Some(diag.clone()),
                _ => None,
            };
            return outcome(Category::Slow, diag, false, Some(detail));
        }
    }

    match stripped {
        Compiled::Rejected { diag, .. } => {
            outcome(Category::Rejected, Some(diag.clone()), false, None)
        }
        Compiled::Ok { clvm, run, .. } => {
            if run == explicit_run {
                outcome(Category::Inferred, None, clvm == explicit_clvm, None)
            } else {
                let detail = format!("explicit: {explicit_run:?}, inferred: {run:?}");
                outcome(Category::Unsound, None, false, Some(detail))
            }
        }
        Compiled::Panicked { .. } => unreachable!(),
    }
}

fn is_slow(explicit: Duration, stripped: Duration) -> bool {
    stripped >= SLOW_FLOOR && stripped.as_secs_f64() >= explicit.as_secs_f64() * SLOW_FACTOR
}
