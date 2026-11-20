//! The worker processes that compile programs, and the driver's side of talking to them.
//!
//! The driver sends one command per line, and the worker answers with one message per line, each
//! a kind followed by JSON:
//!
//! - `run SEED START` compiles the seed's explicit program and its variants from `START` on. The
//!   worker answers `explicit`, then `variant` for each variant, then `done`.
//! - `shrink SEED INDEX BUDGET` shrinks a failing variant, and the worker answers `shrunk`.
//!
//! `progress STAGE` comes before each compile, so the driver's time limit applies to each compile
//! rather than to a whole program. A compile that hangs or kills the worker is recorded against
//! the stage it was in, and the worker is restarted.

use std::io::{BufRead, BufReader, Write};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::mpsc::{Receiver, RecvTimeoutError, channel};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;

use anyhow::{Context, Result, bail};
use serde::{Deserialize, Serialize};

use crate::generate::{Generated, Generator};
use crate::oracle::{self, Category, Compiled, Diag, Outcome, classify, compile_with, millis};
use crate::program::{Program, Variant};
use crate::report::{SeedRecord, VariantRecord};
use crate::shrink;

/// What the worker found for the explicit program.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Explicit {
    Valid {
        time_ms: f64,
        /// See `oracle::fingerprint`. Runs from before this was recorded don't have it.
        #[serde(default)]
        fingerprint: Option<String>,
    },
    Rejected(Diag),
    Panicked(String),
    /// Nothing in the program has generic arguments that can be removed.
    NoSites,
    TimedOut,
    Died(String),
}

/// A failure after shrinking.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Shrunk {
    pub explicit: String,
    pub stripped: String,
    pub shape: String,
    pub tags: Vec<String>,
    /// The shrunk program's own diagnostic, or what went wrong when it ran.
    pub message: String,
}

// The worker side.

pub fn worker() -> Result<()> {
    // Panics in the compiler are caught and reported, so the default message would only add
    // noise. Others are bugs in the fuzzer, which the driver shows from stderr.
    let default_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        if !oracle::is_compiling() {
            default_hook(info);
        }
    }));

    let stdin = std::io::stdin();
    let mut out = std::io::stdout().lock();

    for line in stdin.lock().lines() {
        let line = line?;
        let words: Vec<&str> = line.split_whitespace().collect();
        match words.as_slice() {
            ["run", version, seed, start] => {
                run_command(&mut out, version.parse()?, seed.parse()?, start.parse()?)?;
            }
            ["shrink", version, seed, index, budget] => {
                shrink_command(
                    &mut out,
                    version.parse()?,
                    seed.parse()?,
                    index.parse()?,
                    budget.parse()?,
                )?;
            }
            _ => bail!("unknown command `{line}`"),
        }
    }

    Ok(())
}

fn send(out: &mut impl Write, kind: &str, value: &impl Serialize) -> Result<()> {
    writeln!(out, "{kind} {}", serde_json::to_string(value)?)?;
    out.flush()?;
    Ok(())
}

fn progress(out: &mut impl Write, stage: &str) -> Result<()> {
    writeln!(out, "progress {stage}")?;
    out.flush()?;
    Ok(())
}

pub fn run_command(out: &mut impl Write, version: u32, seed: u64, start: usize) -> Result<()> {
    let generated = Generator::versioned(seed, version).generate();
    let std = generated.program.uses_std();
    let compile = |source: &str| compile_with(source, std);
    let explicit_source = generated.program.print(&Variant::default());

    if generated.variants.is_empty() {
        send(out, "explicit", &Explicit::NoSites)?;
        writeln!(out, "done")?;
        out.flush()?;
        return Ok(());
    }

    progress(out, "explicit")?;
    let explicit = compile(&explicit_source);

    let status = match &explicit {
        Compiled::Ok {
            time, fingerprint, ..
        } => Explicit::Valid {
            time_ms: millis(*time),
            fingerprint: Some(fingerprint.clone()),
        },
        Compiled::Rejected { diag, .. } => Explicit::Rejected(diag.clone()),
        Compiled::Panicked { message } => Explicit::Panicked(message.clone()),
    };
    let valid = matches!(status, Explicit::Valid { .. });
    send(out, "explicit", &status)?;

    if valid {
        for (index, (name, variant)) in generated.variants.iter().enumerate().skip(start) {
            let source = generated.program.print(variant);
            progress(out, &index.to_string())?;
            let stripped = compile(&source);
            // Each compile of the recheck gets its own time limit too. An error writing the
            // progress shows up when the outcome is sent.
            let stage = index.to_string();
            let outcome = classify(&explicit, &stripped, || {
                let _ = progress(out, &stage);
                let explicit = compile(&explicit_source);
                let _ = progress(out, &stage);
                (explicit, compile(&source))
            });
            let outcome = probe_outcome(&generated.program, name, outcome);
            send(out, "variant", &(index, outcome))?;
        }
    }

    writeln!(out, "done")?;
    out.flush()?;
    Ok(())
}

fn shrink_command(
    out: &mut impl Write,
    version: u32,
    seed: u64,
    index: usize,
    budget: usize,
) -> Result<()> {
    let Generated {
        program, variants, ..
    } = Generator::versioned(seed, version).generate();
    // Shrinking can remove the call of a function from the standard library
    let compile_program = |program: &Program, variant: &Variant| {
        compile_with(&program.print(variant), program.uses_std())
    };
    let variant = variants[index].1.clone();

    let explicit = compile_program(&program, &Variant::default());
    let stripped = compile_program(&program, &variant);
    let original = classify(&explicit, &stripped, || {
        (explicit.clone(), stripped.clone())
    });

    // Most candidates only change the variant, so the explicit program is only compiled again
    // when it changes
    let mut last_explicit: Option<(String, Option<oracle::Run>)> = None;
    let mut io_error = None;

    let mut fails = |program: &Program, variant: &Variant| {
        if let Err(error) = progress(out, "shrink") {
            io_error = Some(error);
            return false;
        }

        let source = program.print(&Variant::default());
        if last_explicit
            .as_ref()
            .is_none_or(|(last, _)| *last != source)
        {
            let run = match compile_with(&source, program.uses_std()) {
                Compiled::Ok { run, .. } => Some(run),
                _ => None,
            };
            last_explicit = Some((source, run));
        }
        let Some((_, Some(expected))) = &last_explicit else {
            return false;
        };

        same_failure(&original, expected, &compile_program(program, variant))
    };

    let (program, variant) = shrink::shrink(program, variant, budget, &mut fails);

    if let Some(error) = io_error {
        return Err(error);
    }

    let message = match compile_program(&program, &variant) {
        Compiled::Rejected { diag, .. } => diag.located(),
        Compiled::Panicked { message } => message,
        Compiled::Ok {
            leak: Some(leak), ..
        } => leak,
        Compiled::Ok { run, .. } => format!("{run:?}"),
    };

    let shrunk = Shrunk {
        explicit: program.print(&Variant::default()),
        stripped: program.print(&variant),
        shape: shrink::shape(&program, &variant),
        tags: shrink::tags(&program, &variant),
        message,
    };
    send(out, "shrunk", &shrunk)?;
    Ok(())
}

/// The outcome of a variant, counting the `probe` variant's rejection as inferred when it's that
/// a probe's generic can't be inferred, which is right where nothing gives it a type (see
/// `Generator::probe`). Any other rejection is a failure, as it is for any other variant, since a
/// probe whose arguments give its generics a type must compile.
pub fn probe_outcome(program: &Program, name: &str, mut outcome: Outcome) -> Outcome {
    let callees = program.probe_callees();
    let expected = outcome.diag.as_ref().is_some_and(|diag| {
        diag.kind == "CannotInferGeneric"
            && callees
                .iter()
                .any(|callee| diag.message.contains(&format!(" for `{callee}`")))
    });

    if name == "probe" && outcome.category == Category::Rejected && expected {
        outcome.category = Category::Inferred;
        outcome.detail = outcome
            .diag
            .take()
            .map(|diag| format!("rejected: {}", diag.located()));
    }

    outcome
}

/// Whether a shrunk candidate fails like the original: the same diagnostic for a rejection, a
/// different output for an unsound result, a leaked generic for a leak, or the same panic.
pub fn same_failure(original: &Outcome, expected: &oracle::Run, stripped: &Compiled) -> bool {
    match (original.category, stripped) {
        (Category::Rejected, Compiled::Rejected { diag, .. }) => original
            .diag
            .as_ref()
            .is_some_and(|target| diag.same_failure(target)),
        (Category::Unsound, Compiled::Ok { run, .. }) => run != expected,
        // Any leak counts, since shrinking renames nothing but can remove the binding it was in
        (Category::Leaked, Compiled::Ok { run, leak, .. }) => run == expected && leak.is_some(),
        (Category::Crashed, Compiled::Panicked { message }) => {
            Some(message) == original.detail.as_ref()
        }
        _ => false,
    }
}

// The driver side.

pub enum Message {
    Line(String),
    TimedOut,
    Died(String),
}

/// How the driver talks to a worker: a worker process, or a script in tests.
pub trait Connection {
    fn send(&mut self, command: &str) -> Result<()>;
    fn recv(&mut self, timeout: Duration) -> Message;
    /// Kills the worker and starts a new one.
    fn restart(&mut self) -> Result<()>;
}

/// A worker process, and the lines it has written.
pub struct Worker {
    child: Child,
    stdin: ChildStdin,
    lines: Receiver<String>,
    stderr: Arc<Mutex<Vec<String>>>,
}

impl Worker {
    pub fn spawn() -> Result<Self> {
        let mut child = Command::new(std::env::current_exe()?)
            .arg("--worker")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;

        let stdin = child.stdin.take().unwrap();
        let stdout = child.stdout.take().unwrap();
        let stderr_pipe = child.stderr.take().unwrap();

        let (sender, lines) = channel();
        thread::spawn(move || {
            for line in BufReader::new(stdout).lines().map_while(Result::ok) {
                if sender.send(line).is_err() {
                    break;
                }
            }
        });

        // The last few lines, which say why the worker died
        let stderr = Arc::new(Mutex::new(Vec::new()));
        let stderr_lines = stderr.clone();
        thread::spawn(move || {
            for line in BufReader::new(stderr_pipe).lines().map_while(Result::ok) {
                let mut lines = stderr_lines.lock().unwrap();
                lines.push(line);
                let excess = lines.len().saturating_sub(5);
                lines.drain(..excess);
            }
        });

        Ok(Self {
            child,
            stdin,
            lines,
            stderr,
        })
    }
}

impl Connection for Worker {
    fn send(&mut self, command: &str) -> Result<()> {
        writeln!(self.stdin, "{command}")?;
        self.stdin.flush()?;
        Ok(())
    }

    fn recv(&mut self, timeout: Duration) -> Message {
        match self.lines.recv_timeout(timeout) {
            Ok(line) => Message::Line(line),
            Err(RecvTimeoutError::Timeout) => Message::TimedOut,
            Err(RecvTimeoutError::Disconnected) => {
                let _ = self.child.wait();
                // Give the stderr reader a moment to collect the last lines
                thread::sleep(Duration::from_millis(50));
                Message::Died(self.stderr.lock().unwrap().join("\n"))
            }
        }
    }

    fn restart(&mut self) -> Result<()> {
        let _ = self.child.kill();
        let _ = self.child.wait();
        *self = Self::spawn()?;
        Ok(())
    }
}

impl Drop for Worker {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

fn parse_line(line: &str) -> (&str, &str) {
    line.split_once(' ').unwrap_or((line, ""))
}

/// Where the worker was when it hung or died.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Stage {
    Explicit,
    Variant(usize),
}

/// Checks a seed's program and variants on a worker. A variant that hangs or kills the worker
/// is recorded as slow or crashed, and the rest are checked on a new worker.
pub fn run_seed(
    worker: &mut impl Connection,
    seed: u64,
    generated: &Generated,
    timeout: Duration,
) -> Result<SeedRecord> {
    let mut explicit = None;
    let mut outcomes: Vec<Option<Outcome>> = vec![None; generated.variants.len()];
    let mut start = 0;

    'run: loop {
        worker.send(&format!("run {} {seed} {start}", generated.version))?;
        // A worker that dies before it says anything dies on the explicit program
        let mut stage = Stage::Explicit;

        loop {
            let failure = match worker.recv(timeout) {
                Message::Line(line) => {
                    match parse_line(&line) {
                        ("progress", "explicit") => stage = Stage::Explicit,
                        ("progress", index) => stage = Stage::Variant(index.parse()?),
                        ("explicit", json) => explicit = Some(serde_json::from_str(json)?),
                        ("variant", json) => {
                            let (index, outcome): (usize, Outcome) = serde_json::from_str(json)?;
                            outcomes[index] = Some(outcome);
                        }
                        ("done", _) => break 'run,
                        _ => bail!("unexpected message from worker: {line}"),
                    }
                    continue;
                }
                failure => failure,
            };

            worker.restart()?;

            match stage {
                Stage::Explicit => {
                    explicit = Some(match failure {
                        Message::Died(stderr) => Explicit::Died(stderr),
                        _ => Explicit::TimedOut,
                    });
                    break 'run;
                }
                Stage::Variant(index) => {
                    outcomes[index] = Some(match failure {
                        Message::Died(stderr) => Outcome::failed(
                            Category::Crashed,
                            format!("worker died: {stderr}"),
                            0.0,
                        ),
                        _ => Outcome::failed(
                            Category::Slow,
                            format!("timed out after {} s", timeout.as_secs()),
                            millis(timeout),
                        ),
                    });
                    start = index + 1;
                    if start == outcomes.len() {
                        break 'run;
                    }
                    continue 'run;
                }
            }
        }
    }

    let explicit = explicit.context("worker didn't report the explicit program")?;
    let mut variants = Vec::new();

    if matches!(explicit, Explicit::Valid { .. }) {
        for ((name, variant), outcome) in generated.variants.iter().zip(outcomes) {
            variants.push(VariantRecord {
                name: name.clone(),
                source: generated.program.print(variant),
                shape: shrink::shape(&generated.program, variant),
                tags: shrink::tags(&generated.program, variant),
                outcome: outcome.context("worker skipped a variant")?,
            });
        }
    }

    Ok(SeedRecord {
        seed,
        generator: generated.version,
        explicit_source: generated.program.print(&Variant::default()),
        explicit,
        variants,
    })
}

/// How many candidates shrinking a failure may check, each with up to two compiles.
pub const SHRINK_BUDGET: usize = 3000;

/// Shrinks a failing variant on a worker, or gives up if it hangs or dies.
pub fn shrink_on(
    worker: &mut impl Connection,
    version: u32,
    seed: u64,
    index: usize,
    timeout: Duration,
) -> Result<Option<Shrunk>> {
    worker.send(&format!("shrink {version} {seed} {index} {SHRINK_BUDGET}"))?;
    loop {
        match worker.recv(timeout) {
            Message::Line(line) => match parse_line(&line) {
                ("progress", _) => {}
                ("shrunk", json) => return Ok(Some(serde_json::from_str(json)?)),
                _ => bail!("unexpected message from worker: {line}"),
            },
            Message::TimedOut => {
                eprintln!("Shrinking seed {seed} variant {index} timed out");
                worker.restart()?;
                return Ok(None);
            }
            Message::Died(stderr) => {
                eprintln!("Shrinking seed {seed} variant {index} crashed the worker: {stderr}");
                worker.restart()?;
                return Ok(None);
            }
        }
    }
}

/// Runs `job` on each item with `threads` worker processes, and returns the results in order.
pub fn parallel<T: Sync, R: Send>(
    items: &[T],
    threads: usize,
    job: impl Fn(&mut Worker, &T) -> Result<R> + Sync,
) -> Result<Vec<R>> {
    let next = AtomicUsize::new(0);
    let results = Mutex::new(Vec::new());

    thread::scope(|scope| -> Result<()> {
        let handles: Vec<_> = (0..threads.max(1))
            .map(|_| {
                scope.spawn(|| -> Result<()> {
                    let mut worker = Worker::spawn()?;
                    loop {
                        let index = next.fetch_add(1, Ordering::Relaxed);
                        let Some(item) = items.get(index) else {
                            return Ok(());
                        };
                        let result = job(&mut worker, item)?;
                        let mut results = results.lock().unwrap();
                        results.push((index, result));
                        if results.len() % 250 == 0 {
                            eprintln!("{} of {}", results.len(), items.len());
                        }
                    }
                })
            })
            .collect();
        for handle in handles {
            handle.join().unwrap()?;
        }
        Ok(())
    })?;

    let mut results = results.into_inner().unwrap();
    results.sort_by_key(|(index, _)| *index);
    Ok(results.into_iter().map(|(_, result)| result).collect())
}
