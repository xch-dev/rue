//! A differential fuzzer for generic inference (#341).
//!
//! It generates random programs that compile with every generic argument written out, removes
//! some or all of the generic arguments, and checks what inference does with them:
//!
//! - **Inferred**: it compiles, and `main` gives the same output as the explicit program.
//! - **Rejected**: it doesn't compile, although the explicit arguments are a valid instantiation.
//! - **Unsound**: it compiles, but `main` gives a different output, or fails where the explicit
//!   program didn't. This should never happen.
//! - **Slow**: compiling takes more than 10 times as long as the explicit program (and at least
//!   250 ms), or times out.
//! - **Crashed**: the compiler panicked, or its process died.
//!
//! Each program is checked in three variants: every removable generic argument removed (`all`),
//! a random subset of them (`some`, sometimes written as `_`), and the arguments of one site
//! (`one`). Failures are grouped by category and the first diagnostic, and a few of each group are
//! shrunk to a small reproduction.
//!
//! Run it in release mode, with a seed range:
//!
//! ```text
//! cargo run --release -p rue-tests --bin infer-fuzz -- --seeds 0..3000 --threads 3 --out <dir>
//! ```
//!
//! Options:
//!
//! - `--seeds A..B`: the seeds to generate programs from (default `0..1000`).
//! - `--threads N`: how many programs to compile at once, each in its own worker process
//!   (default 2).
//! - `--timeout S`: the time limit in seconds for each compile (default 10). A compile that takes
//!   longer is killed and counted as slow.
//! - `--shrink N`: how many failures of each group to shrink (default 3, 0 to skip).
//! - `--out DIR`: also write `results.jsonl` (every program, its variants and their results),
//!   `report.md` (the summary) and the shrunk reproductions under `clusters/`.
//! - `--print SEED`: print the explicit program and the variants for one seed, and exit.

mod generate;
mod oracle;
mod program;
mod shrink;

use std::collections::BTreeMap;
use std::fmt::Write as _;
use std::fs;
use std::io::{BufRead, BufReader, Write as _};
use std::path::PathBuf;
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::mpsc::{Receiver, RecvTimeoutError, channel};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;

use anyhow::{Context, Result, bail};
use serde::{Deserialize, Serialize};

use crate::generate::{Generated, Generator};
use crate::oracle::{Category, Compiled, Diag, Outcome, classify, compile, millis};
use crate::program::Variant;

#[derive(Debug, Clone)]
struct Args {
    seeds: std::ops::Range<u64>,
    threads: usize,
    timeout: Duration,
    shrink: usize,
    out: Option<PathBuf>,
}

fn parse_range(text: &str) -> Result<std::ops::Range<u64>> {
    let (start, end) = text
        .split_once("..")
        .context("expected a seed range such as `0..3000`")?;
    Ok(start.parse()?..end.parse()?)
}

fn main() -> Result<()> {
    let mut args = Args {
        seeds: 0..1000,
        threads: 2,
        timeout: Duration::from_secs(10),
        shrink: 3,
        out: None,
    };

    let mut iter = std::env::args().skip(1);
    while let Some(arg) = iter.next() {
        let mut value = || {
            iter.next()
                .with_context(|| format!("`{arg}` needs a value"))
        };
        match arg.as_str() {
            "--worker" => return run_in_large_stack(worker),
            "--print" => return print_seed(value()?.parse()?),
            "--seeds" => args.seeds = parse_range(&value()?)?,
            "--threads" => args.threads = value()?.parse()?,
            "--timeout" => args.timeout = Duration::from_secs(value()?.parse()?),
            "--shrink" => args.shrink = value()?.parse()?,
            "--out" => args.out = Some(PathBuf::from(value()?)),
            _ => bail!("unknown argument `{arg}`; see the doc comment in infer-fuzz/main.rs"),
        }
    }

    drive(&args)
}

/// Compiles in a thread with a large stack, since deeply nested types recurse deeply.
fn run_in_large_stack(f: fn() -> Result<()>) -> Result<()> {
    thread::Builder::new()
        .stack_size(256 * 1024 * 1024)
        .spawn(f)?
        .join()
        .unwrap_or_else(|_| bail!("worker thread panicked"))
}

fn print_seed(seed: u64) -> Result<()> {
    let generated = Generator::new(seed).generate();
    println!(
        "// seed {seed}, explicit\n{}",
        generated.program.print(&Variant::default())
    );
    for (name, variant) in &generated.variants {
        println!(
            "// variant `{name}`: {}\n{}",
            shrink::shape(&generated.program, variant),
            generated.program.print(variant)
        );
    }
    Ok(())
}

// The worker side. The driver sends one command per line, and the worker answers with one
// message per line. `progress` comes before each compile, so the driver's time limit applies to
// each compile rather than to a whole program.

/// What the worker found for the explicit program.
#[derive(Debug, Clone, Serialize, Deserialize)]
enum Explicit {
    Valid {
        time_ms: f64,
    },
    Rejected(Diag),
    Panicked(String),
    /// Nothing in the program has generic arguments that can be removed.
    NoSites,
    TimedOut,
    Died(String),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Shrunk {
    explicit: String,
    stripped: String,
    shape: String,
    tags: Vec<String>,
}

fn worker() -> Result<()> {
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
            ["run", seed, start] => run_command(&mut out, seed.parse()?, start.parse()?)?,
            ["shrink", seed, index, budget] => {
                shrink_command(&mut out, seed.parse()?, index.parse()?, budget.parse()?)?;
            }
            _ => bail!("unknown command `{line}`"),
        }
    }

    Ok(())
}

fn send(out: &mut impl std::io::Write, kind: &str, value: &impl Serialize) -> Result<()> {
    writeln!(out, "{kind} {}", serde_json::to_string(value)?)?;
    out.flush()?;
    Ok(())
}

fn progress(out: &mut impl std::io::Write, stage: &str) -> Result<()> {
    writeln!(out, "progress {stage}")?;
    out.flush()?;
    Ok(())
}

fn run_command(out: &mut impl std::io::Write, seed: u64, start: usize) -> Result<()> {
    let generated = Generator::new(seed).generate();
    let explicit_source = generated.program.print(&Variant::default());

    progress(out, "explicit")?;
    let explicit = compile(&explicit_source);

    let status = match &explicit {
        _ if generated.variants.is_empty() => Explicit::NoSites,
        Compiled::Ok { time, .. } => Explicit::Valid {
            time_ms: millis(*time),
        },
        Compiled::Rejected { diag, .. } => Explicit::Rejected(diag.clone()),
        Compiled::Panicked { message } => Explicit::Panicked(message.clone()),
    };
    let valid = matches!(status, Explicit::Valid { .. });
    send(out, "explicit", &status)?;

    if valid {
        for (index, (_, variant)) in generated.variants.iter().enumerate().skip(start) {
            let source = generated.program.print(variant);
            progress(out, &index.to_string())?;
            let stripped = compile(&source);
            let outcome = classify(&explicit, &stripped, || {
                (compile(&explicit_source), compile(&source))
            });
            send(out, "variant", &(index, outcome))?;
        }
    }

    writeln!(out, "done")?;
    out.flush()?;
    Ok(())
}

fn shrink_command(
    out: &mut impl std::io::Write,
    seed: u64,
    index: usize,
    budget: usize,
) -> Result<()> {
    let Generated {
        program, variants, ..
    } = Generator::new(seed).generate();
    let variant = variants[index].1.clone();

    let explicit = compile(&program.print(&Variant::default()));
    let stripped = compile(&program.print(&variant));
    let original = classify(&explicit, &stripped, || {
        (explicit.clone(), stripped.clone())
    });

    let mut io_error = None;
    let mut fails = |program: &program::Program, variant: &Variant| {
        if let Err(error) = progress(out, "shrink") {
            io_error = Some(error);
            return false;
        }
        let Compiled::Ok { run: expected, .. } = compile(&program.print(&Variant::default()))
        else {
            return false;
        };
        match (original.category, compile(&program.print(variant))) {
            (Category::Rejected, Compiled::Rejected { diag, .. }) => {
                let target = original.diag.as_ref().unwrap();
                diag.kind == target.kind && diag.template() == target.template()
            }
            (Category::Unsound, Compiled::Ok { run, .. }) => run != expected,
            (Category::Crashed, Compiled::Panicked { message }) => {
                Some(&message) == original.detail.as_ref()
            }
            _ => false,
        }
    };

    let (program, variant) = shrink::shrink(program, variant, budget, &mut fails);

    if let Some(error) = io_error {
        return Err(error);
    }

    let shrunk = Shrunk {
        explicit: program.print(&Variant::default()),
        stripped: program.print(&variant),
        shape: shrink::shape(&program, &variant),
        tags: shrink::tags(&program, &variant)
            .into_iter()
            .map(ToString::to_string)
            .collect(),
    };
    send(out, "shrunk", &shrunk)?;
    Ok(())
}

// The driver side.

/// A worker process, and the lines it has written.
struct Worker {
    child: Child,
    stdin: ChildStdin,
    lines: Receiver<String>,
    stderr: Arc<Mutex<Vec<String>>>,
}

enum Message {
    Line(String),
    TimedOut,
    Died(String),
}

impl Worker {
    fn spawn() -> Result<Self> {
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

    /// Kills the worker and starts a new one.
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

#[derive(Debug, Clone, Serialize, Deserialize)]
struct VariantRecord {
    name: String,
    source: String,
    shape: String,
    outcome: Outcome,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct SeedRecord {
    seed: u64,
    explicit_source: String,
    explicit: Explicit,
    variants: Vec<VariantRecord>,
}

fn timed_out(timeout: Duration) -> Outcome {
    Outcome {
        category: Category::Slow,
        diag: None,
        same_clvm: false,
        detail: Some(format!("timed out after {} s", timeout.as_secs())),
        time_ms: millis(timeout),
    }
}

fn died(stderr: String) -> Outcome {
    Outcome {
        category: Category::Crashed,
        diag: None,
        same_clvm: false,
        detail: Some(format!("worker died: {stderr}")),
        time_ms: 0.0,
    }
}

fn run_seed(worker: &mut Worker, seed: u64, timeout: Duration) -> Result<SeedRecord> {
    let generated = Generator::new(seed).generate();
    let mut record = SeedRecord {
        seed,
        explicit_source: generated.program.print(&Variant::default()),
        explicit: Explicit::NoSites,
        variants: Vec::new(),
    };

    let mut outcomes: Vec<Option<Outcome>> = vec![None; generated.variants.len()];
    let mut start = 0;

    'run: while start <= outcomes.len() {
        worker.send(&format!("run {seed} {start}"))?;
        let mut stage = String::new();

        loop {
            let failure = match worker.recv(timeout) {
                Message::Line(line) => {
                    let (kind, rest) = line.split_once(' ').unwrap_or((&line, ""));
                    match kind {
                        "progress" => stage = rest.to_string(),
                        "explicit" => {
                            record.explicit = serde_json::from_str(rest)?;
                        }
                        "variant" => {
                            let (index, outcome): (usize, Outcome) = serde_json::from_str(rest)?;
                            outcomes[index] = Some(outcome);
                        }
                        "done" => break 'run,
                        _ => bail!("unexpected message from worker: {line}"),
                    }
                    continue;
                }
                Message::TimedOut => timed_out(timeout),
                Message::Died(stderr) => died(stderr),
            };

            worker.restart()?;

            if stage == "explicit" {
                record.explicit = if failure.category == Category::Slow {
                    Explicit::TimedOut
                } else {
                    Explicit::Died(failure.detail.unwrap_or_default())
                };
                break 'run;
            }

            let index: usize = stage.parse()?;
            outcomes[index] = Some(failure);
            start = index + 1;
            if start == outcomes.len() {
                break 'run;
            }
            continue 'run;
        }
    }

    if matches!(record.explicit, Explicit::Valid { .. }) {
        for ((name, variant), outcome) in generated.variants.iter().zip(outcomes) {
            record.variants.push(VariantRecord {
                name: name.clone(),
                source: generated.program.print(variant),
                shape: shrink::shape(&generated.program, variant),
                outcome: outcome.context("worker skipped a variant")?,
            });
        }
    }

    Ok(record)
}

/// How many candidates shrinking a failure may check, each with two compiles.
const SHRINK_BUDGET: usize = 3000;

fn shrink_on(
    worker: &mut Worker,
    seed: u64,
    index: usize,
    timeout: Duration,
) -> Result<Option<Shrunk>> {
    worker.send(&format!("shrink {seed} {index} {SHRINK_BUDGET}"))?;
    loop {
        match worker.recv(timeout) {
            Message::Line(line) => {
                let (kind, rest) = line.split_once(' ').unwrap_or((&line, ""));
                match kind {
                    "progress" => {}
                    "shrunk" => return Ok(Some(serde_json::from_str(rest)?)),
                    _ => bail!("unexpected message from worker: {line}"),
                }
            }
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

/// Runs `job` on each item with `threads` workers, and returns the results in order.
fn parallel<T: Sync, R: Send>(
    items: &[T],
    threads: usize,
    job: impl Fn(&mut Worker, &T) -> Result<R> + Sync,
) -> Result<Vec<R>> {
    let next = AtomicU64::new(0);
    let results = Mutex::new(Vec::new());

    thread::scope(|scope| -> Result<()> {
        let handles: Vec<_> = (0..threads.max(1))
            .map(|_| {
                scope.spawn(|| -> Result<()> {
                    let mut worker = Worker::spawn()?;
                    loop {
                        let index = next.fetch_add(1, Ordering::Relaxed) as usize;
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

/// A group of failures with the same category and first diagnostic.
#[derive(Debug, Default)]
struct Cluster {
    /// The failing variants, as (seed, variant index).
    members: Vec<(u64, usize)>,
    /// How many of the `one` variants had each shape.
    shapes: BTreeMap<String, usize>,
    shrunk: Vec<(u64, String, Shrunk)>,
}

fn cluster_key(outcome: &Outcome) -> String {
    let category = format!("{:?}", outcome.category);
    match (&outcome.diag, &outcome.detail) {
        (Some(diag), _) => format!("{category}: {} ({})", diag.template(), diag.kind),
        (None, Some(detail)) if outcome.category == Category::Crashed => {
            format!("{category}: {}", detail.lines().next().unwrap_or_default())
        }
        _ => category,
    }
}

fn drive(args: &Args) -> Result<()> {
    let seeds: Vec<u64> = args.seeds.clone().collect();
    eprintln!(
        "Checking {} programs with {} workers and a {} s time limit per compile",
        seeds.len(),
        args.threads,
        args.timeout.as_secs()
    );

    let records = parallel(&seeds, args.threads, |worker, &seed| {
        run_seed(worker, seed, args.timeout)
    })?;

    let mut clusters: BTreeMap<String, Cluster> = BTreeMap::new();
    for record in &records {
        for (index, variant) in record.variants.iter().enumerate() {
            if variant.outcome.category == Category::Inferred {
                continue;
            }
            let cluster = clusters.entry(cluster_key(&variant.outcome)).or_default();
            cluster.members.push((record.seed, index));
            if variant.name == "one" {
                *cluster.shapes.entry(variant.shape.clone()).or_default() += 1;
            }
        }
    }

    // Shrink a few of each cluster, taking them in turn from each set of tags (see
    // `shrink::tags`), so that the shapes in a large cluster are all sampled. Within a set,
    // `one` variants come first, since a single site is the most specific, and then the shortest
    let mut jobs = Vec::new();
    for (key, cluster) in &clusters {
        let category = cluster_category(&records, cluster);
        if category == Category::Slow {
            continue;
        }
        let mut by_tags: BTreeMap<Vec<&str>, Vec<(u64, usize)>> = BTreeMap::new();
        for &(seed, index) in &cluster.members {
            let generated = Generator::new(seed).generate();
            let tags = shrink::tags(&generated.program, &generated.variants[index].1);
            by_tags
                .entry(tags.into_iter().collect())
                .or_default()
                .push((seed, index));
        }
        let mut groups: Vec<Vec<(u64, usize)>> = by_tags.into_values().collect();
        for group in &mut groups {
            group.sort_by_key(|&(seed, index)| {
                let variant = &records_by_seed(&records, seed).variants[index];
                (variant.name != "one", variant.source.len())
            });
            group.reverse();
        }
        groups.sort_by_key(|group| std::cmp::Reverse(group.len()));

        let limit = if category == Category::Rejected {
            args.shrink
        } else {
            args.shrink.max(10)
        };
        let mut taken = 0;
        while taken < limit && groups.iter().any(|group| !group.is_empty()) {
            for group in &mut groups {
                if taken == limit {
                    break;
                }
                if let Some((seed, index)) = group.pop() {
                    jobs.push((key.clone(), seed, index));
                    taken += 1;
                }
            }
        }
    }

    eprintln!("Shrinking {} failures", jobs.len());
    let shrunk = parallel(&jobs, args.threads, |worker, (key, seed, index)| {
        Ok((
            key.clone(),
            *seed,
            *index,
            shrink_on(worker, *seed, *index, args.timeout)?,
        ))
    })?;
    for (key, seed, index, result) in shrunk {
        let record = records_by_seed(&records, seed);
        let name = &record.variants[index].name;
        let cluster = clusters.get_mut(&key).unwrap();
        let result = result.unwrap_or_else(|| Shrunk {
            explicit: record.explicit_source.clone(),
            stripped: record.variants[index].source.clone(),
            shape: record.variants[index].shape.clone(),
            tags: vec!["not shrunk".to_string()],
        });
        cluster.shrunk.push((seed, name.clone(), result));
    }

    let report = report(&records, &clusters);
    println!("{report}");

    if let Some(out) = &args.out {
        write_outputs(out, &records, &clusters, &report)?;
        eprintln!("Wrote the results to {}", out.display());
    }

    Ok(())
}

fn records_by_seed(records: &[SeedRecord], seed: u64) -> &SeedRecord {
    records.iter().find(|record| record.seed == seed).unwrap()
}

fn cluster_category(records: &[SeedRecord], cluster: &Cluster) -> Category {
    let (seed, index) = cluster.members[0];
    records_by_seed(records, seed).variants[index]
        .outcome
        .category
}

/// The summary: counts for each category, and each cluster with its shapes and reproductions.
fn report(records: &[SeedRecord], clusters: &BTreeMap<String, Cluster>) -> String {
    let mut out = String::new();

    let mut explicit_counts: BTreeMap<String, usize> = BTreeMap::new();
    let mut explicit_rejections: BTreeMap<String, usize> = BTreeMap::new();
    for record in records {
        let name = match &record.explicit {
            Explicit::Valid { .. } => "valid",
            Explicit::Rejected(diag) => {
                *explicit_rejections
                    .entry(format!("{} ({})", diag.template(), diag.kind))
                    .or_default() += 1;
                "explicit program rejected"
            }
            Explicit::Panicked(_) => "explicit program panicked",
            Explicit::NoSites => "no generic arguments to remove",
            Explicit::TimedOut => "explicit program timed out",
            Explicit::Died(_) => "explicit program crashed the worker",
        };
        *explicit_counts.entry(name.to_string()).or_default() += 1;
    }

    let valid = explicit_counts.get("valid").copied().unwrap_or_default();
    let _ = writeln!(out, "# Inference fuzzer results\n");
    let _ = writeln!(
        out,
        "{} seeds, {valid} valid programs (they compile with every generic argument written out).\n",
        records.len()
    );
    for (name, count) in &explicit_counts {
        if name != "valid" {
            let _ = writeln!(out, "- {count} discarded: {name}");
        }
    }
    let _ = writeln!(out);

    let mut table: BTreeMap<String, BTreeMap<Category, usize>> = BTreeMap::new();
    let mut programs: BTreeMap<Category, usize> = BTreeMap::new();
    let mut same_clvm = [0, 0];
    for record in records {
        let mut worst = None;
        for variant in &record.variants {
            let category = variant.outcome.category;
            *table
                .entry(variant.name.clone())
                .or_default()
                .entry(category)
                .or_default() += 1;
            *table
                .entry("total".to_string())
                .or_default()
                .entry(category)
                .or_default() += 1;
            if category == Category::Inferred {
                same_clvm[usize::from(variant.outcome.same_clvm)] += 1;
            }
            worst = worst.max(Some(category));
        }
        if let Some(worst) = worst {
            *programs.entry(worst).or_default() += 1;
        }
    }

    let _ = writeln!(out, "## Counts\n");
    let _ = write!(out, "| variant |");
    for category in Category::ALL {
        let _ = write!(out, " {category:?} |");
    }
    let _ = writeln!(out, "\n|---|---|---|---|---|---|");
    for name in ["all", "some", "one", "total"] {
        let Some(counts) = table.get(name) else {
            continue;
        };
        let _ = write!(out, "| {name} |");
        for category in Category::ALL {
            let _ = write!(
                out,
                " {} |",
                counts.get(&category).copied().unwrap_or_default()
            );
        }
        let _ = writeln!(out);
    }
    let _ = write!(out, "| programs (worst variant) |");
    for category in Category::ALL {
        let _ = write!(
            out,
            " {} |",
            programs.get(&category).copied().unwrap_or_default()
        );
    }
    let _ = writeln!(
        out,
        "\n\nOf the inferred variants, {} compile to the same CLVM as the explicit program, and {} \
         to different CLVM with the same output.\n",
        same_clvm[1], same_clvm[0]
    );

    if !explicit_rejections.is_empty() {
        let _ = writeln!(
            out,
            "Why explicit programs were rejected (generator noise, not inference):\n"
        );
        let mut rejections: Vec<_> = explicit_rejections.into_iter().collect();
        rejections.sort_by(|a, b| b.1.cmp(&a.1));
        for (reason, count) in rejections.iter().take(8) {
            let _ = writeln!(out, "- {count}: {reason}");
        }
        let _ = writeln!(out);
    }

    let _ = writeln!(out, "## Clusters\n");
    let mut sorted: Vec<_> = clusters.iter().collect();
    sorted.sort_by(|a, b| b.1.members.len().cmp(&a.1.members.len()));

    for (number, (key, cluster)) in sorted.into_iter().enumerate() {
        let program_count = {
            let mut seeds: Vec<u64> = cluster.members.iter().map(|(seed, _)| *seed).collect();
            seeds.dedup();
            seeds.len()
        };
        let _ = writeln!(
            out,
            "### {}. {key}\n\n{} variants in {program_count} programs.\n",
            number + 1,
            cluster.members.len()
        );

        if !cluster.shapes.is_empty() {
            let _ = writeln!(out, "Shapes of the single-site variants:\n");
            let mut shapes: Vec<_> = cluster.shapes.iter().collect();
            shapes.sort_by(|a, b| b.1.cmp(a.1));
            for (shape, count) in shapes.iter().take(8) {
                let _ = writeln!(out, "- {count}: `{shape}`");
            }
            if shapes.len() > 8 {
                let _ = writeln!(out, "- ... and {} more", shapes.len() - 8);
            }
            let _ = writeln!(out);
        }

        let mut groups: BTreeMap<String, Vec<&(u64, String, Shrunk)>> = BTreeMap::new();
        for shrunk in &cluster.shrunk {
            groups
                .entry(shrunk.2.tags.join(", "))
                .or_default()
                .push(shrunk);
        }
        let mut groups: Vec<_> = groups.into_iter().collect();
        groups.sort_by(|a, b| b.1.len().cmp(&a.1.len()));

        for (tags, members) in groups {
            let tags = if tags.is_empty() {
                "no tags".to_string()
            } else {
                tags
            };
            let _ = writeln!(out, "#### {} shrunk: {tags}\n", members.len());

            // The smallest reproduction of each group, and the seeds of the others
            let mut members = members;
            members.sort_by_key(|(_, _, shrunk)| shrunk.stripped.len());
            let (seed, name, shrunk) = members[0];
            let detail = records_by_seed(records, *seed)
                .variants
                .iter()
                .find(|variant| variant.name == *name)
                .map(|variant| {
                    variant
                        .outcome
                        .diag
                        .as_ref()
                        .map(|diag| format!("{} at {}", diag.message, diag.line_col))
                        .or_else(|| variant.outcome.detail.clone())
                        .unwrap_or_default()
                })
                .unwrap_or_default();
            let _ = writeln!(
                out,
                "Seed {seed}, variant `{name}`: `{}`\n\n```rue\n{}```\n\nBefore shrinking: {detail}\n",
                shrunk.shape, shrunk.stripped
            );
            if members.len() > 1 {
                let others: Vec<String> = members[1..]
                    .iter()
                    .map(|(seed, name, _)| format!("{seed} `{name}`"))
                    .collect();
                let _ = writeln!(out, "Also: {}\n", others.join(", "));
            }
        }
    }

    out
}

fn write_outputs(
    out: &PathBuf,
    records: &[SeedRecord],
    clusters: &BTreeMap<String, Cluster>,
    report: &str,
) -> Result<()> {
    fs::create_dir_all(out)?;

    let mut results = String::new();
    for record in records {
        results.push_str(&serde_json::to_string(record)?);
        results.push('\n');
    }
    fs::write(out.join("results.jsonl"), results)?;
    fs::write(out.join("report.md"), report)?;

    let dir = out.join("clusters");
    let _ = fs::remove_dir_all(&dir);
    for (number, cluster) in clusters.values().enumerate() {
        let cluster_dir = dir.join(format!("{number:03}"));
        fs::create_dir_all(&cluster_dir)?;
        for (seed, name, shrunk) in &cluster.shrunk {
            fs::write(
                cluster_dir.join(format!("{seed}-{name}-explicit.rue")),
                &shrunk.explicit,
            )?;
            fs::write(
                cluster_dir.join(format!("{seed}-{name}-stripped.rue")),
                &shrunk.stripped,
            )?;
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests;
