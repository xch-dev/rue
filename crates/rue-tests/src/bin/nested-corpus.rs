//! A corpus of random programs with recursive generic aliases and generic function types, for
//! #344 and #362's step 13b.
//!
//! #275 and #342 measured comparisons between recursive types on a corpus of 2,500 programs like
//! these, and #344 lists the 12 of them that still didn't finish. That generator was never
//! committed, so this one was rebuilt from their description: two or three recursive aliases with
//! generic parameters, whose bodies are built from atoms, pairs, unions, references to the aliases
//! and generic function types, followed by an assignment or an `is` check between two of them. The
//! programs are deterministic by seed.
//!
//! The corpus is seeds `0..2500`, compiled with a 60 s time limit per program. Since #386, a
//! comparison that would run for too long gives up after 20,000,000 steps and is reported as too
//! complex, so which programs give up doesn't depend on the machine's speed. The time limit only
//! catches anything that still runs for too long.
//!
//! ```text
//! cargo run --release -p rue-tests --bin nested-corpus -- --print 0
//! cargo run --release -p rue-tests --bin nested-corpus -- \
//!     --rue target/release/rue --seeds 0..2500 --threads 4 --out <file.jsonl>
//! cargo run --release -p rue-tests --bin nested-corpus -- --compare <before.jsonl> <after.jsonl>
//! ```
//!
//! Options:
//!
//! - `--print SEED`: print the program for one seed, and exit.
//! - `--rue PATH`: the `rue` binary to compile each program with (`rue build <file>`), so that two
//!   builds of the compiler can be run on the same programs.
//! - `--seeds A..B`: the seeds (default `0..2500`).
//! - `--threads N`: how many programs to compile at once (default 2).
//! - `--timeout S`: the time limit in seconds for each compile (default 60).
//! - `--out FILE`: where to write the results, one JSON object per line: the seed, the status
//!   (`ok`, `too_complex`, `timeout` or `crashed`), the time taken in milliseconds, and a hash of
//!   everything the compiler printed.
//! - `--compare BEFORE AFTER`: compare two result files, and print each seed whose status or
//!   output changed.

use std::{
    collections::BTreeMap,
    fmt::Write as _,
    fs,
    hash::{DefaultHasher, Hash, Hasher},
    io::Read as _,
    path::{Path, PathBuf},
    process::{Command, Stdio},
    sync::{
        Mutex,
        atomic::{AtomicUsize, Ordering},
    },
    thread,
    time::{Duration, Instant},
};

use anyhow::{Context, Result, bail};
use serde::{Deserialize, Serialize};

/// A small deterministic PRNG (`SplitMix64`), as in `rue-types/tests/memo.rs`.
struct Rng(u64);

impl Rng {
    fn next(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9e37_79b9_7f4a_7c15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
        z ^ (z >> 31)
    }

    fn below(&mut self, n: usize) -> usize {
        (self.next() % n as u64) as usize
    }

    fn chance(&mut self, percent: usize) -> bool {
        self.below(100) < percent
    }

    fn pick<T: Copy>(&mut self, items: &[T]) -> T {
        items[self.below(items.len())]
    }
}

const ATOMS: &[&str] = &["Int", "Bytes", "Bytes32", "Bool", "nil"];
const ALIAS_GENERICS: &[&str] = &["A", "B", "C"];

/// Writes the program for a seed.
struct Generator {
    rng: Rng,
    /// The number of generic parameters of each alias.
    aliases: Vec<usize>,
    /// The index of the alias being written, which names the generics of its function types.
    alias: usize,
    /// The number of generic function types written in the alias so far.
    functions: usize,
}

impl Generator {
    fn new(seed: u64) -> Self {
        Self {
            rng: Rng(seed),
            aliases: Vec::new(),
            alias: 0,
            functions: 0,
        }
    }

    fn program(&mut self) -> String {
        let count = 2 + self.rng.below(2);
        self.aliases = (0..count).map(|_| self.rng.below(3)).collect();

        let mut source = String::new();

        for alias in 0..count {
            self.alias = alias;
            self.functions = 0;
            let generics: Vec<String> = ALIAS_GENERICS[..self.aliases[alias]]
                .iter()
                .map(ToString::to_string)
                .collect();
            let body = self.ty(3, &generics);
            let params = if generics.is_empty() {
                String::new()
            } else {
                format!("<{}>", generics.join(", "))
            };
            writeln!(source, "type N{alias}{params} = {body};").unwrap();
        }

        self.alias = count;

        for query in ["Q1", "Q2"] {
            let ty = if self.rng.chance(70) {
                self.reference(2, &[])
            } else {
                self.ty(2, &[])
            };
            writeln!(source, "type {query} = {ty};").unwrap();
        }

        if self.rng.chance(70) {
            source.push_str("fn main(x: Q1, y: Q2) -> Int {\n    let _z: Q1 = y;\n    0\n}\n");
        } else {
            source.push_str("fn main(x: Q1, y: Q2) -> Int {\n    if y is Q1 { 1 } else { 0 }\n}\n");
        }

        source
    }

    /// A random type, where `generics` are the generics in scope.
    fn ty(&mut self, depth: usize, generics: &[String]) -> String {
        let choice = if depth == 0 {
            self.rng.below(3)
        } else {
            self.rng.below(9)
        };

        match choice {
            0 | 1 if !generics.is_empty() && self.rng.chance(50) => {
                generics[self.rng.below(generics.len())].clone()
            }
            0 | 1 => self.rng.pick(ATOMS).to_string(),
            2 => self.reference(depth.saturating_sub(1), generics),
            3 | 4 => format!(
                "({}, {})",
                self.ty(depth - 1, generics),
                self.ty(depth - 1, generics)
            ),
            5 => format!(
                "{} | {}",
                self.member(depth - 1, generics),
                self.member(depth - 1, generics)
            ),
            6 | 7 => self.function(depth - 1, generics),
            _ => self.reference(depth - 1, generics),
        }
    }

    /// A union member, which can't be a function type without parentheses.
    fn member(&mut self, depth: usize, generics: &[String]) -> String {
        if self.rng.chance(60) {
            self.reference(depth, generics)
        } else {
            let ty = self.ty(depth, generics);
            if ty.starts_with("fn") || ty.contains(" | ") {
                format!("({ty}, nil)")
            } else {
                ty
            }
        }
    }

    /// A reference to one of the aliases, with random arguments.
    fn reference(&mut self, depth: usize, generics: &[String]) -> String {
        let alias = self.rng.below(self.aliases.len());
        let args: Vec<String> = (0..self.aliases[alias])
            .map(|_| self.ty(depth.min(1), generics))
            .collect();

        if args.is_empty() {
            format!("N{alias}")
        } else {
            format!("N{alias}<{}>", args.join(", "))
        }
    }

    /// A function type with up to two generics of its own, which its parameters and return type
    /// can reference anywhere, including nested inside the aliases.
    fn function(&mut self, depth: usize, generics: &[String]) -> String {
        let own: Vec<String> = (0..self.rng.below(3))
            .map(|index| format!("U{}{}{index}", self.alias, self.functions))
            .collect();
        self.functions += 1;

        let mut scope = generics.to_vec();
        scope.extend(own.iter().cloned());

        let params: Vec<String> = (0..=self.rng.below(2))
            .map(|index| format!("p{index}: {}", self.ty(depth, &scope)))
            .collect();
        let ret = self.ty(depth, &scope);

        let own = if own.is_empty() {
            String::new()
        } else {
            format!("<{}>", own.join(", "))
        };

        format!("fn{own}({}) -> {ret}", params.join(", "))
    }
}

/// The result of compiling one program.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct Record {
    seed: u64,
    status: Status,
    millis: u64,
    /// A hash of everything the compiler printed, other than for a timeout or crash.
    output: Option<u64>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
enum Status {
    Ok,
    TooComplex,
    Timeout,
    Crashed,
}

fn compile(rue: &Path, dir: &Path, seed: u64, timeout: Duration) -> Result<Record> {
    let path = dir.join(format!("p{seed:04}.rue"));
    fs::write(&path, Generator::new(seed).program())?;

    let start = Instant::now();
    let mut child = Command::new(rue)
        .arg("build")
        .arg(&path)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .with_context(|| format!("couldn't run {}", rue.display()))?;

    let mut stdout = child.stdout.take().unwrap();
    let mut stderr = child.stderr.take().unwrap();
    let out = thread::spawn(move || {
        let mut text = String::new();
        stdout.read_to_string(&mut text).map(|_| text)
    });
    let err = thread::spawn(move || {
        let mut text = String::new();
        stderr.read_to_string(&mut text).map(|_| text)
    });

    let status = loop {
        if let Some(status) = child.try_wait()? {
            break Some(status);
        }

        if start.elapsed() > timeout {
            child.kill()?;
            child.wait()?;
            break None;
        }

        thread::sleep(Duration::from_millis(5));
    };

    let millis = u64::try_from(start.elapsed().as_millis()).unwrap_or(u64::MAX);
    let text = out.join().unwrap()? + &err.join().unwrap()?;
    // The path differs between runs, so it isn't part of the output
    let text = text.replace(&path.display().to_string(), "<program>");

    let (status, output) = match status {
        None => (Status::Timeout, None),
        Some(status) if status.code().is_none() || text.contains("panicked") => {
            (Status::Crashed, None)
        }
        Some(_) => {
            let mut hasher = DefaultHasher::new();
            text.hash(&mut hasher);
            let status = if text.contains("is too complex") {
                Status::TooComplex
            } else {
                Status::Ok
            };
            (status, Some(hasher.finish()))
        }
    };

    Ok(Record {
        seed,
        status,
        millis,
        output,
    })
}

fn run(rue: &Path, seeds: &[u64], threads: usize, timeout: Duration) -> Result<Vec<Record>> {
    let dir = std::env::temp_dir().join(format!("nested-corpus-{}", std::process::id()));
    fs::create_dir_all(&dir)?;

    let next = AtomicUsize::new(0);
    let records = Mutex::new(Vec::new());

    thread::scope(|scope| -> Result<()> {
        let handles: Vec<_> = (0..threads.max(1))
            .map(|_| {
                scope.spawn(|| -> Result<()> {
                    loop {
                        let index = next.fetch_add(1, Ordering::Relaxed);
                        let Some(&seed) = seeds.get(index) else {
                            return Ok(());
                        };
                        let record = compile(rue, &dir, seed, timeout)?;
                        if record.status != Status::Ok {
                            eprintln!("p{seed:04}: {:?} in {} ms", record.status, record.millis);
                        }
                        records.lock().unwrap().push(record);
                    }
                })
            })
            .collect();

        for handle in handles {
            handle.join().unwrap()?;
        }

        Ok(())
    })?;

    fs::remove_dir_all(&dir)?;

    let mut records = records.into_inner().unwrap();
    records.sort_by_key(|record| record.seed);
    Ok(records)
}

fn load(path: &Path) -> Result<BTreeMap<u64, Record>> {
    fs::read_to_string(path)
        .with_context(|| format!("couldn't read {}", path.display()))?
        .lines()
        .map(|line| {
            let record: Record = serde_json::from_str(line)?;
            Ok((record.seed, record))
        })
        .collect()
}

fn summary(records: &[&Record]) -> String {
    let count = |status| {
        records
            .iter()
            .filter(|record| record.status == status)
            .count()
    };
    format!(
        "{} programs: {} ok, {} too complex, {} timed out, {} crashed",
        records.len(),
        count(Status::Ok),
        count(Status::TooComplex),
        count(Status::Timeout),
        count(Status::Crashed)
    )
}

fn compare(before: &Path, after: &Path) -> Result<()> {
    let before = load(before)?;
    let after = load(after)?;

    println!("before: {}", summary(&before.values().collect::<Vec<_>>()));
    println!("after: {}", summary(&after.values().collect::<Vec<_>>()));

    for (seed, old) in &before {
        let Some(new) = after.get(seed) else {
            continue;
        };

        if old.status != new.status || old.output != new.output {
            println!(
                "p{seed:04}: {:?} ({} ms) -> {:?} ({} ms){}",
                old.status,
                old.millis,
                new.status,
                new.millis,
                if old.status == new.status {
                    ", output changed"
                } else {
                    ""
                }
            );
        }
    }

    Ok(())
}

fn parse_range(text: &str) -> Result<std::ops::Range<u64>> {
    let (start, end) = text
        .split_once("..")
        .context("expected a seed range such as `0..2500`")?;
    Ok(start.parse()?..end.parse()?)
}

fn main() -> Result<()> {
    let mut seeds = 0..2500;
    let mut threads = 2;
    let mut timeout = Duration::from_secs(60);
    let mut rue = None;
    let mut out = None;

    let mut iter = std::env::args().skip(1);

    while let Some(arg) = iter.next() {
        let mut value = || {
            iter.next()
                .with_context(|| format!("`{arg}` needs a value"))
        };

        match arg.as_str() {
            "--print" => {
                print!("{}", Generator::new(value()?.parse()?).program());
                return Ok(());
            }
            "--compare" => {
                let before = PathBuf::from(value()?);
                let after = PathBuf::from(value()?);
                return compare(&before, &after);
            }
            "--seeds" => seeds = parse_range(&value()?)?,
            "--threads" => threads = value()?.parse()?,
            "--timeout" => timeout = Duration::from_secs(value()?.parse()?),
            "--rue" => rue = Some(PathBuf::from(value()?)),
            "--out" => out = Some(PathBuf::from(value()?)),
            _ => bail!("unknown argument `{arg}`; see the doc comment in nested-corpus.rs"),
        }
    }

    let Some(rue) = rue else {
        bail!("`--rue` is needed to compile the programs");
    };

    let seeds: Vec<u64> = seeds.collect();
    let records = run(&rue, &seeds, threads, timeout)?;
    println!("{}", summary(&records.iter().collect::<Vec<_>>()));

    if let Some(out) = out {
        let mut text = String::new();

        for record in &records {
            writeln!(text, "{}", serde_json::to_string(record)?).unwrap();
        }

        fs::write(&out, text)?;
    }

    Ok(())
}
