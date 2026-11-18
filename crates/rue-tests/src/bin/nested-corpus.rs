//! A corpus of random programs with recursive generic aliases and generic function types, for
//! #344 and #362's step 13b.
//!
//! #275 and #342 measured comparisons between recursive types on a corpus of 2,500 programs like
//! these, and #344 lists the 12 of them that still didn't finish. That generator and its programs
//! were lost, so this one was rebuilt from their description: two or three recursive aliases with
//! generic parameters, whose bodies are built from atoms, pairs, unions, references to the aliases
//! and generic function types, followed by an assignment or an `is` check between two of them. The
//! programs are deterministic by seed. The aliases are regular and contractive, so that the
//! programs get as far as comparing the types: an alias is only referred to under a pair or a
//! function type inside the aliases, and its arguments aren't built from the referring alias's own
//! generics.
//!
//! The corpus is seeds `0..2500`, compiled with a 300 s time limit per program. Since #386, a
//! comparison that would run for too long gives up after 20,000,000 steps and is reported as too
//! complex, which doesn't depend on the machine's speed. But some work isn't counted in steps, so a
//! program can still run past the time limit instead, which does depend on it, and a program close
//! to the limit can be either. Both count as not finishing, and `--compare` lists a change between
//! them without counting it as one.
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
//! - `--timeout S`: the time limit in seconds for each compile (default 300).
//! - `--out FILE`: where to write the results, one JSON object per line: the seed, the status
//!   (`compiled`, `rejected`, `too_complex`, `timeout` or `crashed`), the time taken in
//!   milliseconds, a hash of everything the compiler printed, and its first error.
//! - `--compare BEFORE AFTER`: compare two result files, and print each seed whose status or
//!   output changed.

use std::{
    collections::BTreeMap,
    fmt::Write as _,
    fs,
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
            let own = self.aliases[alias];
            let scope = Scope {
                generics: ALIAS_GENERICS[..own]
                    .iter()
                    .map(ToString::to_string)
                    .collect(),
                own,
                guarded: false,
            };
            let body = self.ty(3, &scope);
            let params = if own == 0 {
                String::new()
            } else {
                format!("<{}>", scope.generics.join(", "))
            };
            writeln!(source, "type N{alias}{params} = {body};").unwrap();
        }

        self.alias = count;

        // The queries aren't part of a cycle, so they can refer to the aliases anywhere
        let scope = Scope {
            generics: Vec::new(),
            own: 0,
            guarded: true,
        };

        for query in ["Q1", "Q2"] {
            let ty = if self.rng.chance(70) {
                self.reference(2, &scope)
            } else {
                self.ty(2, &scope)
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

    /// A random type.
    fn ty(&mut self, depth: usize, scope: &Scope) -> String {
        let choice = if depth == 0 {
            self.rng.below(3)
        } else {
            self.rng.below(9)
        };

        match choice {
            0 | 1 if !scope.generics.is_empty() && self.rng.chance(50) => {
                scope.generics[self.rng.below(scope.generics.len())].clone()
            }
            0 | 1 => self.rng.pick(ATOMS).to_string(),
            2 if scope.guarded => self.reference(depth.saturating_sub(1), scope),
            2 => self.rng.pick(ATOMS).to_string(),
            3 | 4 => {
                let scope = scope.guarded();
                format!(
                    "({}, {})",
                    self.ty(depth - 1, &scope),
                    self.ty(depth - 1, &scope)
                )
            }
            5 => format!(
                "{} | {}",
                self.member(depth - 1, scope),
                self.member(depth - 1, scope)
            ),
            6 | 7 => self.function(depth - 1, scope),
            _ if scope.guarded => self.reference(depth - 1, scope),
            _ => {
                let scope = scope.guarded();
                format!("({}, nil)", self.reference(depth - 1, &scope))
            }
        }
    }

    /// A union member, which can't be a function type without parentheses.
    fn member(&mut self, depth: usize, scope: &Scope) -> String {
        if self.rng.chance(60) {
            if scope.guarded {
                self.reference(depth, scope)
            } else {
                format!("({}, nil)", self.reference(depth, &scope.guarded()))
            }
        } else {
            let ty = self.ty(depth, scope);
            if ty.starts_with("fn") || ty.contains(" | ") {
                format!("({ty}, nil)")
            } else {
                ty
            }
        }
    }

    /// A reference to one of the aliases, with random arguments. An argument is either one of the
    /// generics in scope, or a type that doesn't refer to the alias's own generics, so that the
    /// aliases are regular (an alias that refers to itself with arguments built from its own
    /// generics would expand without end). An argument's own references to the aliases are
    /// guarded, since the alias can use the argument where it isn't.
    fn reference(&mut self, depth: usize, scope: &Scope) -> String {
        let alias = self.rng.below(self.aliases.len());
        let inner = Scope {
            generics: scope.generics[scope.own..].to_vec(),
            own: 0,
            guarded: false,
        };
        let args: Vec<String> = (0..self.aliases[alias])
            .map(|_| {
                if !scope.generics.is_empty() && self.rng.chance(40) {
                    scope.generics[self.rng.below(scope.generics.len())].clone()
                } else {
                    self.ty(depth.min(1), &inner)
                }
            })
            .collect();

        if args.is_empty() {
            format!("N{alias}")
        } else {
            format!("N{alias}<{}>", args.join(", "))
        }
    }

    /// A function type with up to two generics of its own, which its parameters and return type
    /// can reference anywhere, including nested inside the aliases.
    fn function(&mut self, depth: usize, scope: &Scope) -> String {
        let own: Vec<String> = (0..self.rng.below(3))
            .map(|index| format!("U{}{}{index}", self.alias, self.functions))
            .collect();
        self.functions += 1;

        let mut scope = scope.guarded();
        scope.generics.extend(own.iter().cloned());

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

/// The generics in scope where a type is written, and whether a reference to an alias there is
/// guarded by a pair or a function type, so that a cycle of aliases through it is contractive.
#[derive(Debug, Clone)]
struct Scope {
    generics: Vec<String>,
    /// The number of generics at the start of `generics` that are the alias's own parameters.
    own: usize,
    guarded: bool,
}

impl Scope {
    fn guarded(&self) -> Self {
        Self {
            guarded: true,
            ..self.clone()
        }
    }
}

/// The result of compiling one program.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct Record {
    seed: u64,
    status: Status,
    millis: u64,
    /// A hash of everything the compiler printed (see `fnv1a`), other than for a timeout or crash.
    output: Option<u64>,
    /// The first error the compiler reported, if any.
    error: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
enum Status {
    /// It compiled without errors.
    Compiled,
    /// It finished with an error other than a comparison being too complex.
    Rejected,
    TooComplex,
    Timeout,
    Crashed,
}

impl Status {
    /// Whether compiling the program finished, rather than giving up or running out of time.
    fn finished(self) -> bool {
        matches!(self, Self::Compiled | Self::Rejected)
    }
}

/// A 64-bit FNV-1a hash, which unlike `DefaultHasher` is the same with every build, so that result
/// files from different builds can be compared.
fn fnv1a(text: &str) -> u64 {
    text.bytes().fold(0xcbf2_9ce4_8422_2325, |hash, byte| {
        (hash ^ u64::from(byte)).wrapping_mul(0x0100_0000_01b3)
    })
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
        let mut bytes = Vec::new();
        stdout.read_to_end(&mut bytes).map(|_| bytes)
    });
    let err = thread::spawn(move || {
        let mut bytes = Vec::new();
        stderr.read_to_end(&mut bytes).map(|_| bytes)
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
    let mut bytes = out.join().unwrap()?;
    bytes.extend(err.join().unwrap()?);
    let text = String::from_utf8_lossy(&bytes);
    let error = text
        .lines()
        .find(|line| line.starts_with("Error"))
        .map(ToString::to_string);

    let (status, output) = match status {
        None => (Status::Timeout, None),
        Some(status) if status.code().is_none() || text.contains("panicked") => {
            (Status::Crashed, None)
        }
        Some(_) => {
            let status = if text.contains("is too complex") {
                Status::TooComplex
            } else if error.is_some() {
                Status::Rejected
            } else {
                Status::Compiled
            };
            (status, Some(fnv1a(&text)))
        }
    };

    Ok(Record {
        seed,
        status,
        millis,
        output,
        error,
    })
}

fn run(rue: &Path, seeds: &[u64], threads: usize, timeout: Duration) -> Result<Vec<Record>> {
    let dir = std::env::temp_dir().join(format!("nested-corpus-{}", std::process::id()));
    fs::create_dir_all(&dir)?;

    let next = AtomicUsize::new(0);
    let records = Mutex::new(Vec::new());

    let result = thread::scope(|scope| -> Result<()> {
        let handles: Vec<_> = (0..threads.max(1))
            .map(|_| {
                scope.spawn(|| -> Result<()> {
                    loop {
                        let index = next.fetch_add(1, Ordering::Relaxed);
                        let Some(&seed) = seeds.get(index) else {
                            return Ok(());
                        };
                        let record = match compile(rue, &dir, seed, timeout) {
                            Ok(record) => record,
                            Err(error) => {
                                // The other workers stop after the program they're on
                                next.store(seeds.len(), Ordering::Relaxed);
                                return Err(error);
                            }
                        };
                        if !record.status.finished() {
                            eprintln!("p{seed:04}: {:?} in {} ms", record.status, record.millis);
                        }
                        records.lock().unwrap().push(record);
                    }
                })
            })
            .collect();

        handles
            .into_iter()
            .try_for_each(|handle| handle.join().unwrap())
    });

    fs::remove_dir_all(&dir)?;
    result?;

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
        "{} programs: {} compiled, {} rejected, {} too complex, {} timed out, {} crashed",
        records.len(),
        count(Status::Compiled),
        count(Status::Rejected),
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
            let note = if old.status == new.status {
                ", output changed"
            } else if !old.status.finished() && !new.status.finished() {
                ", neither finished"
            } else {
                ""
            };
            println!(
                "p{seed:04}: {:?} ({} ms) -> {:?} ({} ms){note}",
                old.status, old.millis, new.status, new.millis
            );

            if new.status.finished() && new.error != old.error {
                println!("    {}", new.error.as_deref().unwrap_or("compiles"));
            }
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
    let mut timeout = Duration::from_secs(300);
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

#[cfg(test)]
mod tests {
    use super::*;

    /// The corpus is referred to by seed, so a seed's program must never change.
    #[test]
    fn test_programs_are_stable() {
        expect_test::expect![[r#"
            type N0 = ((N2<Bytes> | N0, fn<U000, U001>(p0: U000, p1: Bytes) -> Bytes), N2<(Bool, Int)>);
            type N1<A> = fn(p0: (N2<Bytes32> | A, fn(p0: Bytes, p1: A) -> N0), p1: (nil, fn<U120>(p0: N0, p1: A) -> U120)) -> N1<Bytes> | Bytes;
            type N2<A> = A;
            type Q1 = ((N0, Bytes), N0);
            type Q2 = Bool;
            fn main(x: Q1, y: Q2) -> Int {
                if y is Q1 { 1 } else { 0 }
            }
        "#]]
        .assert_eq(&Generator::new(0).program());

        let hash = (0..2500).fold(0, |hash: u64, seed| {
            hash.rotate_left(5) ^ fnv1a(&Generator::new(seed).program())
        });
        assert_eq!(hash, 3_163_750_450_864_588_512, "the programs changed");
    }
}
