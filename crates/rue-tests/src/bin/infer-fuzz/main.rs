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
//! (`one`). Generator version 2 adds a fourth, `expected`, which removes only the arguments
//! that nothing but the expected type gives a type to, such as `let x: Int | nil = none();`.
//! Failures are grouped by category and the first diagnostic, and a few of each group are shrunk
//! to a small reproduction.
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
//! - `--generator N`: the generator version (default 1). A version's programs never change, so
//!   runs of the same version stay comparable with `--compare`, but the same seed is a different
//!   program in each version. Version 2
//!   adds generic structs and aliases, unions of function types, generic structs and literals,
//!   generic function types in annotations and return types, longer chains and cycles, `std`'s
//!   `merge_list`, and the `expected` variant. The versions are listed in `generate.rs`.
//! - `--print SEED`: print the explicit program and the variants for one seed, and exit.
//! - `--compare BASELINE NEW`: compare two runs of the same seeds, each a `results.jsonl` or the
//!   `--out` directory that holds it. It prints how many variants went from each status to each
//!   other one, and every variant whose status, output, first diagnostic or fingerprint (a hash
//!   of every diagnostic and the CLVM) changed, marking the regressions: variants that are no
//!   longer inferred, or become unsound, slow or crashed. It exits with status 1 if there are any
//!   regressions. The format is described in `compare.rs`.

mod compare;
mod generate;
mod leak;
mod oracle;
mod program;
mod report;
mod shrink;
mod worker;

use std::path::PathBuf;
use std::thread;
use std::time::Duration;

use anyhow::{Context, Result, bail};

use crate::generate::Generator;
use crate::program::Variant;
use crate::worker::{Shrunk, parallel, run_seed, shrink_on};

#[derive(Debug, Clone)]
struct Args {
    seeds: std::ops::Range<u64>,
    generator: u32,
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
        generator: 1,
        threads: 2,
        timeout: Duration::from_secs(10),
        shrink: 3,
        out: None,
    };

    let mut print = None;
    let mut iter = std::env::args().skip(1);
    while let Some(arg) = iter.next() {
        let mut value = || {
            iter.next()
                .with_context(|| format!("`{arg}` needs a value"))
        };
        match arg.as_str() {
            "--worker" => return run_in_large_stack(worker::worker),
            "--print" => print = Some(value()?.parse()?),
            "--generator" => {
                args.generator = value()?.parse()?;
                if !generate::VERSIONS.contains(&args.generator) {
                    bail!("there's no generator version {}", args.generator);
                }
            }
            "--compare" => {
                let before = compare::load(&PathBuf::from(value()?))?;
                let after = compare::load(&PathBuf::from(value()?))?;
                let comparison = compare::compare(&before, &after);
                println!("{}", comparison.report());
                std::process::exit(i32::from(comparison.regressions().next().is_some()));
            }
            "--seeds" => args.seeds = parse_range(&value()?)?,
            "--threads" => args.threads = value()?.parse()?,
            "--timeout" => args.timeout = Duration::from_secs(value()?.parse()?),
            "--shrink" => args.shrink = value()?.parse()?,
            "--out" => args.out = Some(PathBuf::from(value()?)),
            _ => bail!("unknown argument `{arg}`; see the doc comment in infer-fuzz/main.rs"),
        }
    }

    if let Some(seed) = print {
        print_seed(seed, args.generator);
        return Ok(());
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

fn print_seed(seed: u64, version: u32) {
    let generated = Generator::versioned(seed, version).generate();
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
        let generated = Generator::versioned(seed, args.generator).generate();
        run_seed(worker, seed, &generated, args.timeout)
    })?;

    let mut clusters = report::clusters(&records);

    let jobs = report::shrink_jobs(&records, &clusters, args.shrink);
    eprintln!("Shrinking {} failures", jobs.len());
    let shrunk = parallel(&jobs, args.threads, |worker, &(cluster, (seed, index))| {
        Ok((
            cluster,
            (seed, index),
            shrink_on(worker, args.generator, seed, index, args.timeout)?,
        ))
    })?;

    for (cluster, member, result) in shrunk {
        let variant = &report::record(&records, member.0).variants[member.1];
        let shrunk = result.unwrap_or_else(|| Shrunk {
            explicit: report::record(&records, member.0).explicit_source.clone(),
            stripped: variant.source.clone(),
            shape: variant.shape.clone(),
            tags: vec!["not shrunk".to_string()],
            message: String::new(),
        });
        clusters[cluster].shrunk.push((member, shrunk));
    }

    let report = report::report(&records, &clusters);
    println!("{report}");

    if let Some(out) = &args.out {
        report::write_outputs(out, &records, &clusters, &report)?;
        eprintln!("Wrote the results to {}", out.display());
    }

    Ok(())
}

#[cfg(test)]
mod tests;
