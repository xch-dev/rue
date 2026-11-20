//! Compares a baseline run with a new run, seed by seed, so a change to the compiler is checked
//! variant by variant rather than by the totals.
//!
//! Run the same seeds with both compilers, each with `--out`, then
//! `infer-fuzz --compare BASELINE NEW`, where each is a `results.jsonl` or the directory that holds
//! it. Each variant of the baseline is matched with the variant of the same seed and name in the
//! new run. It's only compared when its source is the same in both, so a change to the generator
//! makes variants "not comparable" rather than changed.
//!
//! A variant **changes** when its status (inferred, rejected, unsound, slow or crashed), its
//! output, its first diagnostic, or its fingerprint differs, a crash's first diagnostic being the
//! first line of its panic message. The fingerprint (`oracle::fingerprint`) is a hash of every
//! diagnostic, warnings included, and the CLVM, so a change that should keep the compiler's
//! behaviour identical can be checked with no changes at all. Outputs and fingerprints are only
//! compared when both runs recorded them. A change is a
//! **regression** when the variant was inferred and no longer is, or becomes unsound, leaked,
//! slow or crashed.
//!
//! An explicit program whose status or fingerprint changes is a change of its own, with the
//! variant `explicit`.
//! It's a regression if it compiled and no longer does, or newly crashes. When it compiles in only
//! one of the runs, its variants can't be compared, but a variant of a newly compiling program
//! that's unsound, leaked, slow or crashed is reported as a regression, with `none` as its before
//! side.
//! Seeds and variants that are only in one run, or whose source differs, count as not comparable.
//! Variants and explicit programs that are compared, but whose fingerprint is missing from either
//! run, are counted as without fingerprints, so a baseline from before fingerprints were recorded
//! doesn't pass a check for no changes unnoticed.
//!
//! The report is meant to be diffed and read by scripts, so its format is kept stable:
//!
//! ```text
//! # Comparison
//!
//! | before \ after | Inferred | Rejected | Unsound | Slow | Crashed |
//! |---|---|---|---|---|---|
//! | Inferred | 8676 | 0 | 0 | 0 | 0 |
//! ...
//!
//! Compared 11638 variants: 12 changed, 0 regressed, 3 newly inferred, 0 not comparable, 0 variants and explicit programs without fingerprints.
//!
//! ## Changes
//!
//! - seed 42 `all` [regression]: "Inferred: 7" -> "Rejected: Cannot infer generic `T` for `f`; ... at 3:5"
//! - seed 57 `one` [diagnostic]: "Rejected: ... at 4:1" -> "Rejected: ... at 4:1"
//! ```
//!
//! Each change is one line, `- seed <seed> `<variant>` [<kind>]: <before> -> <after>`, sorted by
//! seed, with the explicit program's change first and then the variants by name:
//!
//! - `<variant>` is `all`, `some`, `one`, or `explicit` for the explicit program.
//! - `<kind>` is `regression`, `status`, `output`, `diagnostic` or `fingerprint`, the first of them
//!   that applies.
//! - `<before>` and `<after>` are JSON strings, so they never contain a line break or an
//!   unescaped `"`, and ` -> ` between them is the only one outside a string. Each is the status
//!   (`Inferred`, `Rejected`, `Unsound`, `Slow`, `Crashed` for a variant; `Valid`, `Rejected`,
//!   `Panicked`, `NoSites`, `TimedOut`, `Died` for an explicit program), then `: ` and the first
//!   diagnostic with its position, the output, or the detail, if there is one (the detail, which
//!   is the timing, for a slow variant). For a `fingerprint` change, ` #` and the fingerprint
//!   follow. `<before>` is `"none"` for a variant that only the new run has.
//!
//! The command exits with status 1 if there are any regressions.

use std::collections::BTreeMap;
use std::fmt::Write as _;
use std::fs;
use std::path::Path;

use anyhow::{Context, Result};

use crate::oracle::{Category, Outcome, Run};
use crate::report::SeedRecord;
use crate::worker::Explicit;

/// Reads `results.jsonl`, or the one in a directory that `--out` wrote.
pub fn load(path: &Path) -> Result<Vec<SeedRecord>> {
    let file = if path.is_dir() {
        path.join("results.jsonl")
    } else {
        path.to_path_buf()
    };
    let text =
        fs::read_to_string(&file).with_context(|| format!("can't read {}", file.display()))?;
    text.lines()
        .enumerate()
        .filter(|(_, line)| !line.trim().is_empty())
        .map(|(index, line)| {
            serde_json::from_str(line)
                .with_context(|| format!("can't parse line {} of {}", index + 1, file.display()))
        })
        .collect()
}

/// What changed about a variant, the first that applies.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ChangeKind {
    Regression,
    Status,
    Output,
    Diagnostic,
    Fingerprint,
}

impl ChangeKind {
    fn name(self) -> &'static str {
        match self {
            ChangeKind::Regression => "regression",
            ChangeKind::Status => "status",
            ChangeKind::Output => "output",
            ChangeKind::Diagnostic => "diagnostic",
            ChangeKind::Fingerprint => "fingerprint",
        }
    }
}

/// A variant whose result changed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Change {
    pub seed: u64,
    pub variant: String,
    pub kind: ChangeKind,
    pub before: String,
    pub after: String,
}

#[derive(Debug, Default)]
pub struct Comparison {
    /// How many variants went from each category to each other one.
    pub transitions: BTreeMap<(Category, Category), usize>,
    pub changes: Vec<Change>,
    /// Variants that are only in one run, whose source differs, or whose explicit program only
    /// compiles in one run.
    pub not_comparable: usize,
    /// Variants and explicit programs that were compared, but whose fingerprint is missing from
    /// either run (one from before fingerprints were recorded, or a crash), so a change in what
    /// they report other than the first diagnostic wouldn't be seen.
    pub without_fingerprints: usize,
}

impl Comparison {
    pub fn regressions(&self) -> impl Iterator<Item = &Change> {
        self.changes
            .iter()
            .filter(|change| change.kind == ChangeKind::Regression)
    }
}

fn describe(outcome: &Outcome) -> String {
    let category = format!("{:?}", outcome.category);
    let output = outcome.output.as_ref().map(|run| match run {
        Run::Output(output) => output.clone(),
        Run::Error(error) => format!("error {error}"),
    });
    match (&outcome.diag, output, &outcome.detail) {
        (Some(diag), _, _) => format!("{category}: {}", diag.located()),
        // A slow variant's timing is what explains it
        (None, _, Some(detail)) if outcome.category == Category::Slow => {
            format!("{category}: {detail}")
        }
        (None, Some(output), _) => format!("{category}: {output}"),
        (None, None, Some(detail)) => format!("{category}: {detail}"),
        _ => category,
    }
}

fn describe_explicit(explicit: &Explicit) -> String {
    match explicit {
        Explicit::Valid { .. } => "Valid".to_string(),
        Explicit::Rejected(diag) => format!("Rejected: {}", diag.located()),
        Explicit::Panicked(message) => format!("Panicked: {message}"),
        Explicit::NoSites => "NoSites".to_string(),
        Explicit::TimedOut => "TimedOut".to_string(),
        Explicit::Died(stderr) => format!("Died: {stderr}"),
    }
}

fn is_regression(before: Category, after: Category) -> bool {
    (before == Category::Inferred && after != Category::Inferred)
        || (matches!(
            after,
            Category::Unsound | Category::Leaked | Category::Slow | Category::Crashed
        ) && after != before)
}

/// A crash's first diagnostic: the first line of its panic message, as `report::cluster_key` uses.
fn first_crash_line(outcome: &Outcome) -> Option<&str> {
    (outcome.category == Category::Crashed)
        .then(|| outcome.detail.as_deref()?.lines().next())
        .flatten()
}

/// Whether both runs recorded a fingerprint, and they're different.
fn differ(before: Option<&str>, after: Option<&str>) -> bool {
    before.is_some() && after.is_some() && before != after
}

/// The description of a variant or explicit program, with its fingerprint for a `fingerprint`
/// change, since the rest of the description is the same.
fn with_fingerprint(description: String, kind: ChangeKind, fingerprint: Option<&str>) -> String {
    match (kind, fingerprint) {
        (ChangeKind::Fingerprint, Some(fingerprint)) => format!("{description} #{fingerprint}"),
        _ => description,
    }
}

fn explicit_fingerprint(explicit: &Explicit) -> Option<&str> {
    match explicit {
        Explicit::Valid { fingerprint, .. } => fingerprint.as_deref(),
        _ => None,
    }
}

/// What changed between the two outcomes of a variant, if anything.
fn change_kind(before: &Outcome, after: &Outcome) -> Option<ChangeKind> {
    if is_regression(before.category, after.category) {
        Some(ChangeKind::Regression)
    } else if before.category != after.category {
        Some(ChangeKind::Status)
    } else if before.output.is_some() && after.output.is_some() && before.output != after.output {
        Some(ChangeKind::Output)
    } else if before.diag != after.diag || first_crash_line(before) != first_crash_line(after) {
        Some(ChangeKind::Diagnostic)
    } else if differ(before.fingerprint.as_deref(), after.fingerprint.as_deref()) {
        Some(ChangeKind::Fingerprint)
    } else {
        None
    }
}

pub fn compare(before: &[SeedRecord], after: &[SeedRecord]) -> Comparison {
    let mut comparison = Comparison::default();
    let before_seeds: BTreeMap<u64, &SeedRecord> =
        before.iter().map(|record| (record.seed, record)).collect();
    let after_seeds: BTreeMap<u64, &SeedRecord> =
        after.iter().map(|record| (record.seed, record)).collect();

    // Seeds that are only in the new run
    for (seed, new) in &after_seeds {
        if !before_seeds.contains_key(seed) {
            comparison.not_comparable += new.variants.len();
        }
    }

    for (&seed, old) in &before_seeds {
        let Some(new) = after_seeds.get(&seed) else {
            comparison.not_comparable += old.variants.len();
            continue;
        };

        if old.explicit_source != new.explicit_source {
            comparison.not_comparable += old.variants.len() + new.variants.len();
            continue;
        }

        let was_valid = matches!(old.explicit, Explicit::Valid { .. });
        let is_valid = matches!(new.explicit, Explicit::Valid { .. });
        let (before_explicit, after_explicit) = (
            describe_explicit(&old.explicit),
            describe_explicit(&new.explicit),
        );

        // A panic is compared by its first line, as a variant's is
        let first_line = |text: &str| text.lines().next().unwrap_or_default().to_string();
        if first_line(&before_explicit) != first_line(&after_explicit) {
            let crashes = matches!(new.explicit, Explicit::Panicked(_) | Explicit::Died(_))
                && !matches!(old.explicit, Explicit::Panicked(_) | Explicit::Died(_));
            let kind = if (was_valid && !is_valid) || crashes {
                ChangeKind::Regression
            } else if was_valid == is_valid
                && std::mem::discriminant(&old.explicit) == std::mem::discriminant(&new.explicit)
            {
                ChangeKind::Diagnostic
            } else {
                ChangeKind::Status
            };
            comparison.changes.push(Change {
                seed,
                variant: "explicit".to_string(),
                kind,
                before: before_explicit,
                after: after_explicit,
            });
        } else if !matches!(old.explicit, Explicit::NoSites)
            && (explicit_fingerprint(&old.explicit).is_none()
                || explicit_fingerprint(&new.explicit).is_none())
        {
            // Only a valid program records one, so the same rejection or crash is compared by its
            // first line alone. A program with no sites has nothing to compare
            comparison.without_fingerprints += 1;
        } else if differ(
            explicit_fingerprint(&old.explicit),
            explicit_fingerprint(&new.explicit),
        ) {
            let kind = ChangeKind::Fingerprint;
            comparison.changes.push(Change {
                seed,
                variant: "explicit".to_string(),
                kind,
                before: with_fingerprint(
                    before_explicit,
                    kind,
                    explicit_fingerprint(&old.explicit),
                ),
                after: with_fingerprint(after_explicit, kind, explicit_fingerprint(&new.explicit)),
            });
        }

        if !was_valid || !is_valid {
            comparison.not_comparable += old.variants.len() + new.variants.len();

            // Nothing to compare a newly compiling program's variants with, but they still
            // mustn't be unsound, slow or crashed
            for variant in &new.variants {
                if is_regression(Category::Rejected, variant.outcome.category) {
                    comparison.changes.push(Change {
                        seed,
                        variant: variant.name.clone(),
                        kind: ChangeKind::Regression,
                        before: "none".to_string(),
                        after: describe(&variant.outcome),
                    });
                }
            }
            continue;
        }

        let matching = |variant: &crate::report::VariantRecord,
                        others: &[crate::report::VariantRecord]| {
            others
                .iter()
                .position(|other| other.name == variant.name && other.source == variant.source)
        };

        for other in &new.variants {
            if matching(other, &old.variants).is_none() {
                comparison.not_comparable += 1;
            }
        }

        for variant in &old.variants {
            let Some(index) = matching(variant, &new.variants) else {
                comparison.not_comparable += 1;
                continue;
            };
            let other = &new.variants[index];

            *comparison
                .transitions
                .entry((variant.outcome.category, other.outcome.category))
                .or_default() += 1;

            if variant.outcome.fingerprint.is_none() || other.outcome.fingerprint.is_none() {
                comparison.without_fingerprints += 1;
            }

            if let Some(kind) = change_kind(&variant.outcome, &other.outcome) {
                comparison.changes.push(Change {
                    seed,
                    variant: variant.name.clone(),
                    kind,
                    before: with_fingerprint(
                        describe(&variant.outcome),
                        kind,
                        variant.outcome.fingerprint.as_deref(),
                    ),
                    after: with_fingerprint(
                        describe(&other.outcome),
                        kind,
                        other.outcome.fingerprint.as_deref(),
                    ),
                });
            }
        }
    }

    comparison.changes.sort_by(|a, b| {
        let key = |change: &Change| {
            (
                change.seed,
                change.variant != "explicit",
                change.variant.clone(),
            )
        };
        key(a).cmp(&key(b))
    });
    comparison
}

impl Comparison {
    /// The report, in the format the module's doc comment describes.
    pub fn report(&self) -> String {
        let mut out = String::new();
        let _ = writeln!(out, "# Comparison\n");
        let _ = write!(out, "| before \\ after |");
        for category in Category::ALL {
            let _ = write!(out, " {category:?} |");
        }
        let _ = writeln!(out, "\n|---|{}", "---|".repeat(Category::ALL.len()));
        for from in Category::ALL {
            let _ = write!(out, "| {from:?} |");
            for to in Category::ALL {
                let count = self
                    .transitions
                    .get(&(from, to))
                    .copied()
                    .unwrap_or_default();
                let _ = write!(out, " {count} |");
            }
            let _ = writeln!(out);
        }

        let compared: usize = self.transitions.values().sum();
        let newly_inferred: usize = self
            .transitions
            .iter()
            .filter(|((from, to), _)| *from != Category::Inferred && *to == Category::Inferred)
            .map(|(_, count)| count)
            .sum();
        let _ = writeln!(
            out,
            "\nCompared {compared} variants: {} changed, {} regressed, {newly_inferred} newly \
             inferred, {} not comparable, {} variants and explicit programs without \
             fingerprints.",
            self.changes.len(),
            self.regressions().count(),
            self.not_comparable,
            self.without_fingerprints
        );

        if !self.changes.is_empty() {
            let _ = writeln!(out, "\n## Changes\n");
            for change in &self.changes {
                let quote = |text: &str| serde_json::to_string(text).expect("strings serialize");
                let _ = writeln!(
                    out,
                    "- seed {} `{}` [{}]: {} -> {}",
                    change.seed,
                    change.variant,
                    change.kind.name(),
                    quote(&change.before),
                    quote(&change.after)
                );
            }
        }

        out
    }
}
