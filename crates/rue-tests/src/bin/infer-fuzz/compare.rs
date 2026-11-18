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
//! output, or its first diagnostic differs. Outputs are only compared when both runs recorded them.
//! A change is a **regression** when the variant was inferred and no longer is, or becomes
//! unsound, slow or crashed. An explicit program that compiled and no longer does is a regression
//! too, and its variants aren't compared.
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
//! Compared 11638 variants: 12 changed, 0 regressed, 3 newly inferred, 0 not comparable.
//!
//! ## Changes
//!
//! - seed 42 `all` [regression]: Inferred -> Rejected: Cannot infer generic `T` for `f`; ... at 3:5
//! - seed 57 `one` [diagnostic]: Rejected: <message> at 4:1 -> Rejected: <message> at 4:1
//! ```
//!
//! Each change is one line: `- seed <seed> `<variant>` [<kind>]: <before> -> <after>`, sorted by
//! seed and then variant. `<kind>` is `regression`, `status`, `output` or `diagnostic`, the first
//! of them that applies. `<variant>` is `all`, `some`, `one`, or `explicit` for an explicit
//! program that no longer compiles. Each side is the status, then `: ` and the first diagnostic
//! with its position, the output, or the detail, when there is one. The command exits with status
//! 1 if there are any regressions.

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
        .map(|line| Ok(serde_json::from_str(line)?))
        .collect()
}

/// What changed about a variant, the first that applies.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ChangeKind {
    Regression,
    Status,
    Output,
    Diagnostic,
}

impl ChangeKind {
    fn name(self) -> &'static str {
        match self {
            ChangeKind::Regression => "regression",
            ChangeKind::Status => "status",
            ChangeKind::Output => "output",
            ChangeKind::Diagnostic => "diagnostic",
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
    /// Variants whose source differs between the runs, or whose seed is only in one of them.
    pub not_comparable: usize,
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
        (None, Some(output), _) => format!("{category}: {output}"),
        (None, None, Some(detail)) => format!("{category}: {detail}"),
        _ => category,
    }
}

fn is_regression(before: Category, after: Category) -> bool {
    (before == Category::Inferred && after != Category::Inferred)
        || (matches!(
            after,
            Category::Unsound | Category::Slow | Category::Crashed
        ) && after != before)
}

/// What changed between the two outcomes of a variant, if anything.
fn change_kind(before: &Outcome, after: &Outcome) -> Option<ChangeKind> {
    if is_regression(before.category, after.category) {
        Some(ChangeKind::Regression)
    } else if before.category != after.category {
        Some(ChangeKind::Status)
    } else if before.output.is_some() && after.output.is_some() && before.output != after.output {
        Some(ChangeKind::Output)
    } else if before.diag != after.diag {
        Some(ChangeKind::Diagnostic)
    } else {
        None
    }
}

pub fn compare(before: &[SeedRecord], after: &[SeedRecord]) -> Comparison {
    let mut comparison = Comparison::default();
    let after: BTreeMap<u64, &SeedRecord> =
        after.iter().map(|record| (record.seed, record)).collect();

    for old in before {
        let Some(new) = after.get(&old.seed) else {
            comparison.not_comparable += old.variants.len();
            continue;
        };

        if old.explicit_source != new.explicit_source {
            comparison.not_comparable += old.variants.len();
            continue;
        }

        // An explicit program that no longer compiles is a regression of its own, whatever its
        // variants did
        if matches!(old.explicit, Explicit::Valid { .. })
            && !matches!(new.explicit, Explicit::Valid { .. })
        {
            comparison.changes.push(Change {
                seed: old.seed,
                variant: "explicit".to_string(),
                kind: ChangeKind::Regression,
                before: "Valid".to_string(),
                after: format!("{:?}", new.explicit),
            });
            comparison.not_comparable += old.variants.len();
            continue;
        }

        for variant in &old.variants {
            let Some(other) = new
                .variants
                .iter()
                .find(|other| other.name == variant.name && other.source == variant.source)
            else {
                comparison.not_comparable += 1;
                continue;
            };

            *comparison
                .transitions
                .entry((variant.outcome.category, other.outcome.category))
                .or_default() += 1;

            if let Some(kind) = change_kind(&variant.outcome, &other.outcome) {
                comparison.changes.push(Change {
                    seed: old.seed,
                    variant: variant.name.clone(),
                    kind,
                    before: describe(&variant.outcome),
                    after: describe(&other.outcome),
                });
            }
        }
    }

    comparison
        .changes
        .sort_by(|a, b| (a.seed, &a.variant).cmp(&(b.seed, &b.variant)));
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
             inferred, {} not comparable.",
            self.changes.len(),
            self.regressions().count(),
            self.not_comparable
        );

        if !self.changes.is_empty() {
            let _ = writeln!(out, "\n## Changes\n");
            for change in &self.changes {
                let _ = writeln!(
                    out,
                    "- seed {} `{}` [{}]: {} -> {}",
                    change.seed,
                    change.variant,
                    change.kind.name(),
                    change.before,
                    change.after
                );
            }
        }

        out
    }
}
