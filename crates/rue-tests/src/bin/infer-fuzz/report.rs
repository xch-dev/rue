//! The results of a run: a record for each seed, the clusters of failures, and the summary.

use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write as _;
use std::fs;
use std::path::Path;

use anyhow::Result;
use serde::{Deserialize, Serialize};

use crate::oracle::{Category, Diag, Outcome};
use crate::worker::{Explicit, Shrunk};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VariantRecord {
    pub name: String,
    pub source: String,
    pub shape: String,
    pub tags: Vec<String>,
    pub outcome: Outcome,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SeedRecord {
    pub seed: u64,
    pub explicit_source: String,
    pub explicit: Explicit,
    pub variants: Vec<VariantRecord>,
}

/// The record of a seed, from records sorted by seed.
pub fn record(records: &[SeedRecord], seed: u64) -> &SeedRecord {
    let index = records
        .binary_search_by_key(&seed, |record| record.seed)
        .expect("every seed has a record");
    &records[index]
}

fn variant(records: &[SeedRecord], (seed, index): (u64, usize)) -> &VariantRecord {
    &record(records, seed).variants[index]
}

/// A failing variant, as (seed, variant index).
pub type Member = (u64, usize);

/// A group of failures with the same category and first diagnostic.
#[derive(Debug)]
pub struct Cluster {
    pub key: String,
    pub category: Category,
    /// The failing variants, as (seed, variant index).
    pub members: Vec<(u64, usize)>,
    /// How many of the `one` variants had each shape.
    pub shapes: BTreeMap<String, usize>,
    pub shrunk: Vec<(Member, Shrunk)>,
}

/// Groups failures by category and first diagnostic, or the first line of a panic.
pub fn cluster_key(outcome: &Outcome) -> String {
    let category = format!("{:?}", outcome.category);
    match (&outcome.diag, &outcome.detail) {
        (Some(diag), _) => format!("{category}: {}", diag.key()),
        (None, Some(detail)) if outcome.category == Category::Crashed => {
            format!("{category}: {}", detail.lines().next().unwrap_or_default())
        }
        _ => category,
    }
}

/// The clusters of failures, largest first. Their order is the numbering in the report and of
/// the directories under `clusters/`.
pub fn clusters(records: &[SeedRecord]) -> Vec<Cluster> {
    let mut clusters: BTreeMap<String, Cluster> = BTreeMap::new();
    for record in records {
        for (index, variant) in record.variants.iter().enumerate() {
            if variant.outcome.category == Category::Inferred {
                continue;
            }
            let key = cluster_key(&variant.outcome);
            let cluster = clusters.entry(key.clone()).or_insert_with(|| Cluster {
                key,
                category: variant.outcome.category,
                members: Vec::new(),
                shapes: BTreeMap::new(),
                shrunk: Vec::new(),
            });
            cluster.members.push((record.seed, index));
            if variant.name == "one" {
                *cluster.shapes.entry(variant.shape.clone()).or_default() += 1;
            }
        }
    }
    let mut clusters: Vec<Cluster> = clusters.into_values().collect();
    clusters.sort_by_key(|cluster| std::cmp::Reverse(cluster.members.len()));
    clusters
}

/// Which failures to shrink: up to `limit` of each cluster (at least 10 for the rare categories),
/// taken in turn from each set of tags (see `shrink::tags`), so that the shapes in a large
/// cluster are all sampled. Within a set, `one` variants come first, since a single site is the
/// most specific, and then the shortest. Slow failures aren't shrunk.
pub fn shrink_jobs(
    records: &[SeedRecord],
    clusters: &[Cluster],
    limit: usize,
) -> Vec<(usize, (u64, usize))> {
    let mut jobs = Vec::new();
    for (number, cluster) in clusters.iter().enumerate() {
        if cluster.category == Category::Slow {
            continue;
        }

        let mut by_tags: BTreeMap<&[String], Vec<(u64, usize)>> = BTreeMap::new();
        for &member in &cluster.members {
            by_tags
                .entry(&variant(records, member).tags)
                .or_default()
                .push(member);
        }
        let mut groups: Vec<Vec<(u64, usize)>> = by_tags.into_values().collect();
        for group in &mut groups {
            // Reversed, so that `pop` takes the best first
            group.sort_by_key(|&member| {
                let variant = variant(records, member);
                std::cmp::Reverse((variant.name != "one", variant.source.len()))
            });
        }
        groups.sort_by_key(|group| std::cmp::Reverse(group.len()));

        let limit = if cluster.category == Category::Rejected {
            limit
        } else {
            limit.max(10)
        };
        let mut taken = 0;
        while taken < limit && groups.iter().any(|group| !group.is_empty()) {
            for group in &mut groups {
                if taken < limit
                    && let Some(member) = group.pop()
                {
                    jobs.push((number, member));
                    taken += 1;
                }
            }
        }
    }
    jobs
}

/// The summary: counts for each category, and each cluster with its shapes and reproductions.
pub fn report(records: &[SeedRecord], clusters: &[Cluster]) -> String {
    let mut out = String::new();

    write_programs(&mut out, records);
    write_counts(&mut out, records);

    let _ = writeln!(out, "## Clusters\n");
    for (index, cluster) in clusters.iter().enumerate() {
        write_cluster(&mut out, records, index + 1, cluster);
    }

    out
}

/// How many programs were valid, and why the others were discarded.
fn write_programs(out: &mut String, records: &[SeedRecord]) {
    let mut discarded: BTreeMap<&str, usize> = BTreeMap::new();
    let mut rejections: BTreeMap<String, usize> = BTreeMap::new();
    let mut valid = 0;
    for record in records {
        let reason = match &record.explicit {
            Explicit::Valid { .. } => {
                valid += 1;
                continue;
            }
            Explicit::Rejected(diag) => {
                *rejections.entry(diag.key()).or_default() += 1;
                "explicit program rejected"
            }
            Explicit::Panicked(_) => "explicit program panicked",
            Explicit::NoSites => "no generic arguments to remove",
            Explicit::TimedOut => "explicit program timed out",
            Explicit::Died(_) => "explicit program crashed the worker",
        };
        *discarded.entry(reason).or_default() += 1;
    }

    let _ = writeln!(out, "# Inference fuzzer results\n");
    let _ = writeln!(
        out,
        "{} seeds, {valid} valid programs (they compile with every generic argument written \
         out).\n",
        records.len()
    );
    for (reason, count) in &discarded {
        let _ = writeln!(out, "- {count} discarded: {reason}");
    }
    let _ = writeln!(out);

    if !rejections.is_empty() {
        let _ = writeln!(
            out,
            "Why explicit programs were rejected (generator noise, not inference):\n"
        );
        let mut rejections: Vec<_> = rejections.into_iter().collect();
        rejections.sort_by_key(|(_, count)| std::cmp::Reverse(*count));
        for (reason, count) in rejections.iter().take(8) {
            let _ = writeln!(out, "- {count}: {reason}");
        }
        let _ = writeln!(out);
    }
}

/// A table of each variant kind against each category.
fn write_counts(out: &mut String, records: &[SeedRecord]) {
    let mut table: BTreeMap<&str, BTreeMap<Category, usize>> = BTreeMap::new();
    let mut programs: BTreeMap<Category, usize> = BTreeMap::new();
    let mut same_clvm = [0, 0];
    for record in records {
        for variant in &record.variants {
            let category = variant.outcome.category;
            for row in [variant.name.as_str(), "total"] {
                *table.entry(row).or_default().entry(category).or_default() += 1;
            }
            if category == Category::Inferred {
                same_clvm[usize::from(variant.outcome.same_clvm)] += 1;
            }
        }
        if let Some(worst) = record
            .variants
            .iter()
            .map(|variant| variant.outcome.category)
            .max()
        {
            *programs.entry(worst).or_default() += 1;
        }
    }
    table.insert("programs (worst variant)", programs);

    let _ = writeln!(out, "## Counts\n");
    let _ = write!(out, "| variant |");
    for category in Category::ALL {
        let _ = write!(out, " {category:?} |");
    }
    let _ = writeln!(out, "\n|---|{}", "---|".repeat(Category::ALL.len()));
    for row in ["all", "some", "one", "total", "programs (worst variant)"] {
        let Some(counts) = table.get(row) else {
            continue;
        };
        let _ = write!(out, "| {row} |");
        for category in Category::ALL {
            let count = counts.get(&category).copied().unwrap_or_default();
            let _ = write!(out, " {count} |");
        }
        let _ = writeln!(out);
    }
    let _ = writeln!(
        out,
        "\nOf the inferred variants, {} compile to the same CLVM as the explicit program, and {} \
         to different CLVM with the same output.\n",
        same_clvm[1], same_clvm[0]
    );
}

fn write_cluster(out: &mut String, records: &[SeedRecord], number: usize, cluster: &Cluster) {
    let programs: BTreeSet<u64> = cluster.members.iter().map(|(seed, _)| *seed).collect();
    let _ = writeln!(
        out,
        "### {number}. {}\n\n{} variants in {} programs. Shrunk reproductions are in \
         `clusters/{number:02}`.\n",
        cluster.key,
        cluster.members.len(),
        programs.len()
    );

    if !cluster.shapes.is_empty() {
        let _ = writeln!(out, "Shapes of the single-site variants:\n");
        let mut shapes: Vec<_> = cluster.shapes.iter().collect();
        shapes.sort_by_key(|(_, count)| std::cmp::Reverse(**count));
        for (shape, count) in shapes.iter().take(8) {
            let _ = writeln!(out, "- {count}: `{shape}`");
        }
        if shapes.len() > 8 {
            let _ = writeln!(out, "- ... and {} more", shapes.len() - 8);
        }
        let _ = writeln!(out);
    }

    // The shrunk failures, grouped by their tags, with the smallest of each group
    let mut groups: BTreeMap<String, Vec<&(Member, Shrunk)>> = BTreeMap::new();
    for shrunk in &cluster.shrunk {
        let tags = if shrunk.1.tags.is_empty() {
            "no tags".to_string()
        } else {
            shrunk.1.tags.join(", ")
        };
        groups.entry(tags).or_default().push(shrunk);
    }
    let mut groups: Vec<_> = groups.into_iter().collect();
    groups.sort_by_key(|(_, members)| std::cmp::Reverse(members.len()));

    for (tags, mut members) in groups {
        let _ = writeln!(out, "#### {} shrunk: {tags}\n", members.len());

        members.sort_by_key(|(_, shrunk)| shrunk.stripped.len());
        let ((seed, index), shrunk) = members[0];
        let original = variant(records, (*seed, *index));
        let before = original
            .outcome
            .diag
            .as_ref()
            .map(Diag::located)
            .or_else(|| original.outcome.detail.clone())
            .unwrap_or_default();
        let _ = writeln!(
            out,
            "Seed {seed}, variant `{}`: `{}`\n\n```rue\n{}```\n\n{}\n\nBefore shrinking: {before}\n",
            original.name, shrunk.shape, shrunk.stripped, shrunk.message
        );

        if members.len() > 1 {
            let others: Vec<String> = members[1..]
                .iter()
                .map(|((seed, index), _)| {
                    format!("{seed} `{}`", variant(records, (*seed, *index)).name)
                })
                .collect();
            let _ = writeln!(out, "Also: {}\n", others.join(", "));
        }
    }
}

/// Writes `results.jsonl`, `report.md`, and the shrunk reproductions under `clusters/NN/`,
/// numbered as in the report.
pub fn write_outputs(
    out: &Path,
    records: &[SeedRecord],
    clusters: &[Cluster],
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
    for (index, cluster) in clusters.iter().enumerate() {
        let cluster_dir = dir.join(format!("{:02}", index + 1));
        fs::create_dir_all(&cluster_dir)?;
        for ((seed, index), shrunk) in &cluster.shrunk {
            let name = &variant(records, (*seed, *index)).name;
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
