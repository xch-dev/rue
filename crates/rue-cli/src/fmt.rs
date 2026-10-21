use std::{
    collections::BTreeSet,
    env, fs,
    path::{Path, PathBuf},
    process,
};

use anyhow::{Context, Result, bail};
use clap::Parser;
use colored::Colorize;
use glob::glob;
use rue_formatter::{FormatError, FormatOptions, format_source};
use rue_options::find_project;
use walkdir::WalkDir;

#[derive(Debug, Parser)]
pub struct FmtArgs {
    #[clap(value_name = "PATH_OR_GLOB")]
    paths: Vec<String>,
    #[clap(long)]
    check: bool,
}

pub fn format(args: &FmtArgs) -> Result<()> {
    let current_dir = env::current_dir()?.canonicalize()?;
    let targets = resolve_targets(&args.paths, &current_dir)?;
    let mut pending = Vec::new();

    for path in targets {
        let source = fs::read_to_string(&path)
            .with_context(|| format!("Failed to read {}", path.display()))?;
        let options = find_project(&path, false)
            .with_context(|| format!("Failed to resolve project for {}", path.display()))?
            .map_or_else(FormatOptions::default, |project| project.format_options);
        let formatted = match format_source(&source, &options) {
            Ok(formatted) => formatted,
            // Files with syntax errors can't be formatted, but that shouldn't prevent formatting
            // the rest of the files
            Err(FormatError::Parse { .. }) => {
                eprintln!(
                    "{}",
                    format!(
                        "Skipped {}, since it contains syntax errors",
                        display_path(&path, &current_dir)
                    )
                    .yellow()
                    .bold()
                );
                continue;
            }
            Err(error) => {
                return Err(error).with_context(|| format!("Failed to format {}", path.display()));
            }
        };
        if formatted != source {
            pending.push((path, formatted));
        }
    }

    if args.check {
        for (path, _) in &pending {
            eprintln!(
                "{}",
                format!("Needs formatting: {}", display_path(path, &current_dir))
                    .yellow()
                    .bold()
            );
        }
        if !pending.is_empty() {
            process::exit(1);
        }
        return Ok(());
    }

    for (path, formatted) in pending {
        fs::write(&path, formatted)
            .with_context(|| format!("Failed to write {}", path.display()))?;
        println!(
            "{}",
            format!("Formatted {}", display_path(&path, &current_dir))
                .green()
                .bold()
        );
    }

    Ok(())
}

fn resolve_targets(inputs: &[String], current_dir: &Path) -> Result<Vec<PathBuf>> {
    let mut targets = BTreeSet::new();

    if inputs.is_empty() {
        let project = find_project(current_dir, false)?;
        let Some(project) = project else {
            bail!("No project found");
        };
        collect_path(&project.entrypoint, &mut targets)?;
    } else {
        for input in inputs {
            let path = current_dir.join(input);
            if path.exists() {
                if !collect_path(&path, &mut targets)? {
                    bail!("No Rue files found for `{input}`");
                }
                continue;
            }

            let pattern = if Path::new(input).is_absolute() {
                input.clone()
            } else {
                current_dir.join(input).to_string_lossy().into_owned()
            };
            let mut matched = false;
            let mut found_rue = false;
            for entry in
                glob(&pattern).with_context(|| format!("Invalid glob pattern `{input}`"))?
            {
                let path =
                    entry.with_context(|| format!("Failed to expand glob pattern `{input}`"))?;
                matched = true;
                found_rue |= collect_path(&path, &mut targets)?;
            }
            if !matched {
                bail!("Path or glob `{input}` did not match any paths");
            }
            if !found_rue {
                bail!("No Rue files found for `{input}`");
            }
        }
    }

    if targets.is_empty() {
        bail!("No Rue files found");
    }

    Ok(targets.into_iter().collect())
}

fn collect_path(path: &Path, targets: &mut BTreeSet<PathBuf>) -> Result<bool> {
    if path.is_file() {
        if !is_rue_file(path) {
            bail!("Expected a Rue source file, found {}", path.display());
        }
        targets.insert(path.canonicalize()?);
        return Ok(true);
    }

    if !path.is_dir() {
        bail!("Path does not exist: {}", path.display());
    }

    let mut found_rue = false;
    for entry in WalkDir::new(path).follow_links(false) {
        let entry = entry.with_context(|| format!("Failed to walk {}", path.display()))?;
        if entry.file_type().is_file() && is_rue_file(entry.path()) {
            targets.insert(entry.path().canonicalize()?);
            found_rue = true;
        }
    }

    Ok(found_rue)
}

fn is_rue_file(path: &Path) -> bool {
    path.extension().is_some_and(|extension| extension == "rue")
}

fn display_path(path: &Path, current_dir: &Path) -> String {
    path.strip_prefix(current_dir)
        .unwrap_or(path)
        .display()
        .to_string()
}
