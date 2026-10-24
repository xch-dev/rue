use std::{
    fs, io,
    path::{Path, PathBuf},
};

use thiserror::Error;

use crate::{CompilerOptions, FormatOptions, Manifest};

#[derive(Debug, Error)]
pub enum Error {
    #[error("IO error: {0}")]
    Io(#[from] io::Error),

    #[error("TOML error: {0}")]
    Toml(#[from] toml::de::Error),

    #[error("Missing parent directory")]
    MissingParent,
}

#[derive(Debug, Clone)]
pub struct Project {
    pub manifest: Option<Manifest>,
    pub options: CompilerOptions,
    pub format_options: FormatOptions,
    pub entrypoint: PathBuf,
    /// Names in the manifest's `[lints]` section that don't refer to a known lint.
    pub unknown_lints: Vec<String>,
}

pub fn find_project(path: &Path, debug: bool) -> Result<Option<Project>, Error> {
    if path.is_file() && path.file_name().is_some_and(|name| name == "Rue.toml") {
        let manifest: Manifest = toml::from_str(&fs::read_to_string(path)?)?;
        let entrypoint = path
            .parent()
            .ok_or(Error::MissingParent)?
            .join(&manifest.compiler.entrypoint);

        let mut options = if debug {
            CompilerOptions::debug()
        } else {
            CompilerOptions::default()
        };

        if let Some(false) = manifest.compiler.std {
            options.std = false;
        }

        let unknown_lints = manifest.apply_lints(&mut options.lints);

        return Ok(Some(Project {
            format_options: manifest.formatter,
            manifest: Some(manifest),
            options,
            entrypoint,
            unknown_lints,
        }));
    }

    if path.is_dir() && path.join("Rue.toml").exists() {
        return find_project(&path.join("Rue.toml"), debug);
    }

    if let Some(parent) = path.parent()
        && let Some(project) = find_project(parent, debug)?
    {
        return Ok(Some(project));
    }

    if path.is_dir() && path.join("main.rue").exists() {
        return Ok(Some(Project {
            manifest: None,
            options: if debug {
                CompilerOptions::debug()
            } else {
                CompilerOptions::default()
            },
            format_options: FormatOptions::default(),
            entrypoint: path.to_path_buf(),
            unknown_lints: Vec::new(),
        }));
    }

    if path.is_file() && path.extension().is_some_and(|ext| ext == "rue") {
        return Ok(Some(Project {
            manifest: None,
            options: if debug {
                CompilerOptions::debug()
            } else {
                CompilerOptions::default()
            },
            format_options: FormatOptions::default(),
            entrypoint: path.to_path_buf(),
            unknown_lints: Vec::new(),
        }));
    }

    Ok(None)
}

#[cfg(test)]
mod tests {
    use rue_diagnostic::{Lint, LintLevel};

    use super::*;

    #[test]
    fn applies_manifest_lints() -> Result<(), Error> {
        let dir = std::env::temp_dir().join(format!("rue-options-lints-{}", std::process::id()));
        fs::create_dir_all(dir.join("puzzles"))?;
        fs::write(
            dir.join("Rue.toml"),
            "[compiler]\nentrypoint = \"puzzles\"\n\n[lints]\nunused_binding = \"deny\"\ntypo_lint = \"allow\"\n",
        )?;

        let project = find_project(&dir, false);
        fs::remove_dir_all(&dir)?;
        let project = project?.expect("project should be found");

        assert_eq!(
            project.options.lints.get(Lint::UnusedBinding),
            LintLevel::Deny
        );
        assert_eq!(project.unknown_lints, vec!["typo_lint".to_string()]);

        Ok(())
    }
}
