use std::{
    fs, io,
    path::{Path, PathBuf},
};

use thiserror::Error;

use crate::{CompilerOptions, Manifest};

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
    pub entrypoint: PathBuf,
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

        return Ok(Some(Project {
            manifest: Some(manifest),
            options,
            entrypoint,
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
            entrypoint: path.to_path_buf(),
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
            entrypoint: path.to_path_buf(),
        }));
    }

    Ok(None)
}
