use std::{
    fs, io,
    path::{Path, PathBuf},
};

use thiserror::Error;

use crate::{CompilerOptions, ForbiddenLint, FormatOptions, Manifest, UnknownLint};

#[derive(Debug, Error)]
pub enum Error {
    #[error("IO error: {0}")]
    Io(#[from] io::Error),

    /// The manifest at the given path couldn't be parsed.
    #[error("TOML error: {error}")]
    Toml {
        path: PathBuf,
        error: toml::de::Error,
    },

    #[error("Missing parent directory")]
    MissingParent,
}

#[derive(Debug, Clone)]
pub struct Project {
    pub manifest: Option<Manifest>,
    /// The path to the `Rue.toml` file, if there is one.
    pub manifest_path: Option<PathBuf>,
    pub options: CompilerOptions,
    pub format_options: FormatOptions,
    pub entrypoint: PathBuf,
    /// Names in the manifest's `[lints]` section that don't refer to a known lint or group.
    pub unknown_lints: Vec<UnknownLint>,
    /// Entries in the manifest's `[lints]` section that try to change the level of a lint that a
    /// group forbids, which are errors.
    pub forbidden_lints: Vec<ForbiddenLint>,
}

/// Finds the project that a path belongs to.
///
/// The nearest `Rue.toml` in the path or its ancestors is used if there is one, so a project with
/// a manifest is found from anywhere inside it, even from a directory with its own `main.rue`.
/// Otherwise the nearest directory with a `main.rue` is the project, and otherwise a `.rue` file
/// is a project by itself.
pub fn find_project(path: &Path, debug: bool) -> Result<Option<Project>, Error> {
    // A path to a `Rue.toml` file is found as the manifest of its parent directory
    let manifest_path = path
        .ancestors()
        .map(|ancestor| ancestor.join("Rue.toml"))
        .find(|manifest| manifest.is_file());

    if let Some(path) = manifest_path {
        let manifest: Manifest =
            toml::from_str(&fs::read_to_string(&path)?).map_err(|error| Error::Toml {
                path: path.clone(),
                error,
            })?;
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

        let problems = manifest.apply_lints(&mut options.lints);

        return Ok(Some(Project {
            format_options: manifest.formatter,
            manifest: Some(manifest),
            manifest_path: Some(path),
            options,
            entrypoint,
            unknown_lints: problems.unknown,
            forbidden_lints: problems.forbidden,
        }));
    }

    let entrypoint = path
        .ancestors()
        .find(|ancestor| ancestor.is_dir() && ancestor.join("main.rue").exists())
        .or_else(|| {
            (path.is_file() && path.extension().is_some_and(|ext| ext == "rue")).then_some(path)
        });

    Ok(entrypoint.map(|entrypoint| Project {
        manifest: None,
        manifest_path: None,
        options: if debug {
            CompilerOptions::debug()
        } else {
            CompilerOptions::default()
        },
        format_options: FormatOptions::default(),
        entrypoint: entrypoint.to_path_buf(),
        unknown_lints: Vec::new(),
        forbidden_lints: Vec::new(),
    }))
}

#[cfg(test)]
mod tests {
    use std::sync::atomic::{AtomicUsize, Ordering};

    use rue_diagnostic::{Lint, LintLevel};

    use super::*;

    /// A directory under the system's temporary directory, which is removed when it's dropped.
    struct TempDir(PathBuf);

    impl TempDir {
        fn new() -> Self {
            static COUNTER: AtomicUsize = AtomicUsize::new(0);

            let path = std::env::temp_dir().join(format!(
                "rue-options-{}-{}",
                std::process::id(),
                COUNTER.fetch_add(1, Ordering::SeqCst)
            ));
            fs::remove_dir_all(&path).ok();
            fs::create_dir_all(&path).unwrap();

            // `find_project` searches the parent directories, so a project above the temporary
            // directory would be found instead of the one a test sets up
            for ancestor in path.ancestors().skip(1) {
                for name in ["Rue.toml", "main.rue"] {
                    assert!(
                        !ancestor.join(name).exists(),
                        "{} would be found by these tests; remove it or set TMPDIR elsewhere",
                        ancestor.join(name).display()
                    );
                }
            }

            Self(path)
        }

        /// Writes a file at a path relative to the directory, creating its parents.
        fn write(&self, path: &str, contents: &str) -> PathBuf {
            let path = self.0.join(path);
            fs::create_dir_all(path.parent().unwrap()).unwrap();
            fs::write(&path, contents).unwrap();
            path
        }
    }

    impl Drop for TempDir {
        fn drop(&mut self) {
            fs::remove_dir_all(&self.0).ok();
        }
    }

    fn find(path: &Path, debug: bool) -> Project {
        find_project(path, debug)
            .unwrap()
            .expect("project should be found")
    }

    #[test]
    fn applies_manifest_lints() {
        let dir = TempDir::new();
        dir.write(
            "Rue.toml",
            "[compiler]\nentrypoint = \"puzzles\"\n\n[lints]\nunused_binding = \"deny\"\nunused_import = \"forbid\"\ntypo_lint = \"allow\"\n",
        );

        let project = find(&dir.0, false);

        assert_eq!(
            project.options.lints.get(Lint::UnusedBinding),
            LintLevel::Deny
        );
        assert_eq!(
            project.options.lints.get(Lint::UnusedImport),
            LintLevel::Forbid
        );
        assert_eq!(
            project
                .unknown_lints
                .iter()
                .map(|lint| lint.name.as_str())
                .collect::<Vec<_>>(),
            vec!["typo_lint"]
        );
    }

    #[test]
    fn manifest_entrypoint_is_relative_to_the_manifest() {
        let dir = TempDir::new();
        let manifest = dir.write(
            "Rue.toml",
            "[compiler]\nentrypoint = \"src\"\n\n[formatter]\nmax_width = 80\n",
        );

        for path in [dir.0.clone(), manifest.clone()] {
            let project = find(&path, false);

            assert_eq!(project.entrypoint, dir.0.join("src"));
            assert_eq!(project.manifest_path.as_deref(), Some(manifest.as_path()));
            assert_eq!(project.options, CompilerOptions::default());
            assert_eq!(
                project.format_options,
                FormatOptions {
                    max_width: 80,
                    indent_width: 4,
                }
            );
            assert!(project.manifest.is_some());
            assert!(project.unknown_lints.is_empty());
            assert!(project.forbidden_lints.is_empty());
        }
    }

    #[test]
    fn manifest_is_found_from_a_file_inside_of_the_project() {
        let dir = TempDir::new();
        let manifest = dir.write("Rue.toml", "[compiler]\n");
        let file = dir.write("puzzles/nested/main.rue", "");

        let project = find(&file, false);

        assert_eq!(project.manifest_path.as_deref(), Some(manifest.as_path()));
        assert_eq!(project.entrypoint, dir.0.join("puzzles"));
    }

    #[test]
    fn manifest_takes_precedence_over_a_main_file() {
        let dir = TempDir::new();
        dir.write("Rue.toml", "[compiler]\n");
        dir.write("puzzles/main.rue", "");

        let project = find(&dir.0.join("puzzles"), false);

        assert!(project.manifest.is_some());
        assert_eq!(project.entrypoint, dir.0.join("puzzles"));
    }

    #[test]
    fn manifest_can_disable_the_standard_library() {
        let dir = TempDir::new();
        dir.write("Rue.toml", "[compiler]\nstd = false\n");

        assert_eq!(
            find(&dir.0, false).options,
            CompilerOptions {
                std: false,
                ..CompilerOptions::default()
            }
        );
        assert_eq!(
            find(&dir.0, true).options,
            CompilerOptions {
                std: false,
                ..CompilerOptions::debug()
            }
        );

        // Enabling it explicitly is the same as leaving it out
        dir.write("Rue.toml", "[compiler]\nstd = true\n");
        assert!(find(&dir.0, false).options.std);
    }

    #[test]
    fn manifest_lints_apply_to_debug_options() {
        let dir = TempDir::new();
        dir.write(
            "Rue.toml",
            "[compiler]\n\n[lints]\nunused_binding = \"deny\"\n",
        );

        let project = find(&dir.0, true);

        assert!(project.options.debug_symbols);
        assert_eq!(
            project.options.lints.get(Lint::UnusedBinding),
            LintLevel::Deny
        );
    }

    #[test]
    fn invalid_manifest_is_an_error_with_its_path() {
        let dir = TempDir::new();
        let manifest = dir.write("Rue.toml", "[compiler\n");

        let Err(Error::Toml { path, .. }) = find_project(&dir.0, false) else {
            panic!("expected a TOML error");
        };

        assert_eq!(path, manifest);
    }

    #[test]
    fn directory_with_a_main_file_is_a_project_without_a_manifest() {
        let dir = TempDir::new();
        dir.write("main.rue", "");

        for (debug, options) in [
            (false, CompilerOptions::default()),
            (true, CompilerOptions::debug()),
        ] {
            let project = find(&dir.0, debug);

            assert!(project.manifest.is_none());
            assert!(project.manifest_path.is_none());
            assert_eq!(project.entrypoint, dir.0);
            assert_eq!(project.options, options);
            assert_eq!(project.format_options, FormatOptions::default());
        }
    }

    #[test]
    fn rue_file_is_a_project_without_a_manifest() {
        let dir = TempDir::new();
        let file = dir.write("puzzle.rue", "");

        for (debug, options) in [
            (false, CompilerOptions::default()),
            (true, CompilerOptions::debug()),
        ] {
            let project = find(&file, debug);

            assert!(project.manifest.is_none());
            assert_eq!(project.entrypoint, file);
            assert_eq!(project.options, options);
        }
    }

    #[test]
    fn nearest_manifest_is_used() {
        let dir = TempDir::new();
        dir.write("Rue.toml", "[compiler]\n");
        let inner = dir.write("inner/Rue.toml", "[compiler]\nentrypoint = \"src\"\n");
        let file = dir.write("inner/src/main.rue", "");

        let project = find(&file, false);

        assert_eq!(project.manifest_path.as_deref(), Some(inner.as_path()));
        assert_eq!(project.entrypoint, dir.0.join("inner/src"));
    }

    #[test]
    fn nearest_main_file_is_used() {
        let dir = TempDir::new();
        dir.write("main.rue", "");
        dir.write("inner/main.rue", "");
        let file = dir.write("inner/deeper/other.rue", "");

        assert_eq!(
            find(&dir.0.join("inner"), false).entrypoint,
            dir.0.join("inner")
        );
        assert_eq!(find(&file, false).entrypoint, dir.0.join("inner"));
        assert_eq!(find(&dir.0, false).entrypoint, dir.0);
    }

    #[test]
    fn manifest_further_up_is_used_over_a_main_file() {
        let dir = TempDir::new();
        let manifest = dir.write("Rue.toml", "[compiler]\nentrypoint = \"src\"\n");
        dir.write("src/main.rue", "");
        dir.write("src/inner/main.rue", "");

        let project = find(&dir.0.join("src/inner"), false);

        assert_eq!(project.manifest_path.as_deref(), Some(manifest.as_path()));
        assert_eq!(project.entrypoint, dir.0.join("src"));
    }

    #[test]
    fn rue_file_next_to_a_main_file_uses_its_directory() {
        let dir = TempDir::new();
        dir.write("main.rue", "");
        let file = dir.write("other.rue", "");

        assert_eq!(find(&file, false).entrypoint, dir.0);
    }

    #[test]
    fn other_paths_are_not_projects() {
        let dir = TempDir::new();
        let file = dir.write("notes.txt", "");

        assert!(find_project(&dir.0, false).unwrap().is_none());
        assert!(find_project(&file, false).unwrap().is_none());
        assert!(
            find_project(&dir.0.join("missing.rue"), false)
                .unwrap()
                .is_none()
        );
    }
}
