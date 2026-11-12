//! Finding the projects that files belong to, compiling them, and keeping track of what each one
//! reported, so that diagnostics and queries never come from a compile that's out of date.

use std::collections::HashSet;
use std::panic::{self, AssertUnwindSafe};
use std::path::{Path, PathBuf};
use std::sync::atomic::Ordering;

use indexmap::IndexMap;
use rue_compiler::{Compiler, FileTree, normalize_path};
use rue_diagnostic::{Source, SourceKind};
use rue_options::{Project, find_project};

use crate::cache::Cache;
use crate::paths;
use crate::worker::{Diagnostics, Snapshot, State};
use crate::{Task, diagnostic, invalid_manifest_diagnostics, manifest_diagnostics};

/// A project that diagnostics were published for.
#[derive(Debug, Clone)]
pub struct Tracked {
    /// The path the project was found from, as the client reaches it, which is used to find it
    /// again.
    path: PathBuf,
    /// The files whose diagnostics were published.
    files: HashSet<SourceKind>,
}

/// The project that a file belongs to.
struct FoundProject {
    /// The canonical path of the project's manifest, or its entrypoint if it doesn't have one,
    /// which identifies it however the client reaches it.
    key: PathBuf,
    /// The project's manifest or entrypoint, as the client reaches it.
    path: PathBuf,
    /// The directory the project is in, as the client reaches it.
    root: PathBuf,
    /// The project, or why its manifest is invalid.
    project: Result<Project, toml::de::Error>,
}

impl FoundProject {
    fn find(path: &Path) -> Option<Self> {
        let (path, project) = match find_project(path, false) {
            Ok(Some(project)) => (
                project
                    .manifest_path
                    .clone()
                    .unwrap_or_else(|| project.entrypoint.clone()),
                Ok(project),
            ),
            Err(rue_options::Error::Toml { path, error }) => (path, Err(error)),
            Ok(None) | Err(_) => return None,
        };

        let root = if path.is_dir() {
            path.clone()
        } else {
            path.parent().unwrap_or(&path).to_path_buf()
        };

        // Only the directory is canonicalized, since a manifest can be a symlink that's shared
        // between projects
        let key = if path.is_dir() {
            paths::canonical(&path)?
        } else {
            paths::canonical(&root)?.join(path.file_name()?)
        };

        Some(Self {
            key,
            path,
            root,
            project,
        })
    }
}

/// The result of compiling a project, which hasn't been recorded yet.
struct Compiled {
    key: PathBuf,
    path: PathBuf,
    diagnostics: Diagnostics,
    /// The compile, which answers queries about the project's files.
    cache: Option<(Cache<Compiler>, Vec<Source>)>,
    /// Whether the project doesn't exist anymore, so it's no longer tracked.
    removed: bool,
}

impl Compiled {
    fn removed(key: PathBuf) -> Self {
        Self {
            path: key.clone(),
            key,
            diagnostics: Diagnostics::new(),
            cache: None,
            removed: true,
        }
    }
}

impl State {
    /// Recompiles the projects that the changed files belong to, if diagnostics were reported for
    /// them, and the projects of the files the editor changed. Projects that aren't the editor's
    /// are only compiled once the user opened one of their files, so that the server doesn't
    /// report diagnostics for the whole workspace.
    pub fn recompile(&mut self, changed: &[PathBuf], edited: &[PathBuf]) -> Diagnostics {
        let snapshot = self.shared.snapshot();

        let mut lookups = edited.to_vec();
        let mut removed = Vec::new();

        // A project may not exist anymore, or may have become part of another one (for example,
        // if a manifest was deleted, or created in a parent directory). Its files are cleared, and
        // those open in the editor are compiled with whichever project they're part of now.
        for (key, tracked) in &self.published {
            if FoundProject::find(&tracked.path).is_some_and(|project| &project.key == key) {
                continue;
            }

            removed.push(key.clone());

            lookups.extend(
                tracked
                    .files
                    .iter()
                    .filter_map(|kind| self.shared.paths.open_path(kind)),
            );
        }

        // Each project is only compiled once, no matter how many of its files changed
        let mut projects = IndexMap::new();

        for path in lookups {
            if let Some(project) = FoundProject::find(&path) {
                projects.entry(project.key.clone()).or_insert(project);
            }
        }

        for path in changed {
            if let Some(project) = FoundProject::find(path)
                && self.published.contains_key(&project.key)
            {
                projects.entry(project.key.clone()).or_insert(project);
            }
        }

        let mut compiled = removed
            .into_iter()
            .map(Compiled::removed)
            .chain(
                projects
                    .values()
                    .map(|project| self.compile(project, &snapshot)),
            )
            .collect::<Vec<_>>();

        // A file that a project stops including keeps the diagnostics and cache of whichever
        // project was compiled last, so the other projects that include it are compiled too, and
        // recorded last, to replace them
        let dropped = compiled
            .iter()
            .flat_map(|compiled| {
                self.published
                    .get(&compiled.key)
                    .into_iter()
                    .flat_map(|tracked| &tracked.files)
                    .filter(|kind| !compiled.diagnostics.contains_key(*kind))
            })
            .cloned()
            .collect::<HashSet<_>>();

        let others = self
            .published
            .iter()
            .filter(|(key, tracked)| {
                !compiled.iter().any(|compiled| &compiled.key == *key)
                    && tracked.files.iter().any(|kind| dropped.contains(kind))
            })
            .filter_map(|(_, tracked)| FoundProject::find(&tracked.path))
            .collect::<Vec<_>>();

        compiled.extend(
            others
                .iter()
                .map(|project| self.compile(project, &snapshot)),
        );

        // Everything is compiled before any state is updated, so that a panic leaves the results
        // of the last compile in place
        let mut diagnostics = Diagnostics::new();

        for compiled in compiled {
            // The editor may have changed a project's files while it was compiling, in which case
            // its diagnostics are already out of date, so they aren't published, and the project
            // is compiled again with the edits. Queries still use it, since it's newer than the
            // last compile that was published.
            if self.shared.is_stale(&snapshot, compiled.diagnostics.keys()) {
                if let Some((cache, files)) = compiled.cache {
                    self.insert_project(compiled.key, cache, files);
                }

                self.compile_later(compiled.path);
                continue;
            }

            diagnostics.extend(self.update_project(compiled));
        }

        diagnostics
    }

    /// Compiles a project, without updating any state.
    fn compile(&self, found: &FoundProject, snapshot: &Snapshot) -> Compiled {
        #[cfg(test)]
        self.shared.compiles.fetch_add(1, Ordering::Relaxed);

        // The project's files are reported with the paths the client uses for them, including
        // its manifest, which can be a symlink
        self.shared.paths.add_root(&found.root);

        if found.path.is_symlink() {
            self.shared.paths.add_alias(&found.path);
        }

        let project = match &found.project {
            Ok(project) => project,
            Err(error) => {
                return Compiled {
                    key: found.key.clone(),
                    path: found.path.clone(),
                    diagnostics: invalid_manifest_diagnostics(&found.path, error),
                    cache: None,
                    removed: false,
                };
            }
        };

        let mut diagnostics = Diagnostics::new();
        let mut cache = None;

        let mut ctx = Compiler::new(project.options);
        let mut sources = snapshot.sources.clone();

        self.shared.inject_panic(Task::Compile);
        self.shared.wait_at_gate();

        // If the project can't be compiled (for example, because its entrypoint was deleted), only
        // the manifest's diagnostics are reported, and those of its files are cleared
        if let Ok(tree) = FileTree::compile_path(&mut ctx, &project.entrypoint, &mut sources) {
            let related_information = self.shared.related_information.load(Ordering::Relaxed);

            for item in ctx.take_diagnostics() {
                diagnostics
                    .entry(item.srcloc.source.kind.clone())
                    .or_default()
                    .push(diagnostic(&item, related_information, &self.shared.paths));
            }

            let files = tree
                .all_files()
                .into_iter()
                .filter(|file| matches!(file.source.kind, SourceKind::File(_)))
                .map(|file| file.source.clone())
                .collect::<Vec<_>>();

            for source in &files {
                diagnostics.entry(source.kind.clone()).or_default();
            }

            if let Some(source) = files.first().cloned() {
                cache = Some((Cache::new(ctx, tree, source), files));
            }
        }

        // Always include the manifest, so that its diagnostics are cleared once they're fixed
        if let Some(path) = &project.manifest_path
            && let Ok(kind) = normalize_path(path)
        {
            diagnostics.insert(kind, manifest_diagnostics(path, project));
        }

        Compiled {
            key: found.key.clone(),
            path: found.path.clone(),
            diagnostics,
            cache,
            removed: false,
        }
    }

    /// Records the result of compiling a project, and returns the diagnostics to publish. The
    /// files it published diagnostics for last time but not this time get theirs cleared, and
    /// their caches are removed, since they're not part of the project anymore (unless another
    /// project includes them).
    fn update_project(&mut self, compiled: Compiled) -> Diagnostics {
        let Compiled {
            key,
            path,
            mut diagnostics,
            cache,
            removed,
        } = compiled;

        let previous = if removed {
            self.published.remove(&key)
        } else {
            let files = diagnostics.keys().cloned().collect();
            self.published.insert(key.clone(), Tracked { path, files })
        };

        let removed = previous
            .map(|tracked| tracked.files)
            .unwrap_or_default()
            .into_iter()
            .filter(|kind| {
                !diagnostics.contains_key(kind)
                    && !self
                        .published
                        .values()
                        .any(|tracked| tracked.files.contains(kind))
            })
            .collect::<Vec<_>>();

        for kind in removed {
            self.remove_file(&kind);
            diagnostics.insert(kind, Vec::new());
        }

        // A project that couldn't be compiled can't answer queries
        match cache {
            Some((cache, files)) => self.insert_project(key, cache, files),
            None => self.remove_project(&key),
        }

        diagnostics
    }

    /// Compiles a project again, without reporting anything, after a query panicked partway
    /// through changing its compile's state.
    pub fn rebuild(&mut self, key: &PathBuf) {
        let snapshot = self.shared.snapshot();

        let compiled = self.published.get(key).and_then(|tracked| {
            let project = FoundProject::find(&tracked.path)?;
            panic::catch_unwind(AssertUnwindSafe(|| self.compile(&project, &snapshot))).ok()
        });

        match compiled.and_then(|compiled| Some((compiled.key, compiled.cache?))) {
            Some((key, (cache, files))) => self.insert_project(key, cache, files),
            None => self.remove_project(key),
        }
    }
}
