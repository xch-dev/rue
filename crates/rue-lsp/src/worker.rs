//! Compiling and answering queries happen on a dedicated thread, which owns the compiler's state
//! (it can't be sent between threads). The language server's handlers send it jobs and await
//! their results, so a long compile doesn't stop the server from reading the editor's messages,
//! and edits made in quick succession are only compiled once.
//!
//! Jobs run in the order they're sent. Edits are compiled once no other edit has arrived for
//! [`DEBOUNCE`], but anything else (opening or closing a document, a query, a file changing on
//! disk) compiles the pending edits first, so it always sees every edit sent before it. A query
//! that arrives while a compile is running waits for it.

use std::collections::HashMap;
use std::panic::{self, AssertUnwindSafe};
use std::path::PathBuf;
#[cfg(test)]
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::mpsc::{self, RecvTimeoutError};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

use indexmap::IndexMap;
use rue_compiler::Compiler;
use rue_diagnostic::{Source, SourceKind};
use tokio::sync::oneshot;
use tower_lsp::Client;
use tower_lsp::lsp_types::{Diagnostic, MessageType, Url};

use crate::cache::Cache;
use crate::paths::{Paths, document};
use crate::project::Tracked;
use crate::{Task, lock, panic_message};

/// How long to wait after an edit for more edits before compiling.
pub const DEBOUNCE: Duration = Duration::from_millis(150);

pub type Diagnostics = IndexMap<SourceKind, Vec<Diagnostic>>;

/// The state that the language server's handlers and the worker share.
#[derive(Debug)]
pub struct Shared {
    pub client: Client,
    /// The text of the documents open in the editor, which takes precedence over what's on disk.
    pub documents: Mutex<HashMap<SourceKind, Document>>,
    /// Incremented every time a document's text changes.
    revision: AtomicU64,
    /// The paths the client uses, which can differ from the compiler's.
    pub paths: Paths,
    /// Whether the client shows the related information of diagnostics. If it doesn't, notes are
    /// added to the message instead. This is assumed until the client says otherwise.
    pub related_information: AtomicBool,
    /// A task that panics on purpose, to test that the server survives panics.
    #[cfg(test)]
    pub injected_panic: Mutex<Option<Task>>,
    /// How many times a project was compiled.
    #[cfg(test)]
    pub compiles: AtomicUsize,
    /// Makes compiles wait until the test lets them continue, to test what happens meanwhile.
    #[cfg(test)]
    pub gate: Mutex<Option<Gate>>,
}

#[derive(Debug, Clone)]
pub struct Document {
    text: String,
    revision: u64,
}

/// The text of every open document, and their revisions, when a compile started.
pub struct Snapshot {
    pub sources: HashMap<SourceKind, String>,
    revisions: HashMap<SourceKind, u64>,
}

/// Lets a test hold a compile once it has started.
#[cfg(test)]
#[derive(Debug)]
pub struct Gate {
    pub started: mpsc::Sender<()>,
    pub release: mpsc::Receiver<()>,
}

impl Shared {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: Mutex::default(),
            revision: AtomicU64::new(0),
            paths: Paths::default(),
            related_information: AtomicBool::new(true),
            #[cfg(test)]
            injected_panic: Mutex::default(),
            #[cfg(test)]
            compiles: AtomicUsize::new(0),
            #[cfg(test)]
            gate: Mutex::default(),
        }
    }

    /// Sets the editor's text for a file, or discards it so that what's on disk is used.
    pub fn set_text(&self, kind: SourceKind, text: Option<String>) {
        let revision = self.revision.fetch_add(1, Ordering::Relaxed) + 1;
        let mut documents = lock(&self.documents);

        if let Some(text) = text {
            documents.insert(kind, Document { text, revision });
        } else {
            documents.remove(&kind);
        }
    }

    pub fn text(&self, kind: &SourceKind) -> Option<String> {
        lock(&self.documents)
            .get(kind)
            .map(|document| document.text.clone())
    }

    pub fn snapshot(&self) -> Snapshot {
        let documents = lock(&self.documents);

        Snapshot {
            sources: documents
                .iter()
                .map(|(kind, document)| (kind.clone(), document.text.clone()))
                .collect(),
            revisions: documents
                .iter()
                .map(|(kind, document)| (kind.clone(), document.revision))
                .collect(),
        }
    }

    /// Whether the editor changed any of these files since the snapshot was taken, which means
    /// that what was compiled from it is already out of date.
    pub fn is_stale<'a>(
        &self,
        snapshot: &Snapshot,
        files: impl IntoIterator<Item = &'a SourceKind>,
    ) -> bool {
        let documents = lock(&self.documents);

        files.into_iter().any(|kind| {
            documents.get(kind).map(|document| document.revision)
                != snapshot.revisions.get(kind).copied()
        })
    }

    #[cfg(test)]
    pub fn inject_panic(&self, task: Task) {
        let injected = *lock(&self.injected_panic);
        assert!(injected != Some(task), "injected panic");
    }

    #[cfg(not(test))]
    #[allow(clippy::unused_self)]
    pub fn inject_panic(&self, _task: Task) {}

    /// Waits until the test lets the compile continue, if it asked to hold it.
    #[cfg(test)]
    pub fn wait_at_gate(&self) {
        if let Some(gate) = lock(&self.gate).take() {
            gate.started.send(()).ok();
            gate.release.recv().ok();
        }
    }

    #[cfg(not(test))]
    #[allow(clippy::unused_self)]
    pub fn wait_at_gate(&self) {}

    async fn publish(&self, diagnostics: Diagnostics) {
        for (kind, diagnostics) in diagnostics {
            let (uris, stale) = self.paths.publish(&kind);

            for uri in stale {
                self.client.publish_diagnostics(uri, Vec::new(), None).await;
            }

            for uri in uris {
                self.client
                    .publish_diagnostics(uri, diagnostics.clone(), None)
                    .await;
            }
        }
    }

    pub async fn log_panic(&self, task: Task, message: &str) {
        self.client
            .log_message(
                MessageType::ERROR,
                format!("Internal error while {task}: {message}"),
            )
            .await;
    }
}

type Run = Box<dyn FnOnce(&mut State) + Send>;

enum Job {
    /// The editor changed a file, which is compiled once the edits stop.
    Edit(PathBuf),
    /// Compiles the projects of files that changed, and of the ones the editor changed.
    Compile {
        changed: Vec<PathBuf>,
        edited: Vec<PathBuf>,
        done: Option<oneshot::Sender<Diagnostics>>,
    },
    Run(Run),
}

/// Sends jobs to the worker thread, which stops once this is dropped.
#[derive(Debug)]
pub struct Worker {
    jobs: mpsc::Sender<Job>,
}

impl Worker {
    pub fn new(shared: Arc<Shared>) -> Self {
        let (sender, receiver) = mpsc::channel();

        thread::Builder::new()
            .name("rue-compiler".to_string())
            .spawn(move || State::new(shared).run(&receiver))
            .expect("failed to start the compiler thread");

        Self { jobs: sender }
    }

    fn send(&self, job: Job) {
        self.jobs.send(job).ok();
    }

    /// Compiles the project of a file the editor changed, once the edits stop.
    pub fn edit(&self, path: PathBuf) {
        self.send(Job::Edit(path));
    }

    /// Compiles the projects of files that changed, and publishes their diagnostics. The
    /// diagnostics are also returned, once they're published.
    pub fn compile(
        &self,
        changed: Vec<PathBuf>,
        edited: Vec<PathBuf>,
    ) -> oneshot::Receiver<Diagnostics> {
        let (done, receiver) = oneshot::channel();

        self.send(Job::Compile {
            changed,
            edited,
            done: Some(done),
        });

        receiver
    }

    /// Runs a task with the compiler's state, once the jobs before it are done. If it panics, the
    /// panic's message is returned instead.
    pub fn run<T: Send + 'static>(
        &self,
        f: impl FnOnce(&mut State) -> T + Send + 'static,
    ) -> oneshot::Receiver<Result<T, String>> {
        let (done, receiver) = oneshot::channel();

        self.send(Job::Run(Box::new(move |state| {
            let result = panic::catch_unwind(AssertUnwindSafe(|| f(state)));

            // The task may have panicked partway through changing the compiler's state, so it's
            // compiled again
            if result.is_err() {
                state.rebuild();
            }

            done.send(result.map_err(|payload| panic_message(&*payload)))
                .ok();
        })));

        receiver
    }
}

/// The state that only the worker thread has.
pub struct State {
    pub shared: Arc<Shared>,
    /// The files that each project last published diagnostics for, keyed by the project's
    /// canonical manifest (or entrypoint, if it doesn't have one).
    pub published: HashMap<PathBuf, Tracked>,
    /// The last compile of each project, which answers queries about its files.
    pub projects: HashMap<PathBuf, Cache<Compiler>>,
    /// The project whose compile answers queries about each file, and the file's source.
    pub files: HashMap<SourceKind, (PathBuf, Source)>,
    /// The files the editor changed that haven't been compiled yet.
    edited: Vec<PathBuf>,
    /// When to compile the edits, if no other edit arrives first.
    deadline: Option<Instant>,
}

impl State {
    fn new(shared: Arc<Shared>) -> Self {
        Self {
            shared,
            published: HashMap::new(),
            projects: HashMap::new(),
            files: HashMap::new(),
            edited: Vec::new(),
            deadline: None,
        }
    }

    fn run(mut self, jobs: &mpsc::Receiver<Job>) {
        loop {
            let job = match self.deadline {
                Some(deadline) => {
                    match jobs.recv_timeout(deadline.saturating_duration_since(Instant::now())) {
                        Ok(job) => job,
                        Err(RecvTimeoutError::Timeout) => {
                            self.flush();
                            continue;
                        }
                        Err(RecvTimeoutError::Disconnected) => return,
                    }
                }
                None => match jobs.recv() {
                    Ok(job) => job,
                    Err(_) => return,
                },
            };

            match job {
                Job::Edit(path) => {
                    if !self.edited.contains(&path) {
                        self.edited.push(path);
                    }

                    self.deadline = Some(Instant::now() + DEBOUNCE);
                }
                Job::Compile {
                    changed,
                    mut edited,
                    done,
                } => {
                    edited.extend(self.take_edits());

                    let diagnostics = self.compile_and_publish(&changed, &edited);

                    if let Some(done) = done {
                        done.send(diagnostics).ok();
                    }
                }
                Job::Run(run) => {
                    self.flush();
                    run(&mut self);
                }
            }
        }
    }

    fn take_edits(&mut self) -> Vec<PathBuf> {
        self.deadline = None;
        std::mem::take(&mut self.edited)
    }

    /// Compiles the edits that are waiting, if there are any.
    fn flush(&mut self) {
        let edited = self.take_edits();

        if !edited.is_empty() {
            self.compile_and_publish(&[], &edited);
        }
    }

    fn compile_and_publish(&mut self, changed: &[PathBuf], edited: &[PathBuf]) -> Diagnostics {
        match panic::catch_unwind(AssertUnwindSafe(|| self.recompile(changed, edited))) {
            Ok(diagnostics) => {
                futures::executor::block_on(self.shared.publish(diagnostics.clone()));
                diagnostics
            }
            Err(payload) => {
                let message = panic_message(&*payload);
                futures::executor::block_on(self.shared.log_panic(Task::Compile, &message));
                Diagnostics::new()
            }
        }
    }

    /// The last compile of a document's file, set up to answer queries about it.
    pub fn cache(&mut self, uri: &Url) -> Option<&mut Cache<Compiler>> {
        let (_, kind) = document(uri)?;
        let (key, source) = self.files.get(&kind)?;
        let cache = self.projects.get_mut(key)?;
        cache.set_source(source.clone());
        Some(cache)
    }

    /// Records a project's compile, so that it answers queries about its files (instead of
    /// whichever project compiled them last, if they're part of more than one).
    pub fn insert_project(&mut self, key: PathBuf, cache: Cache<Compiler>, files: Vec<Source>) {
        self.files.retain(|_, (other, _)| *other != key);

        for source in files {
            self.files
                .insert(source.kind.clone(), (key.clone(), source));
        }

        self.projects.insert(key, cache);
    }

    /// Forgets a project's compile.
    pub fn remove_project(&mut self, key: &PathBuf) {
        self.files.retain(|_, (other, _)| other != key);
        self.projects.remove(key);
    }

    /// Forgets a file, and the compile of its project once no other file uses it.
    pub fn remove_file(&mut self, kind: &SourceKind) {
        let Some((key, _)) = self.files.remove(kind) else {
            return;
        };

        if !self.files.values().any(|(other, _)| *other == key) {
            self.projects.remove(&key);
        }
    }
}
