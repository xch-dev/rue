//! Crash sweep: opens every `.rue` file under `examples/` and `tests/` and makes sure the
//! language server never panics on it, whether it's complete or half-typed.
//!
//! For each file, it (a) requests hover, go to definition, references and completion at many
//! cursor positions, and (b) recompiles edited versions of the file, as if it were being typed,
//! and makes the same requests where the edit happened. The edits are: everything after the
//! cursor removed, the rest of the cursor's line removed, and a chunk removed from the cursor on.
//!
//! [`crash_sweep_sampled`] runs with `cargo test` on a deterministic sample, so that it takes a few
//! seconds. [`crash_sweep_exhaustive`] makes requests at every character boundary and edits at
//! every token boundary, and is meant to be run in release mode after changes to the compiler or
//! the language server (it takes a long time in debug mode). Overflow checks and debug assertions
//! are turned back on, so that it catches everything a debug build would:
//!
//! ```sh
//! CARGO_PROFILE_RELEASE_DEBUG_ASSERTIONS=true CARGO_PROFILE_RELEASE_OVERFLOW_CHECKS=true \
//!     cargo test --release -p rue-lsp crash_sweep_exhaustive -- --ignored --nocapture
//! ```

use std::cell::{Cell, RefCell};
use std::collections::BTreeMap;
use std::fmt::Write;
use std::fs;
use std::mem;
use std::num::NonZeroUsize;
use std::panic::{self, AssertUnwindSafe, PanicHookInfo};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};
use std::thread;

use tower_lsp::lsp_types::{
    CompletionParams, GotoDefinitionParams, HoverParams, PartialResultParams, Position,
    ReferenceContext, ReferenceParams, TextDocumentIdentifier, TextDocumentPositionParams, Url,
    WorkDoneProgressParams,
};

use super::TestProject;

const MANIFEST: &str = "[compiler]\nentrypoint = \"main.rue\"\n";

/// How many tokens (roughly) are removed from the text by the chunk removing edit.
const CHUNK_TOKENS: usize = 6;

/// How much of each file to cover.
struct Coverage {
    /// Only every `file_stride`th file (in sorted order) is swept.
    file_stride: usize,
    /// The maximum number of cursor positions to request at in the original text, or `None` for
    /// every character boundary.
    positions: Option<usize>,
    /// The maximum number of places to edit the text at, or `None` for every token boundary.
    edits: Option<usize>,
    /// Files larger than this many bytes aren't edited, since each edit compiles the whole file
    /// again (and a few of the larger files take seconds to compile in debug mode).
    edit_size_limit: usize,
    /// Whether to print each file as it's finished (shown with `--nocapture`).
    progress: bool,
}

#[test]
fn crash_sweep_sampled() {
    run(&Coverage {
        file_stride: 2,
        positions: Some(8),
        edits: Some(1),
        edit_size_limit: 1500,
        progress: false,
    });
}

#[test]
#[ignore = "takes a long time; run in release mode, see the module documentation"]
fn crash_sweep_exhaustive() {
    run(&Coverage {
        file_stride: 1,
        positions: None,
        edits: None,
        edit_size_limit: usize::MAX,
        progress: true,
    });
}

fn run(coverage: &Coverage) {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let mut files = Vec::new();

    for dir in ["examples", "tests"] {
        collect_rue_files(&root.join(dir), &mut files);
    }

    files.sort();
    files = files.into_iter().step_by(coverage.file_stride).collect();

    // The largest files take the longest, so they're started first to keep every thread busy
    files.sort_by_key(|path| std::cmp::Reverse(fs::metadata(path).unwrap().len()));
    assert!(
        !files.is_empty(),
        "no `.rue` files found under {}",
        root.display()
    );

    let next = AtomicUsize::new(0);
    let threads = thread::available_parallelism().map_or(1, NonZeroUsize::get);

    // Each thread has its own language server and project directory, and takes the next file
    let quiet = QuietPanics::install();

    let failures = thread::scope(|scope| {
        let workers = (0..threads)
            .map(|_| {
                scope.spawn(|| {
                    let mut sweep = Sweep::new();

                    while let Some(path) = files.get(next.fetch_add(1, Ordering::Relaxed)) {
                        let name = path
                            .strip_prefix(&root)
                            .unwrap_or(path)
                            .display()
                            .to_string();
                        let text = fs::read_to_string(path).unwrap();
                        sweep.file(&name, &text, coverage);

                        if coverage.progress {
                            eprintln!("swept {name}");
                        }
                    }

                    sweep.failures
                })
            })
            .collect::<Vec<_>>();

        workers
            .into_iter()
            .flat_map(|worker| worker.join().unwrap())
            .collect::<BTreeMap<_, _>>()
    });

    drop(quiet);

    if !failures.is_empty() {
        let mut report = format!(
            "the language server panicked on {} distinct inputs:\n",
            failures.len()
        );

        for ((file, request, message), failure) in &failures {
            writeln!(
                report,
                "\n{file} ({}): {request} at {}:{} panicked {} times, first with:\n    {message}",
                failure.edit, failure.position.line, failure.position.character, failure.count
            )
            .unwrap();
        }

        panic!("{report}");
    }
}

fn collect_rue_files(dir: &Path, files: &mut Vec<PathBuf>) {
    for entry in fs::read_dir(dir).unwrap() {
        let path = entry.unwrap().path();

        if path.is_dir() {
            collect_rue_files(&path, files);
        } else if path.extension().is_some_and(|extension| extension == "rue") {
            files.push(path);
        }
    }
}

/// The first input a panic happened on, and how many inputs it happened on in that file.
struct Failure {
    /// How the file was edited, e.g. `rest of line removed at 3:4`.
    edit: String,
    position: Position,
    count: usize,
}

#[derive(Clone, Copy)]
enum Request {
    Hover,
    GotoDefinition,
    References,
    Completion,
}

impl Request {
    const ALL: [Self; 4] = [
        Self::Hover,
        Self::GotoDefinition,
        Self::References,
        Self::Completion,
    ];

    fn name(self) -> &'static str {
        match self {
            Self::Hover => "hover",
            Self::GotoDefinition => "goto definition",
            Self::References => "references",
            Self::Completion => "completion",
        }
    }

    fn send(self, project: &TestProject, document: TextDocumentPositionParams) {
        let backend = project.backend();

        match self {
            Self::Hover => {
                backend.on_hover(&HoverParams {
                    text_document_position_params: document,
                    work_done_progress_params: WorkDoneProgressParams::default(),
                });
            }
            Self::GotoDefinition => {
                backend.on_goto_definition(&GotoDefinitionParams {
                    text_document_position_params: document,
                    work_done_progress_params: WorkDoneProgressParams::default(),
                    partial_result_params: PartialResultParams::default(),
                });
            }
            Self::References => {
                backend.on_references(&ReferenceParams {
                    text_document_position: document,
                    work_done_progress_params: WorkDoneProgressParams::default(),
                    partial_result_params: PartialResultParams::default(),
                    context: ReferenceContext {
                        include_declaration: true,
                    },
                });
            }
            Self::Completion => {
                backend.on_completion(&CompletionParams {
                    text_document_position: document,
                    work_done_progress_params: WorkDoneProgressParams::default(),
                    partial_result_params: PartialResultParams::default(),
                    context: None,
                });
            }
        }
    }
}

struct Sweep {
    project: TestProject,
    uri: Url,
    /// Keyed by file, request and panic message, so that one bug doesn't flood the report.
    failures: BTreeMap<(String, &'static str, String), Failure>,
}

impl Sweep {
    fn new() -> Self {
        let project = TestProject::with_manifest(Some(MANIFEST), &[("main.rue", "")]);
        let uri = project.uri("main.rue");

        Self {
            project,
            uri,
            failures: BTreeMap::new(),
        }
    }

    fn file(&mut self, name: &str, text: &str, coverage: &Coverage) {
        let boundaries = char_boundaries(text);

        if self.compile(name, "original", text, Position::default()) {
            for &(_, position) in &sample(&boundaries, coverage.positions) {
                self.requests(name, "original", text, position);
            }
        }

        if text.len() > coverage.edit_size_limit {
            return;
        }

        let tokens = boundaries
            .iter()
            .copied()
            .filter(|&(offset, _)| is_token_boundary(text, offset))
            .collect::<Vec<_>>();
        let cuts = sample(
            &tokens.iter().enumerate().collect::<Vec<_>>(),
            coverage.edits,
        );

        for (i, &(offset, position)) in cuts {
            let line_end = text[offset..].find('\n').map_or(text.len(), |i| offset + i);
            let chunk_end = tokens
                .get(i + CHUNK_TOKENS)
                .map_or(text.len(), |&(offset, _)| offset);

            let at = format!("{}:{}", position.line, position.character);
            let mut compiled = Vec::new();

            for (edit, edited) in [
                (format!("truncated at {at}"), text[..offset].to_string()),
                (
                    format!("rest of line removed at {at}"),
                    format!("{}{}", &text[..offset], &text[line_end..]),
                ),
                (
                    format!("{} bytes removed at {at}", chunk_end - offset),
                    format!("{}{}", &text[..offset], &text[chunk_end..]),
                ),
            ] {
                // Near the end of a line or of the file, some edits give the same text
                if edited == text || compiled.contains(&edited) {
                    continue;
                }

                if self.compile(name, &edit, &edited, position) {
                    self.requests(name, &edit, &edited, position);
                }

                compiled.push(edited);
            }
        }
    }

    /// Recompiles the project with the given text, and returns whether it succeeded.
    fn compile(&mut self, name: &str, edit: &str, text: &str, position: Position) -> bool {
        let backend = self.project.backend();
        let result = catch(|| backend.on_change_impl(&self.uri, Some(text.to_string())));

        match result {
            Ok(diagnostics) => {
                // Nothing is compiled if the project isn't found, which would make the sweep pass
                // without testing anything
                assert!(!diagnostics.is_empty(), "the project wasn't compiled");
                true
            }
            Err(message) => {
                self.fail(name, edit, "compile", message, position);
                false
            }
        }
    }

    fn requests(&mut self, name: &str, edit: &str, text: &str, position: Position) {
        for request in Request::ALL {
            let document = TextDocumentPositionParams::new(
                TextDocumentIdentifier::new(self.uri.clone()),
                position,
            );
            let project = &self.project;

            if let Err(message) = catch(|| request.send(project, document)) {
                self.fail(name, edit, request.name(), message, position);

                // The server was started again, so the text needs to be compiled again
                if !self.compile(name, edit, text, position) {
                    return;
                }
            }
        }
    }

    fn fail(
        &mut self,
        name: &str,
        edit: &str,
        request: &'static str,
        message: String,
        position: Position,
    ) {
        // A panic while a lock is held poisons it, so the server is started again
        *self = Self {
            failures: std::mem::take(&mut self.failures),
            ..Self::new()
        };

        self.failures
            .entry((name.to_string(), request, message))
            .or_insert_with(|| Failure {
                edit: edit.to_string(),
                position,
                count: 0,
            })
            .count += 1;
    }
}

/// Every character boundary in the text (including the end), with its LSP position.
fn char_boundaries(text: &str) -> Vec<(usize, Position)> {
    let mut boundaries = Vec::with_capacity(text.len() + 1);
    let mut position = Position::default();

    for (offset, char) in text.char_indices() {
        boundaries.push((offset, position));

        if char == '\n' {
            position.line += 1;
            position.character = 0;
        } else {
            position.character += char.len_utf16() as u32;
        }
    }

    boundaries.push((text.len(), position));
    boundaries
}

/// Whether an edit at this offset would split the text between two tokens, roughly: where a word
/// starts or ends, or next to punctuation.
fn is_token_boundary(text: &str, offset: usize) -> bool {
    let (Some(before), Some(after)) = (
        text[..offset].chars().next_back(),
        text[offset..].chars().next(),
    ) else {
        return true;
    };

    let is_word = |char: char| char.is_alphanumeric() || char == '_';

    !(is_word(before) && is_word(after) || before.is_whitespace() && after.is_whitespace())
}

/// Picks up to `limit` evenly spaced items (away from the ends, where edits are trivial), or all
/// of them.
fn sample<T: Copy>(items: &[T], limit: Option<usize>) -> Vec<T> {
    match limit {
        Some(limit) if items.len() > limit => (1..=limit)
            .map(|i| items[i * items.len() / (limit + 1)])
            .collect(),
        _ => items.to_vec(),
    }
}

type PanicHook = Box<dyn Fn(&PanicHookInfo<'_>) + Sync + Send + 'static>;

thread_local! {
    /// Whether a panic on this thread is about to be caught by [`catch`].
    static QUIET: Cell<bool> = const { Cell::new(false) };
    /// The message and location of the last panic caught on this thread.
    static LAST_PANIC: RefCell<Option<String>> = const { RefCell::new(None) };
}

/// Only one sweep swaps the panic hook at a time, so that each restores the right one.
static HOOK_LOCK: Mutex<()> = Mutex::new(());

/// Records panics caught by [`catch`] instead of printing them, while other panics (such as those
/// of other tests running in parallel) are printed as usual. The previous hook is restored when
/// this is dropped.
struct QuietPanics {
    previous: Arc<PanicHook>,
    _lock: std::sync::MutexGuard<'static, ()>,
}

impl QuietPanics {
    fn install() -> Self {
        let lock = HOOK_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let previous = Arc::new(panic::take_hook());
        let fallback = previous.clone();

        panic::set_hook(Box::new(move |info| {
            if QUIET.get() {
                let message = info
                    .payload()
                    .downcast_ref::<&str>()
                    .map(ToString::to_string)
                    .or_else(|| info.payload().downcast_ref::<String>().cloned())
                    .unwrap_or_else(|| "<non-string panic payload>".to_string());
                let location = info
                    .location()
                    .map_or_else(String::new, |location| format!(" at {location}"));

                LAST_PANIC.set(Some(format!("{message}{location}")));
            } else {
                fallback(info);
            }
        }));

        Self {
            previous,
            _lock: lock,
        }
    }
}

impl Drop for QuietPanics {
    fn drop(&mut self) {
        // Dropping the sweep's hook releases its reference to the previous one
        drop(panic::take_hook());

        let previous = mem::replace(&mut self.previous, Arc::new(Box::new(|_| {})));
        let previous = Arc::try_unwrap(previous)
            .unwrap_or_else(|_| unreachable!("the sweep's hook was the only other reference"));

        panic::set_hook(previous);
    }
}

/// Runs the function, and returns the panic message and location if it panics.
fn catch<T>(f: impl FnOnce() -> T) -> Result<T, String> {
    QUIET.set(true);
    let result = panic::catch_unwind(AssertUnwindSafe(f));
    QUIET.set(false);

    result.map_err(|_| {
        LAST_PANIC
            .take()
            .unwrap_or_else(|| "<unknown panic>".to_string())
    })
}

#[test]
fn crash_sweep_records_panics() {
    let quiet = QuietPanics::install();

    let message = catch(|| panic!("boom {}", 1)).unwrap_err();
    let result = catch(|| 1);

    drop(quiet);

    assert_eq!(result, Ok(1));

    assert!(message.starts_with("boom 1 at "), "{message}");
    assert!(message.contains("sweep.rs"), "{message}");
}
