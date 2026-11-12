#![allow(clippy::cast_possible_truncation)]

mod cache;
mod paths;
mod position;

use cache::Cache;
use indexmap::IndexMap;
use rue_compiler::{Compiler, FileTree, normalize_path};
use rue_diagnostic::{Applicability, Lint, SourceKind, SrcLoc};
use rue_formatter::{FormatError, FormatOptions, format_source};
use serde::{Deserialize, Serialize};

use std::collections::{HashMap, HashSet};
use std::fmt::{self, Write};
use std::fs;
use std::panic::{self, AssertUnwindSafe};
use std::path::{Path, PathBuf};
#[cfg(test)]
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, MutexGuard, PoisonError};

use rue_options::{Project, find_project};
use send_wrapper::SendWrapper;
use tower_lsp::jsonrpc::{Error, Result};
use tower_lsp::lsp_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, CodeActionParams,
    CodeActionProviderCapability, CodeActionResponse, CompletionOptions, CompletionParams,
    CompletionResponse, Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity,
    DidChangeTextDocumentParams, DidChangeWatchedFilesParams,
    DidChangeWatchedFilesRegistrationOptions, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentFormattingParams, FileEvent,
    FileSystemWatcher, GlobPattern, GotoDefinitionParams, GotoDefinitionResponse, Hover,
    HoverContents, HoverParams, HoverProviderCapability, InitializeParams, InitializeResult,
    InitializedParams, LanguageString, Location, MarkedString, MessageType, NumberOrString, OneOf,
    Position, Range, ReferenceParams, Registration, ServerCapabilities, TextDocumentSyncCapability,
    TextDocumentSyncKind, TextEdit, Url, WorkspaceEdit,
};
use tower_lsp::{Client, LanguageServer};

use crate::cache::{HoverInfo, SymbolHoverKind};
use crate::paths::{Paths, document, uri_to_path};
use crate::position::offsets_to_range;

/// Logged once the client says it's initialized.
const INITIALIZED: &str = "Rue language server initialized.";

#[derive(Debug)]
pub struct Backend {
    client: Client,
    cache: Mutex<HashMap<SourceKind, SendWrapper<Cache<Arc<Compiler>>>>>,
    file_cache: Mutex<HashMap<SourceKind, String>>,
    /// The files that each project last published diagnostics for, keyed by its manifest (or its
    /// entrypoint, if it doesn't have one), so that they can be cleared once a file isn't part of
    /// the project anymore.
    published: Mutex<HashMap<PathBuf, HashSet<SourceKind>>>,
    /// The paths the client uses, which can differ from the compiler's.
    paths: Paths,
    /// Whether the client can register file watchers when asked to.
    watch_files: AtomicBool,
    /// Whether the client shows the related information of diagnostics. If it doesn't, notes are
    /// added to the message instead. This is assumed until the client says otherwise.
    related_information: AtomicBool,
    /// A task that panics on purpose, to test that the server survives panics.
    #[cfg(test)]
    injected_panic: Mutex<Option<Task>>,
    /// How many times a project was compiled.
    #[cfg(test)]
    compiles: AtomicUsize,
}

/// The work the server does in response to the editor, used to say what failed when it panics.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Task {
    Compile,
    Hover,
    Definition,
    References,
    Completion,
    Formatting,
    CodeAction,
}

impl fmt::Display for Task {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Compile => "compiling",
            Self::Hover => "computing hover information",
            Self::Definition => "finding the definition",
            Self::References => "finding references",
            Self::Completion => "computing completions",
            Self::Formatting => "formatting",
            Self::CodeAction => "computing code actions",
        })
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        let related_information = params
            .capabilities
            .text_document
            .as_ref()
            .and_then(|capabilities| capabilities.publish_diagnostics.as_ref())
            .and_then(|capabilities| capabilities.related_information)
            .unwrap_or(false);

        self.related_information
            .store(related_information, Ordering::Relaxed);

        let watch_files = params
            .capabilities
            .workspace
            .and_then(|capabilities| capabilities.did_change_watched_files)
            .and_then(|capabilities| capabilities.dynamic_registration)
            .unwrap_or(false);

        self.watch_files.store(watch_files, Ordering::Relaxed);

        // Files in the workspace are reported with the paths the client uses for them
        #[allow(deprecated)]
        let root_uri = params.root_uri.iter();

        for uri in params
            .workspace_folders
            .iter()
            .flatten()
            .map(|folder| &folder.uri)
            .chain(root_uri)
        {
            if let Some(path) = uri_to_path(uri) {
                self.paths.add_root(&path);
            }
        }

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![
                        ".".to_string(),
                        ",".to_string(),
                        ":".to_string(),
                    ]),
                    all_commit_characters: Some(vec![
                        ";".to_string(),
                        "(".to_string(),
                        ")".to_string(),
                        "{".to_string(),
                        "}".to_string(),
                        "[".to_string(),
                        "]".to_string(),
                    ]),
                    ..Default::default()
                }),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, INITIALIZED)
            .await;

        // Changes made outside of the editor (such as switching branches) are only seen if the
        // client watches the files
        if self.watch_files.load(Ordering::Relaxed) {
            let watchers = ["**/*.rue", "**/Rue.toml"]
                .into_iter()
                .map(|pattern| FileSystemWatcher {
                    glob_pattern: GlobPattern::String(pattern.to_string()),
                    kind: None,
                })
                .collect();

            let registration = Registration {
                id: "rue-watched-files".to_string(),
                method: "workspace/didChangeWatchedFiles".to_string(),
                register_options: Some(
                    serde_json::to_value(DidChangeWatchedFilesRegistrationOptions { watchers })
                        .expect("registration options are serializable"),
                ),
            };

            if let Err(error) = self.client.register_capability(vec![registration]).await {
                self.client
                    .log_message(
                        MessageType::WARNING,
                        format!("Couldn't watch files for changes: {error}"),
                    )
                    .await;
            }
        }
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.paths.open(&params.text_document.uri);

        self.on_change(params.text_document.uri, Some(params.text_document.text))
            .await;
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        // The server only asks for full syncs, so the last change has the whole text
        let Some(change) = params.content_changes.pop() else {
            return;
        };

        self.on_change(params.text_document.uri, Some(change.text))
            .await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        self.on_change(params.text_document.uri, None).await;
    }

    /// The file's unsaved changes are discarded, so the project is compiled with what's on disk.
    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.paths.close(&params.text_document.uri);

        self.on_change(params.text_document.uri, None).await;
    }

    async fn did_change_watched_files(&self, params: DidChangeWatchedFilesParams) {
        if let Some(diagnostics) = self
            .catch_panic(Task::Compile, || {
                self.on_watched_files_impl(&params.changes)
            })
            .await
        {
            self.publish(diagnostics).await;
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        Ok(self
            .catch_panic(Task::Hover, || self.on_hover(&params))
            .await
            .flatten())
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        Ok(self
            .catch_panic(Task::Definition, || self.on_goto_definition(&params))
            .await
            .flatten())
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        Ok(self
            .catch_panic(Task::References, || self.on_references(&params))
            .await
            .flatten())
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        Ok(self
            .catch_panic(Task::Completion, || self.on_completion(&params))
            .await
            .flatten())
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        self.catch_panic(Task::Formatting, || self.on_formatting(&params))
            .await
            .transpose()
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        Ok(self
            .catch_panic(Task::CodeAction, || {
                self.inject_panic(Task::CodeAction);
                code_actions(&params)
            })
            .await
            .flatten())
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            cache: Mutex::new(HashMap::new()),
            file_cache: Mutex::new(HashMap::new()),
            published: Mutex::new(HashMap::new()),
            paths: Paths::default(),
            watch_files: AtomicBool::new(false),
            related_information: AtomicBool::new(true),
            #[cfg(test)]
            injected_panic: Mutex::default(),
            #[cfg(test)]
            compiles: AtomicUsize::new(0),
        }
    }

    /// Runs a task, and if it panics, logs the panic to the client and returns `None` instead of
    /// taking down the server.
    async fn catch_panic<T>(&self, task: Task, f: impl FnOnce() -> T) -> Option<T> {
        let payload = match panic::catch_unwind(AssertUnwindSafe(f)) {
            Ok(value) => return Some(value),
            Err(payload) => payload,
        };

        let message = payload
            .downcast_ref::<&str>()
            .copied()
            .or_else(|| payload.downcast_ref::<String>().map(String::as_str))
            .unwrap_or("unknown panic");

        let message = format!("Internal error while {task}: {message}");

        self.client.log_message(MessageType::ERROR, message).await;

        None
    }

    #[cfg(test)]
    fn inject_panic(&self, task: Task) {
        let injected = *lock(&self.injected_panic);
        assert!(injected != Some(task), "injected panic");
    }

    #[cfg(not(test))]
    #[allow(clippy::unused_self)]
    fn inject_panic(&self, _task: Task) {}

    async fn on_change(&self, uri: Url, text: Option<String>) {
        if let Some(diagnostics) = self
            .catch_panic(Task::Compile, || self.on_change_impl(&uri, text))
            .await
        {
            self.publish(diagnostics).await;
        }
    }

    async fn publish(&self, diagnostics: IndexMap<SourceKind, Vec<Diagnostic>>) {
        for (kind, diagnostics) in diagnostics {
            for uri in self.paths.uris(&kind) {
                self.client
                    .publish_diagnostics(uri, diagnostics.clone(), None)
                    .await;
            }
        }
    }

    fn on_change_impl(
        &self,
        uri: &Url,
        text: Option<String>,
    ) -> IndexMap<SourceKind, Vec<Diagnostic>> {
        // The file may have been deleted, for example before the editor closes it
        let Some((uri_path, source_kind)) = document(uri) else {
            return IndexMap::new();
        };

        // Keep track of the editor's text even if the project can't be compiled right now, so
        // that it's used once it can be
        {
            let mut file_cache = lock(&self.file_cache);

            if let Some(text) = text {
                file_cache.insert(source_kind, text);
            } else {
                file_cache.remove(&source_kind);
            }
        }

        self.recompile(&[], Some(&uri_path))
    }

    /// Recompiles the projects that files changed on disk belong to, outside of the editor (for
    /// example, by switching branches).
    fn on_watched_files_impl(
        &self,
        changes: &[FileEvent],
    ) -> IndexMap<SourceKind, Vec<Diagnostic>> {
        let changed = changes
            .iter()
            .filter_map(|change| uri_to_path(&change.uri))
            .collect::<Vec<_>>();

        self.recompile(&changed, None)
    }

    /// Recompiles the projects that the changed files belong to, if diagnostics were reported for
    /// them, and the project of the file the editor changed, if any. Projects that aren't the
    /// editor's are only compiled once the user opened one of their files, so that the server
    /// doesn't report diagnostics for the whole workspace.
    fn recompile(
        &self,
        changed: &[PathBuf],
        edited: Option<&Path>,
    ) -> IndexMap<SourceKind, Vec<Diagnostic>> {
        let published = lock(&self.published).clone();

        let mut lookups = edited
            .map(Path::to_path_buf)
            .into_iter()
            .collect::<Vec<_>>();
        let mut removed = Vec::new();

        // A project may not exist anymore, or may have become part of another one (for example,
        // if a manifest was deleted, or created in a parent directory). Its files are cleared, and
        // those open in the editor are compiled with whichever project they're part of now.
        for (key, files) in &published {
            if FoundProject::find(key).is_some_and(|project| &project.key == key) {
                continue;
            }

            removed.push(key.clone());

            lookups.extend(files.iter().filter_map(|kind| self.paths.open_path(kind)));
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
                && published.contains_key(&project.key)
            {
                projects.entry(project.key.clone()).or_insert(project);
            }
        }

        let mut compiled = removed
            .into_iter()
            .map(Compiled::removed)
            .chain(projects.values().map(|project| self.compile(project)))
            .collect::<Vec<_>>();

        // A file that a project stops including keeps the diagnostics and cache of whichever
        // project was compiled last, so the other projects that include it are compiled too, and
        // recorded last, to replace them
        let dropped = compiled
            .iter()
            .flat_map(|compiled| {
                published
                    .get(&compiled.key)
                    .into_iter()
                    .flatten()
                    .filter(|kind| !compiled.diagnostics.contains_key(*kind))
            })
            .collect::<HashSet<_>>();

        for (key, files) in &published {
            if !compiled.iter().any(|compiled| &compiled.key == key)
                && files.iter().any(|kind| dropped.contains(kind))
                && let Some(project) = FoundProject::find(key)
            {
                compiled.push(self.compile(&project));
            }
        }

        // Everything is compiled before any state is updated, so that a panic leaves the results
        // of the last compile in place
        let mut diagnostics = IndexMap::new();

        for compiled in compiled {
            diagnostics.extend(self.update_project(compiled));
        }

        diagnostics
    }

    /// Compiles a project, without updating any state.
    fn compile(&self, found: &FoundProject) -> Compiled {
        #[cfg(test)]
        self.compiles.fetch_add(1, Ordering::Relaxed);

        // The project's files are reported with the paths the client uses for them
        let root = if found.key.is_dir() {
            &found.key
        } else {
            found.key.parent().unwrap_or(&found.key)
        };

        self.paths.add_root(root);

        let project = match &found.project {
            Ok(project) => project,
            Err(error) => {
                return Compiled {
                    key: found.key.clone(),
                    diagnostics: invalid_manifest_diagnostics(&found.key, error),
                    caches: Vec::new(),
                    removed: false,
                };
            }
        };

        let mut diagnostics = IndexMap::<SourceKind, Vec<Diagnostic>>::new();
        let mut caches = Vec::new();

        let mut ctx = Compiler::new(project.options);

        // The compiler gets its own copy of the editor's text, so that the lock isn't held while
        // compiling, which could panic
        let mut sources = lock(&self.file_cache).clone();

        self.inject_panic(Task::Compile);

        // If the project can't be compiled (for example, because its entrypoint was deleted), only
        // the manifest's diagnostics are reported, and those of its files are cleared
        if let Ok(tree) = FileTree::compile_path(&mut ctx, &project.entrypoint, &mut sources) {
            for item in ctx.take_diagnostics() {
                diagnostics
                    .entry(item.srcloc.source.kind.clone())
                    .or_default()
                    .push(diagnostic(
                        &item,
                        self.related_information.load(Ordering::Relaxed),
                        &self.paths,
                    ));
            }

            let ctx = Arc::new(ctx);
            let tree = Arc::new(SendWrapper::new(tree));

            for file in tree.all_files() {
                if !matches!(file.source.kind, SourceKind::File(_)) {
                    continue;
                }

                diagnostics.entry(file.source.kind.clone()).or_default();

                caches.push((
                    file.source.kind.clone(),
                    SendWrapper::new(Cache::new(ctx.clone(), tree.clone(), file.source.clone())),
                ));
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
            diagnostics,
            caches,
            removed: false,
        }
    }

    /// Records the result of compiling a project, and returns the diagnostics to publish. The
    /// files it published diagnostics for last time but not this time get theirs cleared, and
    /// their caches are removed, since they're not part of the project anymore (unless another
    /// project includes them).
    fn update_project(&self, compiled: Compiled) -> IndexMap<SourceKind, Vec<Diagnostic>> {
        let Compiled {
            key,
            mut diagnostics,
            caches,
            removed,
        } = compiled;

        let removed = {
            let mut published = lock(&self.published);

            let previous = if removed {
                published.remove(&key)
            } else {
                published.insert(key, diagnostics.keys().cloned().collect())
            };

            previous
                .unwrap_or_default()
                .into_iter()
                .filter(|kind| {
                    !diagnostics.contains_key(kind)
                        && !published.values().any(|files| files.contains(kind))
                })
                .collect::<Vec<_>>()
        };

        let mut cache = lock(&self.cache);

        for kind in removed {
            cache.remove(&kind);
            diagnostics.insert(kind, Vec::new());
        }

        cache.extend(caches);

        diagnostics
    }

    fn on_formatting(&self, params: &DocumentFormattingParams) -> Result<Vec<TextEdit>> {
        self.inject_panic(Task::Formatting);

        let (path, source_kind) = document(&params.text_document.uri)
            .ok_or_else(|| Error::invalid_params("document URI is not a file"))?;
        let project =
            find_project(&path, false).map_err(|error| Error::invalid_params(error.to_string()))?;
        let options = project.map_or_else(FormatOptions::default, |project| project.format_options);
        let source = lock(&self.file_cache).get(&source_kind).cloned();
        let source = match source {
            Some(source) => source,
            None => fs::read_to_string(&path)
                .map_err(|error| Error::invalid_params(error.to_string()))?,
        };

        formatting_edits(&source, &options)
            .map_err(|error| Error::invalid_params(error.to_string()))
    }

    fn on_hover(&self, params: &HoverParams) -> Option<Hover> {
        self.inject_panic(Task::Hover);

        let (_, kind) = document(&params.text_document_position_params.text_document.uri)?;
        let mut cache = lock(&self.cache).get(&kind)?.to_cloned();
        let position = cache.position(params.text_document_position_params.position);

        let scopes = cache.scopes(position);
        let infos = cache.hover(&scopes, position);

        if infos.is_empty() {
            return None;
        }

        let content = infos
            .into_iter()
            .map(|info| {
                MarkedString::LanguageString(LanguageString {
                    language: "rue".to_string(),
                    value: match &info {
                        HoverInfo::Symbol(info) => match &info.kind {
                            SymbolHoverKind::Binding { type_name } => {
                                format!("let {}: {type_name}", info.name)
                            }
                            SymbolHoverKind::Parameter { type_name } => {
                                format!("{}: {type_name}", info.name)
                            }
                            SymbolHoverKind::Constant { type_name, inline } => format!(
                                "{}const {}: {type_name}",
                                if *inline { "inline " } else { "" },
                                info.name
                            ),
                            SymbolHoverKind::Function { signatures, inline } => signatures
                                .iter()
                                .map(|signature| {
                                    format!(
                                        "{}fn {}{signature}",
                                        if *inline { "inline " } else { "" },
                                        info.name
                                    )
                                })
                                .collect::<Vec<_>>()
                                .join("\n"),
                        },
                        HoverInfo::Module(info) => format!("mod {}", info.name),
                        HoverInfo::Type(info) => {
                            if let Some(inner) = &info.inner_name {
                                format!("type {} = {}", info.name, inner)
                            } else {
                                format!("type {}", info.name)
                            }
                        }
                        HoverInfo::Struct(info) => format!(
                            "struct {} {{\n{}\n}}",
                            info.name,
                            info.fields
                                .iter()
                                .enumerate()
                                .map(|(i, field)| {
                                    let spread =
                                        if !info.nil_terminated && i == info.fields.len() - 1 {
                                            "..."
                                        } else {
                                            ""
                                        };
                                    format!("    {spread}{}: {},", field.name, field.type_name)
                                })
                                .collect::<Vec<String>>()
                                .join("\n")
                        ),
                        HoverInfo::Field(info) => format!("{}: {}", info.name, info.type_name),
                    },
                })
            })
            .collect();

        Some(Hover {
            contents: HoverContents::Array(content),
            range: None,
        })
    }

    fn on_goto_definition(&self, params: &GotoDefinitionParams) -> Option<GotoDefinitionResponse> {
        self.inject_panic(Task::Definition);

        let (_, kind) = document(&params.text_document_position_params.text_document.uri)?;
        let cache = lock(&self.cache).get(&kind)?.to_cloned();
        let position = cache.position(params.text_document_position_params.position);

        let locations = cache
            .definitions(position)
            .into_iter()
            .map(|location| self.paths.location(location))
            .collect::<Vec<_>>();

        if locations.is_empty() {
            None
        } else if locations.len() == 1 {
            Some(GotoDefinitionResponse::Scalar(locations[0].clone()))
        } else {
            Some(GotoDefinitionResponse::Array(locations))
        }
    }

    fn on_references(&self, params: &ReferenceParams) -> Option<Vec<Location>> {
        self.inject_panic(Task::References);

        let (_, kind) = document(&params.text_document_position.text_document.uri)?;
        let cache = lock(&self.cache).get(&kind)?.to_cloned();
        let position = cache.position(params.text_document_position.position);

        let locations = cache
            .references(position)
            .into_iter()
            .map(|location| self.paths.location(location))
            .collect::<Vec<_>>();

        if locations.is_empty() {
            None
        } else {
            Some(locations)
        }
    }

    fn on_completion(&self, params: &CompletionParams) -> Option<CompletionResponse> {
        self.inject_panic(Task::Completion);

        let (_, kind) = document(&params.text_document_position.text_document.uri)?;
        let mut cache = lock(&self.cache).get(&kind)?.to_cloned();
        let position = cache.position(params.text_document_position.position);

        let scopes = cache.scopes(position);
        let completions = cache.completions(&scopes, position);

        if completions.is_empty() {
            return None;
        }

        Some(CompletionResponse::Array(completions))
    }
}

/// Locks a mutex, even if a panic poisoned it. Panics are caught so that the server keeps running,
/// and the state behind each lock is only changed once nothing else can fail, so it's still valid.
fn lock<T>(mutex: &Mutex<T>) -> MutexGuard<'_, T> {
    mutex.lock().unwrap_or_else(PoisonError::into_inner)
}

fn formatting_edits(
    source: &str,
    options: &FormatOptions,
) -> std::result::Result<Vec<TextEdit>, FormatError> {
    let formatted = format_source(source, options)?;
    if formatted == source {
        return Ok(Vec::new());
    }

    Ok(vec![TextEdit::new(
        Range::new(Position::new(0, 0), document_end(source)),
        formatted,
    )])
}

fn document_end(source: &str) -> Position {
    let line = source.bytes().filter(|byte| *byte == b'\n').count();
    let last_line = source
        .rsplit_once('\n')
        .map_or(source, |(_, last_line)| last_line);
    let last_line = last_line.strip_suffix('\r').unwrap_or(last_line);
    let character = last_line.encode_utf16().count();

    Position::new(line.try_into().unwrap(), character.try_into().unwrap())
}

/// A suggestion, which is stored in the `data` of a diagnostic so that a code action can be
/// created for it when the editor asks, without the server having to keep the diagnostics.
#[derive(Debug, Serialize, Deserialize)]
struct SuggestionData {
    title: String,
    preferred: bool,
    edit: WorkspaceEdit,
}

fn location(srcloc: &SrcLoc, paths: &Paths) -> Option<Location> {
    Some(Location::new(
        paths.uri(&srcloc.source.kind)?,
        offsets_to_range(&srcloc.source.text, srcloc.span.start, srcloc.span.end),
    ))
}

fn suggestion_data(
    suggestion: &rue_diagnostic::Suggestion,
    paths: &Paths,
) -> Option<SuggestionData> {
    let mut changes = HashMap::<Url, Vec<TextEdit>>::new();

    for edit in &suggestion.edits {
        let location = location(&edit.srcloc, paths)?;

        changes
            .entry(location.uri)
            .or_default()
            .push(TextEdit::new(location.range, edit.replacement.clone()));
    }

    Some(SuggestionData {
        title: suggestion.message.clone(),
        preferred: suggestion.applicability == Applicability::MachineApplicable,
        edit: WorkspaceEdit::new(changes),
    })
}

/// The quick fixes for the suggestions of the diagnostics the editor sent.
fn code_actions(params: &CodeActionParams) -> Option<CodeActionResponse> {
    let mut actions = Vec::new();

    for diagnostic in &params.context.diagnostics {
        let Some(data) = &diagnostic.data else {
            continue;
        };

        let Ok(suggestions) = serde_json::from_value::<Vec<SuggestionData>>(data.clone()) else {
            continue;
        };

        for suggestion in suggestions {
            actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                title: suggestion.title,
                kind: Some(CodeActionKind::QUICKFIX),
                diagnostics: Some(vec![diagnostic.clone()]),
                edit: Some(suggestion.edit),
                is_preferred: Some(suggestion.preferred),
                ..CodeAction::default()
            }));
        }
    }

    (!actions.is_empty()).then_some(actions)
}

fn diagnostic(
    diagnostic: &rue_diagnostic::Diagnostic,
    supports_related: bool,
    paths: &Paths,
) -> Diagnostic {
    let srcloc = &diagnostic.srcloc;

    let mut message = diagnostic.kind.to_string();
    let mut related_information = Vec::new();

    // Notes that point to a file are shown at that location if the client supports it, and the
    // rest are part of the message
    for note in &diagnostic.notes {
        if supports_related
            && let Some(location) = note
                .srcloc
                .as_ref()
                .and_then(|srcloc| location(srcloc, paths))
        {
            related_information.push(DiagnosticRelatedInformation {
                location,
                message: note.message.clone(),
            });
        } else {
            message.push('\n');
            message.push_str(&note.message);

            // The location is shown like it is on the command line, relative to the file that has
            // the diagnostic
            if let Some(srcloc) = &note.srcloc {
                let relative_to = match &diagnostic.srcloc.source.kind {
                    SourceKind::File(path) => Path::new(path).parent().unwrap_or(Path::new("")),
                    SourceKind::Std(_) => Path::new(""),
                };

                write!(
                    message,
                    " at {}:{}",
                    srcloc.source.kind.display(relative_to),
                    srcloc.start()
                )
                .unwrap();
            }
        }
    }

    let suggestions = diagnostic
        .suggestions
        .iter()
        .filter_map(|suggestion| suggestion_data(suggestion, paths))
        .collect::<Vec<_>>();

    Diagnostic {
        range: offsets_to_range(&srcloc.source.text, srcloc.span.start, srcloc.span.end),
        message,
        related_information: (!related_information.is_empty()).then_some(related_information),
        data: (!suggestions.is_empty())
            .then(|| serde_json::to_value(suggestions).expect("suggestions are serializable")),
        severity: Some(match diagnostic.severity {
            rue_diagnostic::DiagnosticSeverity::Error => DiagnosticSeverity::ERROR,
            rue_diagnostic::DiagnosticSeverity::Warning => DiagnosticSeverity::WARNING,
        }),
        code: diagnostic
            .lint()
            .map(|lint| NumberOrString::String(lint.name().to_string())),
        ..Diagnostic::default()
    }
}

/// Reports problems with the `[lints]` section of `Rue.toml`, such as names that don't refer to a
/// known lint or group.
fn manifest_diagnostics(path: &Path, project: &Project) -> Vec<Diagnostic> {
    if project.unknown_lints.is_empty() && project.forbidden_lints.is_empty() {
        return Vec::new();
    }

    let text = fs::read_to_string(path).unwrap_or_default();

    // Unknown names are reported by the `unknown_lint` lint, at the level set by `Rue.toml`
    let severity = project
        .options
        .lints
        .get(Lint::UnknownLint)
        .severity()
        .map(|severity| match severity {
            rue_diagnostic::DiagnosticSeverity::Error => DiagnosticSeverity::ERROR,
            rue_diagnostic::DiagnosticSeverity::Warning => DiagnosticSeverity::WARNING,
        });

    let unknown = project
        .unknown_lints
        .iter()
        .filter(|_| severity.is_some())
        .map(|lint| Diagnostic {
            range: offsets_to_range(&text, lint.span.start, lint.span.end),
            message: format!("Unknown lint `{}`", lint.name),
            severity,
            code: Some(NumberOrString::String(Lint::UnknownLint.name().to_string())),
            ..Diagnostic::default()
        });

    let forbidden = project.forbidden_lints.iter().map(|lint| Diagnostic {
        range: offsets_to_range(&text, lint.span.start, lint.span.end),
        message: lint.kind().to_string(),
        severity: Some(DiagnosticSeverity::ERROR),
        ..Diagnostic::default()
    });

    unknown.chain(forbidden).collect()
}

/// Nothing can be compiled without the manifest, so this only reports why it's invalid.
fn invalid_manifest_diagnostics(
    path: &Path,
    error: &toml::de::Error,
) -> IndexMap<SourceKind, Vec<Diagnostic>> {
    let mut diagnostics = IndexMap::new();

    if let Ok(kind) = normalize_path(path) {
        diagnostics.insert(kind, vec![manifest_error(path, error)]);
    }

    diagnostics
}

/// The project that a file belongs to.
struct FoundProject {
    /// The project's manifest, or its entrypoint if it doesn't have one, which identifies it.
    key: PathBuf,
    /// The project, or why its manifest is invalid.
    project: std::result::Result<Project, toml::de::Error>,
}

impl FoundProject {
    fn find(path: &Path) -> Option<Self> {
        match find_project(path, false) {
            Ok(Some(project)) => Some(Self {
                key: project
                    .manifest_path
                    .clone()
                    .unwrap_or_else(|| project.entrypoint.clone()),
                project: Ok(project),
            }),
            Err(rue_options::Error::Toml { path, error }) => Some(Self {
                key: path,
                project: Err(error),
            }),
            Ok(None) | Err(_) => None,
        }
    }
}

/// The result of compiling a project, which hasn't been recorded yet.
struct Compiled {
    key: PathBuf,
    diagnostics: IndexMap<SourceKind, Vec<Diagnostic>>,
    caches: Vec<(SourceKind, SendWrapper<Cache<Arc<Compiler>>>)>,
    /// Whether the project doesn't exist anymore, so it's no longer tracked.
    removed: bool,
}

impl Compiled {
    fn removed(key: PathBuf) -> Self {
        Self {
            key,
            diagnostics: IndexMap::new(),
            caches: Vec::new(),
            removed: true,
        }
    }
}

/// Reports why `Rue.toml` couldn't be parsed, such as an invalid lint level.
fn manifest_error(path: &Path, error: &toml::de::Error) -> Diagnostic {
    let text = fs::read_to_string(path).unwrap_or_default();

    Diagnostic {
        range: error
            .span()
            .map(|span| offsets_to_range(&text, span.start, span.end))
            .unwrap_or_default(),
        message: error.message().to_string(),
        severity: Some(DiagnosticSeverity::ERROR),
        ..Diagnostic::default()
    }
}

#[cfg(test)]
mod tests;
