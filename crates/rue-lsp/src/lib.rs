#![allow(clippy::cast_possible_truncation)]

mod cache;
mod paths;
mod position;
mod project;
mod worker;

pub use paths::encode_uris;

use cache::Cache;
use rue_compiler::{Compiler, normalize_path};
use rue_diagnostic::{Applicability, LineIndex, Lint, SourceKind, SrcLoc};
use rue_formatter::{FormatError, FormatOptions, format_source};
use serde::{Deserialize, Serialize};

use std::any::Any;
use std::collections::HashMap;
use std::fmt::{self, Write};
use std::fs;
use std::panic::{self, AssertUnwindSafe};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, MutexGuard, PoisonError};

use rue_options::{Project, find_project};
use tower_lsp_server::jsonrpc::{Error, ErrorCode, Result};
use tower_lsp_server::ls_types::error_codes::REQUEST_FAILED;
use tower_lsp_server::ls_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, CodeActionParams,
    CodeActionProviderCapability, CodeActionResponse, CompletionOptions, CompletionParams,
    CompletionResponse, Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity,
    DidChangeConfigurationParams, DidChangeTextDocumentParams, DidChangeWatchedFilesParams,
    DidChangeWatchedFilesRegistrationOptions, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentChangeOperation, DocumentChanges,
    DocumentFormattingParams, DocumentSymbol, DocumentSymbolParams, DocumentSymbolResponse,
    FileSystemWatcher, GlobPattern, GotoDefinitionParams, GotoDefinitionResponse, Hover,
    HoverContents, HoverParams, HoverProviderCapability, InitializeParams, InitializeResult,
    InitializedParams, InlayHint, InlayHintKind, InlayHintLabel, InlayHintParams, InlayHintTooltip,
    LanguageString, Location, MarkedString, MessageType, NumberOrString, OneOf,
    OptionalVersionedTextDocumentIdentifier, ParameterInformation, ParameterLabel, Position,
    PrepareRenameResponse, Range, ReferenceParams, Registration, RenameFile, RenameOptions,
    RenameParams, ResourceOp, ResourceOperationKind, ServerCapabilities, SignatureHelp,
    SignatureHelpOptions, SignatureHelpParams, SignatureInformation, SymbolInformation,
    TextDocumentEdit, TextDocumentPositionParams, TextDocumentSyncCapability, TextDocumentSyncKind,
    TextEdit, Uri, WorkDoneProgressOptions, WorkspaceEdit, WorkspaceSymbol, WorkspaceSymbolParams,
    WorkspaceSymbolResponse,
};
use tower_lsp_server::{Client, LanguageServer};

use crate::cache::{
    HintKind, HintOptions, HoverInfo, Rename, Signatures, SymbolHoverKind, field_declarations,
};
use crate::paths::{Paths, document, file_name, uri_to_path};
use crate::position::{offset_to_position, offsets_to_range, source_range};
use crate::worker::{Diagnostics, Shared, State, Worker};
use tokio::sync::oneshot;

/// Logged once the client says it's initialized.
const INITIALIZED: &str = "Rue language server initialized.";

#[derive(Debug)]
pub struct Backend {
    shared: Arc<Shared>,
    /// Compiles projects and answers queries about them, on a thread of its own.
    worker: Worker,
    /// Whether the client can register file watchers when asked to.
    watch_files: AtomicBool,
    /// Whether the client asks for inlay hints again when it's told that they changed.
    refresh_hints: AtomicBool,
    /// Whether the client can show the symbols of a document inside of each other.
    nested_symbols: AtomicBool,
}

/// The work the server does in response to the editor, used to say what failed when it panics.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Task {
    Compile,
    Hover,
    Definition,
    References,
    Completion,
    Rename,
    SignatureHelp,
    InlayHints,
    DocumentSymbols,
    WorkspaceSymbols,
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
            Self::Rename => "renaming",
            Self::SignatureHelp => "computing signature help",
            Self::InlayHints => "computing inlay hints",
            Self::DocumentSymbols => "listing the symbols of a document",
            Self::WorkspaceSymbols => "searching for symbols",
            Self::Formatting => "formatting",
            Self::CodeAction => "computing code actions",
        })
    }
}

impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        let related_information = params
            .capabilities
            .text_document
            .as_ref()
            .and_then(|capabilities| capabilities.publish_diagnostics.as_ref())
            .and_then(|capabilities| capabilities.related_information)
            .unwrap_or(false);

        self.shared
            .related_information
            .store(related_information, Ordering::Relaxed);

        let watch_files = params
            .capabilities
            .workspace
            .as_ref()
            .and_then(|capabilities| capabilities.did_change_watched_files.as_ref())
            .and_then(|capabilities| capabilities.dynamic_registration)
            .unwrap_or(false);

        self.watch_files.store(watch_files, Ordering::Relaxed);

        // A module that's a file or a directory is renamed along with it, which the client has
        // to be able to do as part of an edit. Files are only renamed by an edit's document
        // changes, so a client that renames them takes those, whether or not it says so (Neovim
        // doesn't)
        let rename_files = params
            .capabilities
            .workspace
            .as_ref()
            .and_then(|capabilities| capabilities.workspace_edit.as_ref())
            .and_then(|capabilities| capabilities.resource_operations.as_ref())
            .is_some_and(|operations| operations.contains(&ResourceOperationKind::Rename));

        self.shared
            .rename_files
            .store(rename_files, Ordering::Relaxed);

        // The options can only be sent to a client that asks whether a name can be renamed first
        let prepare_rename = params
            .capabilities
            .text_document
            .as_ref()
            .and_then(|capabilities| capabilities.rename.as_ref())
            .and_then(|capabilities| capabilities.prepare_support)
            .unwrap_or(false);

        if let Some(options) = &params.initialization_options {
            set_hint_options(&self.shared, options);
        }

        // The hints are asked for again once the client changes which ones it wants
        let refresh_hints = params
            .capabilities
            .workspace
            .as_ref()
            .and_then(|capabilities| capabilities.inlay_hint.as_ref())
            .and_then(|capabilities| capabilities.refresh_support)
            .unwrap_or(false);

        self.refresh_hints.store(refresh_hints, Ordering::Relaxed);

        // A client that can't show symbols inside of each other gets them as a list
        let nested_symbols = params
            .capabilities
            .text_document
            .as_ref()
            .and_then(|capabilities| capabilities.document_symbol.as_ref())
            .and_then(|capabilities| capabilities.hierarchical_document_symbol_support)
            .unwrap_or(false);

        self.nested_symbols.store(nested_symbols, Ordering::Relaxed);

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
                self.shared.paths.add_root(&path);
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
                rename_provider: Some(if prepare_rename {
                    OneOf::Right(RenameOptions {
                        prepare_provider: Some(true),
                        work_done_progress_options: WorkDoneProgressOptions::default(),
                    })
                } else {
                    OneOf::Left(true)
                }),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec![
                        "(".to_string(),
                        ",".to_string(),
                        "{".to_string(),
                    ]),
                    ..Default::default()
                }),
                inlay_hint_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                workspace_symbol_provider: Some(OneOf::Left(true)),
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
        let client = &self.shared.client;

        client.log_message(MessageType::INFO, INITIALIZED).await;

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

            if let Err(error) = client.register_capability(vec![registration]).await {
                client
                    .log_message(
                        MessageType::WARNING,
                        format!("Couldn't watch files for changes: {error}"),
                    )
                    .await;
            }
        }
    }

    // Compiles happen in the background, and publish their diagnostics once they're done, so
    // these don't wait for them

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.shared.paths.open(&params.text_document.uri);
        drop(self.change(&params.text_document.uri, Some(params.text_document.text)));
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        // The server only asks for full syncs, so the last change has the whole text
        let Some(change) = params.content_changes.pop() else {
            return;
        };

        // Edits are compiled once they stop
        if let Some(path) = self.set_text(&params.text_document.uri, Some(change.text)) {
            self.worker.edit(path);
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        drop(self.change(&params.text_document.uri, None));
    }

    /// The file's unsaved changes are discarded, so the project is compiled with what's on disk,
    /// unless the file is still open through another URI (for example, through a symlink and
    /// through its target).
    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = &params.text_document.uri;
        self.shared.paths.close(uri);

        if let Some((path, kind)) = document(uri)
            && self.shared.paths.is_open(&kind)
        {
            drop(self.worker.compile(Vec::new(), vec![path]));
            return;
        }

        drop(self.change(uri, None));
    }

    /// Recompiles the projects that files changed on disk belong to, outside of the editor (for
    /// example, by switching branches).
    async fn did_change_watched_files(&self, params: DidChangeWatchedFilesParams) {
        let changed = params
            .changes
            .iter()
            .filter_map(|change| uri_to_path(&change.uri))
            .collect();

        drop(self.worker.compile(changed, Vec::new()));
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let HoverParams {
            text_document_position_params: params,
            ..
        } = params;

        let revision = self.revision(&params.text_document.uri);

        Ok(self
            .query(Task::Hover, move |state| {
                hover(
                    state.cache(&params.text_document.uri, revision)?,
                    params.position,
                )
            })
            .await)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let GotoDefinitionParams {
            text_document_position_params: params,
            ..
        } = params;

        let revision = self.revision(&params.text_document.uri);

        Ok(self
            .query(Task::Definition, move |state| {
                let shared = state.shared.clone();
                let cache = state.cache(&params.text_document.uri, revision)?;
                definitions(cache, &shared.paths, params.position)
            })
            .await)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let ReferenceParams {
            text_document_position: params,
            context,
            ..
        } = params;

        let revision = self.revision(&params.text_document.uri);

        Ok(self
            .query(Task::References, move |state| {
                let shared = state.shared.clone();
                let cache = state.cache(&params.text_document.uri, revision)?;
                references(
                    cache,
                    &shared.paths,
                    params.position,
                    context.include_declaration,
                )
            })
            .await)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let CompletionParams {
            text_document_position: params,
            ..
        } = params;

        let revision = self.revision(&params.text_document.uri);

        Ok(self
            .query(Task::Completion, move |state| {
                completions(
                    state.cache(&params.text_document.uri, revision)?,
                    params.position,
                )
            })
            .await)
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let revision = self.revision(&params.text_document.uri);

        self.query(Task::Rename, move |state| {
            let cache = state.cache(&params.text_document.uri, revision)?;
            Some(prepare_rename(cache, params.position))
        })
        .await
        .map_or(Ok(None), |result| result.map_err(request_failed))
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let RenameParams {
            text_document_position: params,
            new_name,
            ..
        } = params;

        let revision = self.revision(&params.text_document.uri);

        self.query(Task::Rename, move |state| {
            let shared = state.shared.clone();
            let cache = state.cache(&params.text_document.uri, revision)?;
            Some(rename(cache, &shared, params.position, &new_name))
        })
        .await
        .map_or(Ok(None), |result| result.map_err(request_failed))
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let SignatureHelpParams {
            text_document_position_params: params,
            ..
        } = params;

        let revision = self.revision(&params.text_document.uri);

        Ok(self
            .query(Task::SignatureHelp, move |state| {
                signature_help(
                    state.cache(&params.text_document.uri, revision)?,
                    params.position,
                )
            })
            .await)
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let revision = self.revision(&params.text_document.uri);
        let options = *lock(&self.shared.hints);

        Ok(self
            .query(Task::InlayHints, move |state| {
                Some(inlay_hints(
                    state.cache(&params.text_document.uri, revision)?,
                    params.range,
                    options,
                ))
            })
            .await)
    }

    /// The client's settings say which inlay hints it wants.
    async fn did_change_configuration(&self, params: DidChangeConfigurationParams) {
        if set_hint_options(&self.shared, &params.settings)
            && self.refresh_hints.load(Ordering::Relaxed)
        {
            self.shared.client.inlay_hint_refresh().await.ok();
        }
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri;
        let revision = self.revision(&uri);
        let nested = self.nested_symbols.load(Ordering::Relaxed);

        Ok(self
            .query(Task::DocumentSymbols, move |state| {
                let symbols = state.cache(&uri, revision)?.document_symbols();

                Some(if nested {
                    DocumentSymbolResponse::Nested(symbols)
                } else {
                    let mut list = Vec::new();
                    list_symbols(&uri, symbols, None, &mut list);
                    DocumentSymbolResponse::Flat(list)
                })
            })
            .await)
    }

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<WorkspaceSymbolResponse>> {
        Ok(self
            .query(Task::WorkspaceSymbols, move |state| {
                Some(WorkspaceSymbolResponse::Nested(workspace_symbols(
                    state,
                    &params.query,
                )))
            })
            .await)
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        self.catch_panic(Task::Formatting, || self.on_formatting(&params))
            .await
            .transpose()
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        Ok(self
            .catch_panic(Task::CodeAction, || {
                self.shared.inject_panic(Task::CodeAction);
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
        let shared = Arc::new(Shared::new(client));

        Self {
            worker: Worker::new(shared.clone()),
            shared,
            watch_files: AtomicBool::new(false),
            refresh_hints: AtomicBool::new(false),
            nested_symbols: AtomicBool::new(false),
        }
    }

    /// Runs a task, and if it panics, logs the panic to the client and returns `None` instead of
    /// taking down the server.
    async fn catch_panic<T>(&self, task: Task, f: impl FnOnce() -> T) -> Option<T> {
        match panic::catch_unwind(AssertUnwindSafe(f)) {
            Ok(value) => Some(value),
            Err(payload) => {
                self.shared.log_panic(task, &panic_message(&*payload)).await;
                None
            }
        }
    }

    /// Answers a query with the compiler's state, once the compiles before it are done. If it
    /// panics, the panic is logged and there's no answer.
    async fn query<T: Send + 'static>(
        &self,
        task: Task,
        f: impl FnOnce(&mut State) -> Option<T> + Send + 'static,
    ) -> Option<T> {
        let result = self.worker.run(Some(task), f);

        #[cfg(test)]
        self.shared.queries.fetch_add(1, Ordering::Relaxed);

        let result = result.await;

        match result {
            Ok(Ok(value)) => value,
            Ok(Err(message)) => {
                self.shared.log_panic(task, &message).await;
                None
            }
            Err(_) => None,
        }
    }

    /// The revision of the editor's text for a document, which a query is about.
    fn revision(&self, uri: &Uri) -> Option<u64> {
        let (_, kind) = document(uri)?;
        self.shared.revision(&kind)
    }

    /// Records the editor's text for a document (or that it should be read from disk), and
    /// returns its path. The text is kept even if the project can't be compiled right now, so
    /// that it's used once it can be.
    fn set_text(&self, uri: &Uri, text: Option<String>) -> Option<PathBuf> {
        // The file may have been deleted, for example before the editor closes it
        let (path, kind) = document(uri)?;
        self.shared.set_text(kind, text);
        Some(path)
    }

    /// Records the editor's text for a document, and compiles its project right away. The
    /// diagnostics are published once they're ready, and the receiver gets them too.
    fn change(&self, uri: &Uri, text: Option<String>) -> Option<oneshot::Receiver<Diagnostics>> {
        let path = self.set_text(uri, text)?;
        Some(self.worker.compile(Vec::new(), vec![path]))
    }

    #[cfg(test)]
    fn on_change_impl(&self, uri: &Uri, text: Option<String>) -> Diagnostics {
        self.change(uri, text)
            .and_then(|receiver| receiver.blocking_recv().ok())
            .unwrap_or_default()
    }

    fn on_formatting(&self, params: &DocumentFormattingParams) -> Result<Vec<TextEdit>> {
        self.shared.inject_panic(Task::Formatting);

        let (path, source_kind) = document(&params.text_document.uri)
            .ok_or_else(|| Error::invalid_params("document URI is not a file"))?;
        let project =
            find_project(&path, false).map_err(|error| Error::invalid_params(error.to_string()))?;
        let options = project.map_or_else(FormatOptions::default, |project| project.format_options);
        let source = match self.shared.text(&source_kind) {
            Some(source) => source,
            None => fs::read_to_string(&path)
                .map_err(|error| Error::invalid_params(error.to_string()))?,
        };

        formatting_edits(&source, &options)
            .map_err(|error| Error::invalid_params(error.to_string()))
    }
}

fn hover(cache: &mut Cache<Compiler>, position: Position) -> Option<Hover> {
    let position = cache.position(position);

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
                        field_declarations(&info.fields, info.nil_terminated)
                            .iter()
                            .map(|field| format!("    {field},"))
                            .collect::<Vec<String>>()
                            .join("\n")
                    ),
                    HoverInfo::Enum(info) => {
                        if info.variants.is_empty() {
                            format!("enum {} {{}}", info.name)
                        } else {
                            format!(
                                "enum {} {{\n{}\n}}",
                                info.name,
                                info.variants
                                    .iter()
                                    .map(|variant| format!("    {variant},"))
                                    .collect::<Vec<String>>()
                                    .join("\n")
                            )
                        }
                    }
                    HoverInfo::Variant(info) => format!("{}{}", info.name, info.declaration),
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

fn definitions(
    cache: &Cache<Compiler>,
    paths: &Paths,
    position: Position,
) -> Option<GotoDefinitionResponse> {
    let position = cache.position(position);

    let mut locations = cache
        .definitions(position)
        .into_iter()
        .map(|location| paths.location(location))
        .collect::<Vec<_>>();

    if locations.len() > 1 {
        Some(GotoDefinitionResponse::Array(locations))
    } else {
        locations.pop().map(GotoDefinitionResponse::Scalar)
    }
}

fn references(
    cache: &Cache<Compiler>,
    paths: &Paths,
    position: Position,
    include_declaration: bool,
) -> Option<Vec<Location>> {
    let position = cache.position(position);

    let locations = cache
        .references(position, include_declaration)
        .into_iter()
        .map(|location| paths.location(location))
        .collect::<Vec<_>>();

    (!locations.is_empty()).then_some(locations)
}

fn prepare_rename(
    cache: &Cache<Compiler>,
    position: Position,
) -> std::result::Result<Option<PrepareRenameResponse>, String> {
    let position = cache.position(position);

    Ok(cache
        .prepare_rename(position)?
        .map(|name| PrepareRenameResponse::RangeWithPlaceholder {
            range: name.range,
            placeholder: name.name,
        }))
}

fn rename(
    cache: &Cache<Compiler>,
    shared: &Shared,
    position: Position,
    new_name: &str,
) -> std::result::Result<Option<WorkspaceEdit>, String> {
    let position = cache.position(position);

    let Some(Rename { edits, moved }) = cache.rename(position, new_name)? else {
        return Ok(None);
    };

    // An edit that's left out would rename only some of the places
    let edits = edits
        .into_iter()
        .map(|(kind, edits)| Some((shared.paths.uri(&kind)?, edits)))
        .collect::<Option<Vec<_>>>()
        .ok_or("A file that would be edited isn't one that the editor can open")?
        .into_iter();

    let Some(moved) = moved else {
        return Ok(Some(WorkspaceEdit::new(edits.collect())));
    };

    if !shared.rename_files.load(Ordering::Relaxed) {
        return Err(format!(
            "`{}` would have to be renamed, which the editor doesn't support",
            file_name(&moved.from)
        ));
    }

    let Some((old_uri, new_uri)) = shared.paths.moved(&moved.from, &moved.to) else {
        return Err(format!(
            "`{}` can't be renamed, since it isn't a file that the editor can open",
            file_name(&moved.from)
        ));
    };

    // The files are edited where they are, and then moved
    let operations = edits
        .map(|(uri, edits)| {
            DocumentChangeOperation::Edit(TextDocumentEdit {
                text_document: OptionalVersionedTextDocumentIdentifier { uri, version: None },
                edits: edits.into_iter().map(OneOf::Left).collect(),
            })
        })
        .chain([DocumentChangeOperation::Op(ResourceOp::Rename(
            RenameFile {
                old_uri,
                new_uri,
                options: None,
                annotation_id: None,
            },
        ))])
        .collect();

    Ok(Some(WorkspaceEdit {
        document_changes: Some(DocumentChanges::Operations(operations)),
        ..WorkspaceEdit::default()
    }))
}

/// The error for a request that can't be answered, with the reason to show to the user.
fn request_failed(message: String) -> Error {
    Error {
        code: ErrorCode::ServerError(REQUEST_FAILED),
        message: message.into(),
        data: None,
    }
}

fn signature_help(cache: &mut Cache<Compiler>, position: Position) -> Option<SignatureHelp> {
    let position = cache.position(position);

    let scopes = cache.scopes(position);
    let Signatures { signatures, active } = cache.signature_help(&scopes, position)?;

    let active_parameter = signatures
        .get(active)
        .and_then(|signature| signature.active_parameter)
        .map(|parameter| parameter as u32);

    let signatures = signatures
        .into_iter()
        .map(|signature| {
            // The parameters are parts of the label, which are counted in UTF-16 code units
            let offset = |index: usize| signature.label[..index].encode_utf16().count() as u32;

            SignatureInformation {
                parameters: Some(
                    signature
                        .parameters
                        .iter()
                        .map(|parameter| ParameterInformation {
                            label: ParameterLabel::LabelOffsets([
                                offset(parameter.start),
                                offset(parameter.end),
                            ]),
                            documentation: None,
                        })
                        .collect(),
                ),
                active_parameter: signature.active_parameter.map(|parameter| parameter as u32),
                documentation: None,
                label: signature.label,
            }
        })
        .collect();

    Some(SignatureHelp {
        signatures,
        active_signature: Some(active as u32),
        active_parameter,
    })
}

fn inlay_hints(cache: &mut Cache<Compiler>, range: Range, options: HintOptions) -> Vec<InlayHint> {
    let range = cache.position(range.start)..cache.position(range.end);

    cache
        .inlay_hints(range, options)
        .into_iter()
        .map(|hint| InlayHint {
            position: hint.position,
            label: InlayHintLabel::String(hint.label),
            kind: Some(match hint.kind {
                HintKind::Type | HintKind::GenericArguments => InlayHintKind::TYPE,
                HintKind::Parameter => InlayHintKind::PARAMETER,
            }),
            text_edits: None,
            tooltip: hint.tooltip.map(InlayHintTooltip::String),
            padding_left: None,
            // A parameter's name is before its argument, and the rest are right after a name
            padding_right: Some(hint.kind == HintKind::Parameter),
            data: None,
        })
        .collect()
}

/// Sets which inlay hints the client wants from its settings, which are either the server's own
/// (`{ "inlayHints": { ... } }`, as the initialization options are) or every setting with the
/// server's under `rue`. The ones that aren't given stay as they are. Returns whether any changed.
fn set_hint_options(shared: &Shared, settings: &serde_json::Value) -> bool {
    let settings = settings.get("rue").unwrap_or(settings);

    let Some(settings) = settings.get("inlayHints") else {
        return false;
    };

    let mut options = lock(&shared.hints);
    let options = &mut *options;
    let previous = *options;

    for (name, option) in [
        ("types", &mut options.types),
        ("genericArguments", &mut options.generic_arguments),
        ("parameterNames", &mut options.parameter_names),
    ] {
        if let Some(value) = settings.get(name).and_then(serde_json::Value::as_bool) {
            *option = value;
        }
    }

    *options != previous
}

/// Adds the symbols of a document and the symbols inside of them to a list, each with the name
/// of the symbol it's inside of.
fn list_symbols(
    uri: &Uri,
    symbols: Vec<DocumentSymbol>,
    container: Option<&str>,
    list: &mut Vec<SymbolInformation>,
) {
    for symbol in symbols {
        #[allow(deprecated)]
        list.push(SymbolInformation {
            name: symbol.name.clone(),
            kind: symbol.kind,
            tags: None,
            deprecated: None,
            location: Location::new(uri.clone(), symbol.range),
            container_name: container.map(ToString::to_string),
        });

        list_symbols(
            uri,
            symbol.children.unwrap_or_default(),
            Some(&symbol.name),
            list,
        );
    }
}

/// The symbols of every project that's been compiled whose names match the query, best match
/// first.
fn workspace_symbols(state: &State, query: &str) -> Vec<WorkspaceSymbol> {
    state.shared.inject_panic(Task::WorkspaceSymbols);

    let mut symbols = state
        .projects
        .values()
        .flat_map(|cache| cache.project_symbols(query))
        .filter_map(|symbol| Some((state.shared.paths.uri(&symbol.file)?, symbol)))
        .collect::<Vec<_>>();

    // The projects are in no particular order, and a file can be part of more than one
    symbols.sort_by(|(a_uri, a), (b_uri, b)| {
        (b.score, &a.name, a_uri.as_str(), a.range.start).cmp(&(
            a.score,
            &b.name,
            b_uri.as_str(),
            b.range.start,
        ))
    });
    symbols.dedup_by(|(a_uri, a), (b_uri, b)| a_uri == b_uri && a.range == b.range);

    symbols
        .into_iter()
        .map(|(uri, symbol)| WorkspaceSymbol {
            name: symbol.name,
            kind: symbol.kind,
            tags: None,
            container_name: symbol.container,
            location: OneOf::Left(Location::new(uri, symbol.range)),
            data: None,
        })
        .collect()
}

fn completions(cache: &mut Cache<Compiler>, position: Position) -> Option<CompletionResponse> {
    let position = cache.position(position);

    let scopes = cache.scopes(position);
    let completions = cache.completions(&scopes, position);

    (!completions.is_empty()).then_some(CompletionResponse::Array(completions))
}

/// The message of a panic's payload.
fn panic_message(payload: &(dyn Any + Send)) -> String {
    payload
        .downcast_ref::<&str>()
        .map(ToString::to_string)
        .or_else(|| payload.downcast_ref::<String>().cloned())
        .unwrap_or_else(|| "unknown panic".to_string())
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
    offset_to_position(
        &LineIndex::with_carriage_returns(source),
        source,
        source.len(),
    )
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
        source_range(&srcloc.source, srcloc.span.start, srcloc.span.end),
    ))
}

fn suggestion_data(
    suggestion: &rue_diagnostic::Suggestion,
    paths: &Paths,
) -> Option<SuggestionData> {
    let mut changes = HashMap::<Uri, Vec<TextEdit>>::new();

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
        range: source_range(&srcloc.source, srcloc.span.start, srcloc.span.end),
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
    let lines = LineIndex::with_carriage_returns(&text);

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
            range: offsets_to_range(&lines, &text, lint.span.start, lint.span.end),
            message: format!("Unknown lint `{}`", lint.name),
            severity,
            code: Some(NumberOrString::String(Lint::UnknownLint.name().to_string())),
            ..Diagnostic::default()
        });

    let forbidden = project.forbidden_lints.iter().map(|lint| Diagnostic {
        range: offsets_to_range(&lines, &text, lint.span.start, lint.span.end),
        message: lint.kind().to_string(),
        severity: Some(DiagnosticSeverity::ERROR),
        ..Diagnostic::default()
    });

    unknown.chain(forbidden).collect()
}

/// Nothing can be compiled without the manifest, so this only reports why it's invalid.
fn invalid_manifest_diagnostics(path: &Path, error: &toml::de::Error) -> Diagnostics {
    let mut diagnostics = Diagnostics::new();

    if let Ok(kind) = normalize_path(path) {
        diagnostics.insert(kind, vec![manifest_error(path, error)]);
    }

    diagnostics
}

/// Reports why `Rue.toml` couldn't be parsed, such as an invalid lint level.
fn manifest_error(path: &Path, error: &toml::de::Error) -> Diagnostic {
    let text = fs::read_to_string(path).unwrap_or_default();
    let lines = LineIndex::with_carriage_returns(&text);

    Diagnostic {
        range: error
            .span()
            .map(|span| offsets_to_range(&lines, &text, span.start, span.end))
            .unwrap_or_default(),
        message: error.message().to_string(),
        severity: Some(DiagnosticSeverity::ERROR),
        ..Diagnostic::default()
    }
}

#[cfg(test)]
mod tests;
