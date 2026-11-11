#![allow(clippy::cast_possible_truncation)]

mod cache;
mod position;

use cache::Cache;
use indexmap::IndexMap;
use rue_compiler::{Compiler, FileTree, normalize_path};
use rue_diagnostic::{Applicability, Lint, SourceKind, SrcLoc};
use rue_formatter::{FormatError, FormatOptions, format_source};
use serde::{Deserialize, Serialize};

use std::collections::HashMap;
use std::fs;
use std::path::Path;
use std::sync::{Arc, Mutex};

use rue_options::{Project, find_project};
use send_wrapper::SendWrapper;
use tower_lsp::jsonrpc::{Error, Result};
use tower_lsp::lsp_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, CodeActionParams,
    CodeActionProviderCapability, CodeActionResponse, CompletionOptions, CompletionParams,
    CompletionResponse, Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity,
    DidChangeTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams,
    DocumentFormattingParams, GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverContents,
    HoverParams, HoverProviderCapability, InitializeParams, InitializeResult, InitializedParams,
    LanguageString, Location, MarkedString, MessageType, NumberOrString, OneOf, Position, Range,
    ReferenceParams, ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind,
    TextEdit, Url, WorkspaceEdit,
};
use tower_lsp::{Client, LanguageServer};

use crate::cache::{HoverInfo, SymbolHoverKind};
use crate::position::offsets_to_range;

#[derive(Debug)]
pub struct Backend {
    client: Client,
    cache: Mutex<HashMap<Url, SendWrapper<Cache<Arc<Compiler>>>>>,
    file_cache: Mutex<HashMap<SourceKind, String>>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
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
            .log_message(MessageType::INFO, "Rue language server initialized.")
            .await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.on_change(params.text_document.uri, Some(params.text_document.text))
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.on_change(
            params.text_document.uri,
            Some(params.content_changes[0].text.clone()),
        )
        .await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        self.on_change(params.text_document.uri, None).await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        Ok(self.on_hover(&params))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        Ok(self.on_goto_definition(&params))
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        Ok(self.on_references(&params))
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        Ok(self.on_completion(&params))
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        self.on_formatting(&params).map(Some)
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        Ok(code_actions(&params))
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
        }
    }

    async fn on_change(&self, uri: Url, text: Option<String>) {
        let diagnostics = self.on_change_impl(&uri, text);

        for (kind, diagnostics) in diagnostics {
            let SourceKind::File(path) = kind else {
                continue;
            };

            let uri = Url::from_file_path(path).unwrap();

            self.client
                .publish_diagnostics(uri, diagnostics, None)
                .await;
        }
    }

    fn on_change_impl(
        &self,
        uri: &Url,
        text: Option<String>,
    ) -> IndexMap<SourceKind, Vec<Diagnostic>> {
        let Ok(uri_path) = uri.to_file_path() else {
            return IndexMap::new();
        };

        let Ok(source_kind) = normalize_path(&uri_path) else {
            return IndexMap::new();
        };

        // Keep track of the editor's text even if the project can't be compiled right now, so
        // that it's used once it can be
        {
            let mut file_cache = self.file_cache.lock().unwrap();

            if let Some(text) = text {
                file_cache.insert(source_kind.clone(), text);
            } else {
                file_cache.remove(&source_kind);
            }
        }

        let project = match find_project(&uri_path, false) {
            Ok(Some(project)) => project,
            Err(rue_options::Error::Toml { path, error }) => {
                return self.invalid_manifest_diagnostics(&path, &error, source_kind);
            }
            Ok(None) | Err(_) => return IndexMap::new(),
        };

        let manifest_diagnostics = project.manifest_path.as_deref().and_then(|path| {
            Some((
                normalize_path(path).ok()?,
                manifest_diagnostics(path, &project),
            ))
        });

        let mut ctx = Compiler::new(project.options);

        let mut file_cache = self.file_cache.lock().unwrap();

        let tree = FileTree::compile_path(&mut ctx, &project.entrypoint, &mut file_cache);

        drop(file_cache);

        let Ok(tree) = tree else {
            return IndexMap::new();
        };

        let mut diagnostics = IndexMap::<SourceKind, Vec<Diagnostic>>::new();

        for item in ctx.take_diagnostics() {
            diagnostics
                .entry(item.srcloc.source.kind.clone())
                .or_default()
                .push(diagnostic(&item));
        }

        let mut cache = self.cache.lock().unwrap();

        let ctx = Arc::new(ctx);
        let tree = Arc::new(SendWrapper::new(tree));

        for file in tree.all_files() {
            let SourceKind::File(path) = &file.source.kind else {
                continue;
            };

            let uri = Url::from_file_path(path).unwrap();

            cache.insert(
                uri,
                SendWrapper::new(Cache::new(ctx.clone(), tree.clone(), file.source.clone())),
            );

            diagnostics.entry(file.source.kind.clone()).or_default();
        }

        // Always include the manifest, so that its diagnostics are cleared once they're fixed
        if let Some((kind, manifest_diagnostics)) = manifest_diagnostics {
            diagnostics.insert(kind, manifest_diagnostics);
        }

        diagnostics
    }

    /// Nothing can be compiled without the manifest, so this only reports why it's invalid. The
    /// diagnostics of the project's files are cleared, since they're out of date.
    fn invalid_manifest_diagnostics(
        &self,
        path: &Path,
        error: &toml::de::Error,
        changed: SourceKind,
    ) -> IndexMap<SourceKind, Vec<Diagnostic>> {
        let mut diagnostics = IndexMap::new();

        let Ok(manifest_kind) = normalize_path(path) else {
            return diagnostics;
        };

        diagnostics.insert(changed, Vec::new());

        if let Some(dir) = path.parent() {
            for uri in self.cache.lock().unwrap().keys() {
                if let Ok(file) = uri.to_file_path()
                    && file.starts_with(dir)
                    && let Ok(kind) = normalize_path(&file)
                {
                    diagnostics.insert(kind, Vec::new());
                }
            }
        }

        diagnostics.insert(manifest_kind, vec![manifest_error(path, error)]);

        diagnostics
    }

    fn on_formatting(&self, params: &DocumentFormattingParams) -> Result<Vec<TextEdit>> {
        let uri = &params.text_document.uri;
        let path = uri
            .to_file_path()
            .map_err(|()| Error::invalid_params("document URI is not a file"))?;
        let source_kind =
            normalize_path(&path).map_err(|error| Error::invalid_params(error.to_string()))?;
        let project =
            find_project(&path, false).map_err(|error| Error::invalid_params(error.to_string()))?;
        let options = project.map_or_else(FormatOptions::default, |project| project.format_options);
        let source = self.file_cache.lock().unwrap().get(&source_kind).cloned();
        let source = match source {
            Some(source) => source,
            None => fs::read_to_string(&path)
                .map_err(|error| Error::invalid_params(error.to_string()))?,
        };

        formatting_edits(&source, &options)
            .map_err(|error| Error::invalid_params(error.to_string()))
    }

    fn on_hover(&self, params: &HoverParams) -> Option<Hover> {
        let mut cache = self
            .cache
            .lock()
            .unwrap()
            .get(&params.text_document_position_params.text_document.uri)?
            .to_cloned();
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
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .clone();

        let cache = self.cache.lock().unwrap().get(&uri)?.to_cloned();
        let position = cache.position(params.text_document_position_params.position);

        let locations = cache.definitions(position);

        if locations.is_empty() {
            None
        } else if locations.len() == 1 {
            Some(GotoDefinitionResponse::Scalar(locations[0].clone()))
        } else {
            Some(GotoDefinitionResponse::Array(locations))
        }
    }

    fn on_references(&self, params: &ReferenceParams) -> Option<Vec<Location>> {
        let uri = params.text_document_position.text_document.uri.clone();

        let cache = self.cache.lock().unwrap().get(&uri)?.to_cloned();
        let position = cache.position(params.text_document_position.position);

        let locations = cache.references(position);

        if locations.is_empty() {
            None
        } else {
            Some(locations)
        }
    }

    fn on_completion(&self, params: &CompletionParams) -> Option<CompletionResponse> {
        let uri = params.text_document_position.text_document.uri.clone();

        let mut cache = self.cache.lock().unwrap().get(&uri)?.to_cloned();
        let position = cache.position(params.text_document_position.position);

        let scopes = cache.scopes(position);
        let completions = cache.completions(&scopes, position);

        if completions.is_empty() {
            return None;
        }

        Some(CompletionResponse::Array(completions))
    }
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

fn location(srcloc: &SrcLoc) -> Option<Location> {
    let SourceKind::File(path) = &srcloc.source.kind else {
        return None;
    };

    Some(Location::new(
        Url::from_file_path(path).ok()?,
        offsets_to_range(&srcloc.source.text, srcloc.span.start, srcloc.span.end),
    ))
}

fn suggestion_data(suggestion: &rue_diagnostic::Suggestion) -> Option<SuggestionData> {
    let mut changes = HashMap::<Url, Vec<TextEdit>>::new();

    for edit in &suggestion.edits {
        let location = location(&edit.srcloc)?;

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

fn diagnostic(diagnostic: &rue_diagnostic::Diagnostic) -> Diagnostic {
    let srcloc = &diagnostic.srcloc;

    let mut message = diagnostic.kind.to_string();
    let mut related_information = Vec::new();

    // Notes that point to a file are shown at that location, and the rest are part of the message
    for note in &diagnostic.notes {
        if let Some(location) = note.srcloc.as_ref().and_then(location) {
            related_information.push(DiagnosticRelatedInformation {
                location,
                message: note.message.clone(),
            });
        } else {
            message.push('\n');
            message.push_str(&note.message);
        }
    }

    let suggestions = diagnostic
        .suggestions
        .iter()
        .filter_map(suggestion_data)
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
