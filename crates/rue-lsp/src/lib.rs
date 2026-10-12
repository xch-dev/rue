#![allow(clippy::cast_possible_truncation)]

mod cache;
mod position;

use cache::Cache;
use indexmap::IndexMap;
use rue_compiler::{Compiler, FileTree, normalize_path};
use rue_diagnostic::SourceKind;
use rue_formatter::{FormatError, FormatOptions, format_source};

use std::collections::HashMap;
use std::fs;
use std::sync::{Arc, Mutex};

use rue_options::find_project;
use send_wrapper::SendWrapper;
use tower_lsp::jsonrpc::{Error, Result};
use tower_lsp::lsp_types::{
    CompletionOptions, CompletionParams, CompletionResponse, Diagnostic, DiagnosticSeverity,
    DidChangeTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams,
    DocumentFormattingParams, GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverContents,
    HoverParams, HoverProviderCapability, InitializeParams, InitializeResult, InitializedParams,
    LanguageString, Location, MarkedString, MessageType, OneOf, Position, Range, ReferenceParams,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, TextEdit, Url,
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

        let Ok(Some(project)) = find_project(&uri_path, false) else {
            return IndexMap::new();
        };

        let Ok(source_kind) = normalize_path(&uri_path) else {
            return IndexMap::new();
        };

        let mut ctx = Compiler::new(project.options);

        let mut file_cache = self.file_cache.lock().unwrap();

        if let Some(text) = text {
            file_cache.insert(source_kind.clone(), text);
        } else {
            file_cache.remove(&source_kind);
        }

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

fn diagnostic(diagnostic: &rue_diagnostic::Diagnostic) -> Diagnostic {
    let srcloc = &diagnostic.srcloc;

    Diagnostic {
        range: offsets_to_range(&srcloc.source.text, srcloc.span.start, srcloc.span.end),
        message: diagnostic.kind.to_string(),
        severity: Some(match diagnostic.kind.severity() {
            rue_diagnostic::DiagnosticSeverity::Error => DiagnosticSeverity::ERROR,
            rue_diagnostic::DiagnosticSeverity::Warning => DiagnosticSeverity::WARNING,
        }),
        ..Diagnostic::default()
    }
}

#[cfg(test)]
mod tests;
