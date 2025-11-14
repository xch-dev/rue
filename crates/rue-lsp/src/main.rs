#![allow(clippy::cast_possible_truncation)]

mod cache;

use cache::Cache;
use indexmap::IndexMap;
use rue_compiler::{Compiler, FileTree, normalize_path};
use rue_diagnostic::SourceKind;

use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use rue_options::find_project;
use send_wrapper::SendWrapper;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    CompletionOptions, CompletionParams, CompletionResponse, Diagnostic, DiagnosticSeverity,
    DidChangeTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams,
    GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverContents, HoverParams,
    HoverProviderCapability, InitializeParams, InitializeResult, InitializedParams, LanguageString,
    Location, MarkedString, MessageType, OneOf, Position, Range, ReferenceParams,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, Url,
};
use tower_lsp::{Client, LanguageServer, LspService, Server};

use crate::cache::HoverInfo;

#[derive(Debug)]
struct Backend {
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
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".to_string(), ",".to_string()]),
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
        self.on_change(params.text_document.uri, None).await;
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

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

impl Backend {
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
        let uri_path = uri.to_file_path().unwrap();
        let Some(project) = find_project(&uri_path, false).unwrap() else {
            return IndexMap::new();
        };

        let mut ctx = Compiler::new(project.options);

        let source_kind = normalize_path(&uri_path).unwrap();

        let mut file_cache = self.file_cache.lock().unwrap();

        if let Some(text) = text {
            file_cache.insert(source_kind.clone(), text);
        } else {
            file_cache.remove(&source_kind);
        }

        let tree = FileTree::compile_path(&mut ctx, &project.entrypoint, &mut file_cache).unwrap();

        drop(file_cache);

        let mut diagnostics = IndexMap::<SourceKind, Vec<Diagnostic>>::new();

        for item in ctx.take_diagnostics() {
            diagnostics
                .entry(item.srcloc.source.kind.clone())
                .or_default()
                .push(diagnostic(&item));
        }

        let mut cache = self.cache.lock().unwrap();

        let ctx = Arc::new(ctx);

        for file in tree.all_files() {
            let SourceKind::File(path) = &file.source.kind else {
                continue;
            };

            let uri = Url::from_file_path(path).unwrap();

            cache.insert(
                uri,
                SendWrapper::new(Cache::new(ctx.clone(), file.source.clone())),
            );

            diagnostics.entry(file.source.kind.clone()).or_default();
        }

        diagnostics
    }

    fn on_hover(&self, params: &HoverParams) -> Option<Hover> {
        let cache = self
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
                        HoverInfo::Symbol(info) => format!("let {}: {}", info.name, info.type_name),
                        HoverInfo::Module(info) => format!("mod {}", info.name),
                        HoverInfo::Type(info) => {
                            if let Some(inner) = &info.inner_name {
                                format!("type {} = {}", info.name, inner)
                            } else {
                                format!("type {}", info.name)
                            }
                        }
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

        let range = cache.definitions(position);

        if range.is_empty() {
            None
        } else if range.len() == 1 {
            Some(GotoDefinitionResponse::Scalar(Location::new(uri, range[0])))
        } else {
            Some(GotoDefinitionResponse::Array(
                range
                    .into_iter()
                    .map(|range| Location::new(uri.clone(), range))
                    .collect(),
            ))
        }
    }

    fn on_references(&self, params: &ReferenceParams) -> Option<Vec<Location>> {
        let uri = params.text_document_position.text_document.uri.clone();

        let cache = self.cache.lock().unwrap().get(&uri)?.to_cloned();
        let position = cache.position(params.text_document_position.position);

        let range = cache.references(position);

        if range.is_empty() {
            None
        } else {
            Some(
                range
                    .into_iter()
                    .map(|range| Location::new(uri.clone(), range))
                    .collect(),
            )
        }
    }

    fn on_completion(&self, params: &CompletionParams) -> Option<CompletionResponse> {
        let uri = params.text_document_position.text_document.uri.clone();

        let mut cache = self.cache.lock().unwrap().get(&uri)?.to_cloned();
        let position = cache.position(params.text_document_position.position);

        let scopes = cache.scopes(position);

        Some(CompletionResponse::Array(
            cache.completions(&scopes, position),
        ))
    }
}

fn diagnostic(diagnostic: &rue_diagnostic::Diagnostic) -> Diagnostic {
    let start = diagnostic.start();
    let end = diagnostic.end();

    Diagnostic {
        range: Range {
            start: Position {
                line: start.line.try_into().unwrap(),
                character: start.col.try_into().unwrap(),
            },
            end: Position {
                line: end.line.try_into().unwrap(),
                character: end.col.try_into().unwrap(),
            },
        },
        message: diagnostic.kind.to_string(),
        severity: Some(match diagnostic.kind.severity() {
            rue_diagnostic::DiagnosticSeverity::Error => DiagnosticSeverity::ERROR,
            rue_diagnostic::DiagnosticSeverity::Warning => DiagnosticSeverity::WARNING,
        }),
        ..Diagnostic::default()
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        cache: Mutex::new(HashMap::new()),
        file_cache: Mutex::new(HashMap::new()),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
