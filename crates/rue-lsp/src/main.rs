#![allow(clippy::cast_possible_truncation)]

mod cache;

use cache::Cache;

use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use rue_compiler::analyze_file;
use rue_diagnostic::{Source, SourceKind};
use rue_options::CompilerOptions;
use send_wrapper::SendWrapper;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidOpenTextDocumentParams,
    GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverContents, HoverParams,
    HoverProviderCapability, InitializeParams, InitializeResult, InitializedParams, LanguageString,
    Location, MarkedString, MessageType, OneOf, Position, Range, ReferenceParams,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, Url,
};
use tower_lsp::{Client, LanguageServer, LspService, Server};

use crate::cache::{HoverInfo, NameKind};

#[derive(Debug)]
struct Backend {
    client: Client,
    cache: Mutex<HashMap<Url, SendWrapper<Cache>>>,
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
        self.on_change(
            params.text_document.uri,
            params.text_document.text,
            params.text_document.version,
        )
        .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.on_change(
            params.text_document.uri,
            params.content_changes[0].text.clone(),
            params.text_document.version,
        )
        .await;
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

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

impl Backend {
    async fn on_change(&self, uri: Url, text: String, _version: i32) {
        let diagnostics = self.on_change_impl(&uri, &text);
        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
    }

    fn on_change_impl(&self, uri: &Url, text: &str) -> Vec<Diagnostic> {
        let compilation = analyze_file(
            Source::new(
                Arc::from(text),
                SourceKind::File(
                    uri.to_file_path()
                        .unwrap()
                        .file_name()
                        .unwrap()
                        .to_str()
                        .unwrap()
                        .to_string(),
                ),
            ),
            CompilerOptions::default(),
        )
        .unwrap();

        let diagnostics = compilation.diagnostics.iter().map(diagnostic).collect();

        let mut cache = self.cache.lock().unwrap();
        cache.insert(uri.clone(), SendWrapper::new(Cache::new(compilation)));

        diagnostics
    }

    fn on_hover(&self, params: &HoverParams) -> Option<Hover> {
        let cache = self.cache.lock().unwrap();
        let cache = cache.get(&params.text_document_position_params.text_document.uri)?;
        let position = cache.position(params.text_document_position_params.position);

        let scopes = cache.scopes(position);
        let info = cache.hover(&scopes, position)?;

        let content = MarkedString::LanguageString(LanguageString {
            language: "rue".to_string(),
            value: match &info {
                HoverInfo::Symbol(info) => format!("let {}: {}", info.name, info.type_name),
                HoverInfo::Type(info) => {
                    if let Some(inner) = &info.inner_name {
                        format!("type {} = {}", info.name, inner)
                    } else {
                        format!("type {}", info.name)
                    }
                }
                HoverInfo::Field(info) => format!("{}: {}", info.name, info.type_name),
            },
        });

        let kind = match info {
            HoverInfo::Symbol(info) => info.kind,
            HoverInfo::Type(info) => info.kind,
            HoverInfo::Field(info) => info.kind,
        };

        Some(Hover {
            contents: HoverContents::Array(vec![
                content,
                MarkedString::String(match kind {
                    NameKind::Declaration => "Declaration".to_string(),
                    NameKind::Reference => "Reference".to_string(),
                    NameKind::Initializer => "Initializer".to_string(),
                }),
            ]),
            range: None,
        })
    }

    fn on_goto_definition(&self, params: &GotoDefinitionParams) -> Option<GotoDefinitionResponse> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .clone();

        let cache = self.cache.lock().unwrap();
        let cache = cache.get(&uri)?;
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

        let cache = self.cache.lock().unwrap();
        let cache = cache.get(&uri)?;
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
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
