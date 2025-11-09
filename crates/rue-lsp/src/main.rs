#![allow(clippy::cast_possible_truncation)]

mod cache;

use cache::Cache;
use rue_compiler::{Compiler, FileTree, normalize_path};
use serde::{Deserialize, Serialize};

use std::collections::HashMap;
use std::fs;
use std::sync::Mutex;

use rue_options::CompilerOptions;
use send_wrapper::SendWrapper;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    CompletionOptions, CompletionParams, CompletionResponse, Diagnostic, DiagnosticSeverity,
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, GotoDefinitionParams,
    GotoDefinitionResponse, Hover, HoverContents, HoverParams, HoverProviderCapability,
    InitializeParams, InitializeResult, InitializedParams, LanguageString, Location, MarkedString,
    MessageType, OneOf, Position, Range, ReferenceParams, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, Url,
};
use tower_lsp::{Client, LanguageServer, LspService, Server};

use crate::cache::HoverInfo;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Manifest {
    pub entrypoint: String,
}

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
        self.on_change(params.text_document.uri).await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        self.on_change(params.text_document.uri).await;
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
    async fn on_change(&self, uri: Url) {
        let diagnostics = self.on_change_impl(&uri);
        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
    }

    fn on_change_impl(&self, uri: &Url) -> Vec<Diagnostic> {
        let mut ctx = Compiler::new(CompilerOptions::default());

        let mut path = uri.to_file_path().unwrap();

        let source_kind = normalize_path(&path).unwrap();

        let mut manifest_path = path.clone();

        loop {
            if manifest_path.is_file()
                || !fs::read_dir(&manifest_path)
                    .map_or(vec![], Iterator::collect)
                    .into_iter()
                    .any(|entry| {
                        let child = entry.unwrap().path();
                        child.is_file()
                            && child.file_name().unwrap().to_string_lossy() == "Rue.toml"
                    })
            {
                let Some(parent) = manifest_path.parent() else {
                    break;
                };
                manifest_path = parent.to_path_buf();
                continue;
            }

            let manifest = fs::read_to_string(manifest_path.join("Rue.toml")).unwrap();
            let manifest: Manifest = toml::from_str(&manifest).unwrap();

            let entrypoint = manifest_path.join(manifest.entrypoint);

            if path.starts_with(&entrypoint) {
                path = entrypoint;
            }

            break;
        }

        let tree = FileTree::compile_path(&mut ctx, &path).unwrap();

        let diagnostics = ctx
            .take_diagnostics()
            .iter()
            .filter(|diagnostic| diagnostic.srcloc.source.kind == source_kind)
            .map(diagnostic)
            .collect();

        let FileTree::File(file) = tree.find(&source_kind).unwrap() else {
            unreachable!();
        };

        let mut cache = self.cache.lock().unwrap();
        cache.insert(
            uri.clone(),
            SendWrapper::new(Cache::new(ctx, file.source.clone())),
        );

        diagnostics
    }

    fn on_hover(&self, params: &HoverParams) -> Option<Hover> {
        let cache = self.cache.lock().unwrap();
        let cache = cache.get(&params.text_document_position_params.text_document.uri)?;
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

    fn on_completion(&self, params: &CompletionParams) -> Option<CompletionResponse> {
        let uri = params.text_document_position.text_document.uri.clone();

        let mut cache = self.cache.lock().unwrap();
        let cache = cache.get_mut(&uri)?;
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
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
