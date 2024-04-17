use rue_compiler::{analyze, DiagnosticKind};
use rue_parser::{line_col, parse, LineCol};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct Backend {
    client: Client,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
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
        .await
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

impl Backend {
    async fn on_change(&self, uri: Url, text: String, _version: i32) {
        let (root, errors) = parse(&text);
        let compiler_errors = analyze(root);

        let mut diagnostics: Vec<Diagnostic> = errors
            .into_iter()
            .map(|error| {
                let start = line_col(&text, error.span().start);
                let end = line_col(&text, error.span().end);

                diagnostic(
                    start,
                    end,
                    format!("{}", error.kind()),
                    DiagnosticSeverity::ERROR,
                )
            })
            .collect();

        for error in compiler_errors {
            let start = line_col(&text, error.span().start);
            let end = line_col(&text, error.span().end);

            let (message, severity) = match error.kind() {
                DiagnosticKind::Error(kind) => (format!("{}", kind), DiagnosticSeverity::ERROR),
                DiagnosticKind::Warning(kind) => (format!("{}", kind), DiagnosticSeverity::WARNING),
            };

            diagnostics.push(diagnostic(start, end, message, severity));
        }

        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
    }
}

fn diagnostic(
    start: LineCol,
    end: LineCol,
    message: String,
    severity: DiagnosticSeverity,
) -> Diagnostic {
    Diagnostic {
        range: Range {
            start: Position {
                line: start.line as u32,
                character: start.col as u32,
            },
            end: Position {
                line: end.line as u32,
                character: end.col as u32,
            },
        },
        message,
        severity: Some(severity),
        ..Diagnostic::default()
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend { client });
    Server::new(stdin, stdout, socket).serve(service).await;
}
