//! End-to-end tests that drive the language server through JSON-RPC messages, the same way an
//! editor would.

use std::fs;

use futures::StreamExt;
use serde_json::{Value, json};
use tokio::sync::mpsc;
use tower::{Service, ServiceExt};
use tower_lsp::LspService;
use tower_lsp::jsonrpc::{Request, Response};
use tower_lsp::lsp_types::Url;

use crate::Backend;

struct Client {
    service: LspService<Backend>,
    notifications: mpsc::UnboundedReceiver<Request>,
    next_id: i64,
}

impl Client {
    fn new() -> Self {
        let (service, mut socket) = LspService::new(Backend::new);
        let (sender, notifications) = mpsc::unbounded_channel();

        // The server blocks on sending messages to the client, so they must be drained
        // concurrently.
        tokio::spawn(async move {
            while let Some(message) = socket.next().await {
                if sender.send(message).is_err() {
                    break;
                }
            }
        });

        Self {
            service,
            notifications,
            next_id: 0,
        }
    }

    async fn request(&mut self, method: &'static str, params: Option<Value>) -> Response {
        self.next_id += 1;

        let mut request = Request::build(method).id(self.next_id);

        if let Some(params) = params {
            request = request.params(params);
        }

        let request = request.finish();

        self.service
            .ready()
            .await
            .unwrap()
            .call(request)
            .await
            .unwrap()
            .expect("expected a response")
    }

    async fn notify(&mut self, method: &'static str, params: Value) {
        let request = Request::build(method).params(params).finish();

        let response = self
            .service
            .ready()
            .await
            .unwrap()
            .call(request)
            .await
            .unwrap();

        assert!(response.is_none(), "notifications don't have responses");
    }

    async fn notification(&mut self, method: &str) -> Value {
        loop {
            let message = self.notifications.recv().await.expect("server closed");

            if message.method() == method {
                return message.params().cloned().unwrap_or(Value::Null);
            }
        }
    }
}

#[tokio::test]
async fn test_protocol_session() {
    let root = std::env::temp_dir().join(format!("rue-lsp-protocol-{}", std::process::id()));
    fs::create_dir_all(&root).unwrap();
    let root = root.canonicalize().unwrap();

    let path = root.join("main.rue");
    let text = "fn main() -> Int {\n    let value = 42;\n    value\n}\n";
    fs::write(
        root.join("Rue.toml"),
        "[compiler]\nentrypoint = \".\"\nstd = false\n",
    )
    .unwrap();
    fs::write(&path, text).unwrap();

    let uri = Url::from_file_path(&path).unwrap();

    let mut client = Client::new();

    let response = client
        .request("initialize", Some(json!({ "capabilities": {} })))
        .await;
    let capabilities = &response.result().unwrap()["capabilities"];
    assert_eq!(capabilities["hoverProvider"], json!(true));
    assert_eq!(capabilities["definitionProvider"], json!(true));
    assert_eq!(capabilities["referencesProvider"], json!(true));
    assert_eq!(capabilities["documentFormattingProvider"], json!(true));
    assert_eq!(capabilities["textDocumentSync"], json!(1));
    assert_eq!(
        capabilities["completionProvider"]["triggerCharacters"],
        json!([".", ",", ":"])
    );

    client.notify("initialized", json!({})).await;

    client
        .notify(
            "textDocument/didOpen",
            json!({
                "textDocument": {
                    "uri": uri,
                    "languageId": "rue",
                    "version": 1,
                    "text": text,
                }
            }),
        )
        .await;

    let diagnostics = client.notification("textDocument/publishDiagnostics").await;
    assert_eq!(diagnostics["uri"], json!(uri));
    assert_eq!(diagnostics["diagnostics"], json!([]));

    let response = client
        .request(
            "textDocument/hover",
            Some(json!({
                "textDocument": { "uri": uri },
                "position": { "line": 2, "character": 6 },
            })),
        )
        .await;
    assert_eq!(
        response.result().unwrap()["contents"],
        json!([{ "language": "rue", "value": "let value: 42" }])
    );

    let response = client
        .request(
            "textDocument/definition",
            Some(json!({
                "textDocument": { "uri": uri },
                "position": { "line": 2, "character": 6 },
            })),
        )
        .await;
    assert_eq!(
        response.result().unwrap(),
        &json!({
            "uri": uri,
            "range": {
                "start": { "line": 1, "character": 8 },
                "end": { "line": 1, "character": 13 },
            },
        })
    );

    client
        .notify(
            "textDocument/didChange",
            json!({
                "textDocument": { "uri": uri, "version": 2 },
                "contentChanges": [{ "text": "fn main() -> Int {\n    \"hello\"\n}\n" }],
            }),
        )
        .await;

    let diagnostics = client.notification("textDocument/publishDiagnostics").await;
    assert_eq!(diagnostics["uri"], json!(uri));
    let diagnostics = diagnostics["diagnostics"].as_array().unwrap();
    assert_eq!(diagnostics.len(), 1);
    assert_eq!(diagnostics[0]["severity"], json!(1));

    let response = client
        .request(
            "textDocument/formatting",
            Some(json!({
                "textDocument": { "uri": uri },
                "options": { "tabSize": 4, "insertSpaces": true },
            })),
        )
        .await;
    assert_eq!(response.result().unwrap(), &json!([]));

    let response = client.request("shutdown", None).await;
    assert!(response.is_ok());

    fs::remove_dir_all(&root).ok();
}
