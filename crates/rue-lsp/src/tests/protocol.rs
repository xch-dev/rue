//! End-to-end tests that drive the language server through JSON-RPC messages, the same way an
//! editor would.

use std::fs;
use std::path::PathBuf;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::Duration;

use futures::StreamExt;
use serde_json::{Value, json};
use tokio::sync::mpsc;
use tower::{Service, ServiceExt};
use tower_lsp::LspService;
use tower_lsp::jsonrpc::{Request, Response};
use tower_lsp::lsp_types::Url;

use crate::{Backend, Task, lock};

/// How long to wait for a message from the server before failing the test, rather than hanging.
const TIMEOUT: Duration = Duration::from_secs(30);

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

    fn backend(&self) -> &Backend {
        self.service.inner()
    }

    /// Sends `initialize` and `initialized`, and skips the log message that follows.
    async fn initialize(&mut self) {
        let response = self
            .request("initialize", Some(json!({ "capabilities": {} })))
            .await;
        assert!(response.is_ok());

        self.notify("initialized", json!({})).await;
        let message = self.notification("window/logMessage").await;
        assert_eq!(message["type"], json!(3));
    }

    async fn open(&mut self, uri: &Url, text: &str) {
        self.notify(
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
    }

    async fn change(&mut self, uri: &Url, version: i32, changes: &[&str]) {
        let changes = changes
            .iter()
            .map(|text| json!({ "text": text }))
            .collect::<Vec<_>>();

        self.notify(
            "textDocument/didChange",
            json!({
                "textDocument": { "uri": uri, "version": version },
                "contentChanges": changes,
            }),
        )
        .await;
    }

    async fn notification(&mut self, method: &str) -> Value {
        loop {
            let message = tokio::time::timeout(TIMEOUT, self.notifications.recv())
                .await
                .unwrap_or_else(|_| panic!("timed out waiting for {method}"))
                .expect("server closed");

            if message.method() == method {
                return message.params().cloned().unwrap_or(Value::Null);
            }
        }
    }

    /// Waits for the diagnostics published for the given file, skipping those for other files
    /// (such as `Rue.toml`).
    async fn diagnostics(&mut self, uri: &Value) -> Value {
        loop {
            let diagnostics = self.notification("textDocument/publishDiagnostics").await;

            if &diagnostics["uri"] == uri {
                return diagnostics;
            }
        }
    }

    /// Returns the messages the server sent for the notifications sent so far, which have all
    /// been handled once the server answers a request sent after them.
    async fn sync(&mut self) -> Vec<Request> {
        let uri = Url::parse("file:///nonexistent.rue").unwrap();
        let response = self
            .request("textDocument/hover", Some(position(&uri, 0, 0)))
            .await;
        assert_eq!(response.result(), Some(&Value::Null));

        let mut messages = Vec::new();

        while let Ok(message) = self.notifications.try_recv() {
            messages.push(message);
        }

        messages
    }

    async fn assert_no_messages(&mut self) {
        let messages = self.sync().await;
        assert!(messages.is_empty(), "unexpected messages: {messages:?}");
    }

    /// Waits for the error the server logs when it catches a panic.
    async fn panic_message(&mut self) -> String {
        loop {
            let message = self.notification("window/logMessage").await;

            if message["type"] == json!(1) {
                return message["message"].as_str().unwrap().to_string();
            }
        }
    }
}

/// A temporary project directory with the given files, which is removed when dropped.
struct TempProject {
    root: PathBuf,
}

impl TempProject {
    fn new(files: &[(&str, &str)]) -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);

        let root = std::env::temp_dir().join(format!(
            "rue-lsp-protocol-{}-{}",
            std::process::id(),
            COUNTER.fetch_add(1, Ordering::SeqCst)
        ));
        fs::create_dir_all(&root).unwrap();
        let root = root.canonicalize().unwrap();

        fs::write(
            root.join("Rue.toml"),
            "[compiler]\nentrypoint = \".\"\nstd = false\n",
        )
        .unwrap();

        for (name, text) in files {
            fs::write(root.join(name), text).unwrap();
        }

        Self { root }
    }

    fn uri(&self, name: &str) -> Url {
        Url::from_file_path(self.root.join(name)).unwrap()
    }
}

impl Drop for TempProject {
    fn drop(&mut self) {
        fs::remove_dir_all(&self.root).ok();
    }
}

#[tokio::test]
async fn test_protocol_session() {
    let root = std::env::temp_dir().join(format!("rue-lsp-protocol-{}", std::process::id()));
    fs::create_dir_all(&root).unwrap();
    let root = root.canonicalize().unwrap();

    let path = root.join("main.rue");
    let text = "fn main() -> Int {\n    let value = 42;\n    value + 1\n}\n";
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

    let diagnostics = client.diagnostics(&json!(uri)).await;
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

    let diagnostics = client.diagnostics(&json!(uri)).await;
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

fn position(uri: &Url, line: u32, character: u32) -> Value {
    json!({
        "textDocument": { "uri": uri },
        "position": { "line": line, "character": character },
    })
}

/// Has a warning with a suggestion (the redundant `+`), so that there's a code action.
const SOURCE: &str = "fn main() -> Int {\n    let value = +42;\n    value + 1\n}\n";

fn injected_panics(client: &Client) -> std::sync::MutexGuard<'_, std::collections::HashSet<Task>> {
    lock(&client.backend().injected_panics)
}

#[tokio::test]
async fn test_compile_panic_is_caught() {
    let project = TempProject::new(&[("main.rue", SOURCE), ("other.rue", "")]);
    let main = project.uri("main.rue");
    let other = project.uri("other.rue");

    let mut client = Client::new();
    client.initialize().await;

    injected_panics(&client).insert(Task::Compile);
    client
        .open(&main, "fn main() -> Int {\n    \"hello\"\n}\n")
        .await;
    assert_eq!(
        client.panic_message().await,
        "Internal error while compiling: injected panic"
    );
    client.assert_no_messages().await;

    // The editor's text is used by the next compile, even though the one after it was opened
    // panicked
    injected_panics(&client).clear();
    client.open(&other, "").await;
    let diagnostics = client.diagnostics(&json!(main)).await;
    assert_eq!(diagnostics["diagnostics"].as_array().unwrap().len(), 1);

    client.change(&main, 2, &[SOURCE]).await;
    client.diagnostics(&json!(main)).await;

    let response = client
        .request("textDocument/hover", Some(position(&main, 2, 6)))
        .await;
    assert_eq!(
        response.result().unwrap()["contents"],
        json!([{ "language": "rue", "value": "let value: Int" }])
    );
}

#[tokio::test]
async fn test_query_panics_are_caught() {
    let project = TempProject::new(&[("main.rue", SOURCE)]);
    let uri = project.uri("main.rue");

    let mut client = Client::new();
    client.initialize().await;
    client.open(&uri, SOURCE).await;

    let diagnostics = client.diagnostics(&json!(uri)).await["diagnostics"].clone();
    assert_eq!(diagnostics.as_array().unwrap().len(), 1);

    let mut references = position(&uri, 2, 6);
    references["context"] = json!({ "includeDeclaration": true });

    let requests = [
        (Task::Hover, "textDocument/hover", position(&uri, 2, 6)),
        (
            Task::Definition,
            "textDocument/definition",
            position(&uri, 2, 6),
        ),
        (Task::References, "textDocument/references", references),
        (
            Task::Completion,
            "textDocument/completion",
            position(&uri, 2, 6),
        ),
        (
            Task::Formatting,
            "textDocument/formatting",
            json!({
                "textDocument": { "uri": uri },
                "options": { "tabSize": 4, "insertSpaces": true },
            }),
        ),
        (
            Task::CodeAction,
            "textDocument/codeAction",
            json!({
                "textDocument": { "uri": uri },
                "range": diagnostics[0]["range"],
                "context": { "diagnostics": diagnostics },
            }),
        ),
    ];

    for (task, method, params) in requests {
        injected_panics(&client).insert(task);

        let response = client.request(method, Some(params.clone())).await;
        assert_eq!(response.result(), Some(&Value::Null), "{method}");
        assert_eq!(
            client.panic_message().await,
            format!("Internal error while {task}: injected panic")
        );

        // The server keeps answering once the panic is gone
        injected_panics(&client).clear();

        let response = client.request(method, Some(params)).await;
        assert_ne!(response.result(), Some(&Value::Null), "{method}");
        assert!(response.is_ok(), "{method}");
    }

    client.assert_no_messages().await;
}

#[tokio::test]
async fn test_poisoned_locks_are_recovered() {
    let project = TempProject::new(&[("main.rue", SOURCE)]);
    let uri = project.uri("main.rue");

    let mut client = Client::new();
    client.initialize().await;
    client.open(&uri, SOURCE).await;
    client.diagnostics(&json!(uri)).await;

    let backend = client.backend();

    std::thread::scope(|scope| {
        scope
            .spawn(|| {
                let _cache = backend.cache.lock();
                let _file_cache = backend.file_cache.lock();
                panic!("poisoning the locks");
            })
            .join()
            .unwrap_err();
    });

    assert!(backend.cache.is_poisoned());
    assert!(backend.file_cache.is_poisoned());

    client
        .change(&uri, 2, &["fn main() -> Int {\n    \"hello\"\n}\n"])
        .await;
    let diagnostics = client.diagnostics(&json!(uri)).await;
    assert_eq!(diagnostics["diagnostics"].as_array().unwrap().len(), 1);

    client.change(&uri, 3, &[SOURCE]).await;
    client.diagnostics(&json!(uri)).await;

    let response = client
        .request("textDocument/hover", Some(position(&uri, 2, 6)))
        .await;
    assert_eq!(
        response.result().unwrap()["contents"],
        json!([{ "language": "rue", "value": "let value: Int" }])
    );
}

#[tokio::test]
async fn test_did_change_uses_last_change() {
    let project = TempProject::new(&[("main.rue", SOURCE)]);
    let uri = project.uri("main.rue");

    let mut client = Client::new();
    client.initialize().await;
    client.open(&uri, SOURCE).await;
    client.sync().await;

    // A notification without changes is ignored
    client.change(&uri, 2, &[]).await;
    client.assert_no_messages().await;

    // With several full changes, the last one is the document's text
    client
        .change(
            &uri,
            3,
            &[
                "fn main() -> Int {\n    \"hello\"\n}\n",
                "fn main() -> Int {\n    42\n}\n",
            ],
        )
        .await;
    let diagnostics = client.diagnostics(&json!(uri)).await;
    assert_eq!(diagnostics["diagnostics"], json!([]));
}
