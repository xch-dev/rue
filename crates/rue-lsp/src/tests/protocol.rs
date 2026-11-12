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

const INITIALIZED: &str = "Rue language server initialized.";

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

    fn inject_panic(&self, task: Option<Task>) {
        *lock(&self.backend().injected_panic) = task;
    }

    /// Sends `initialize` and `initialized`, and skips the log message that follows.
    async fn initialize(&mut self) {
        let response = self
            .request("initialize", Some(json!({ "capabilities": {} })))
            .await;
        assert!(response.is_ok());

        self.notify("initialized", json!({})).await;
        let message = self.notification("window/logMessage").await;
        assert_eq!(message["message"], json!(INITIALIZED));
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

    async fn recv(&mut self) -> Request {
        tokio::time::timeout(TIMEOUT, self.notifications.recv())
            .await
            .expect("timed out waiting for a message from the server")
            .expect("server closed")
    }

    async fn notification(&mut self, method: &str) -> Value {
        loop {
            let message = self.recv().await;

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

    /// Returns the messages the server sent since the last one that was received.
    async fn sync(&mut self) -> Vec<Request> {
        // The server logs a message for `initialized`, and messages arrive in the order they were
        // sent, so the ones before it are all of those sent for earlier notifications
        self.notify("initialized", json!({})).await;

        let mut messages = Vec::new();

        loop {
            let message = self.recv().await;

            if message.method() == "window/logMessage"
                && message.params().unwrap()["message"] == json!(INITIALIZED)
            {
                return messages;
            }

            messages.push(message);
        }
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
    let text = "fn main() -> Int {\n    let value = 42;\n    value + 1\n}\n";
    let project = TempProject::new(&[("main.rue", text)]);
    let uri = project.uri("main.rue");

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

    client.open(&uri, text).await;

    let diagnostics = client.diagnostics(&json!(uri)).await;
    assert_eq!(diagnostics["diagnostics"], json!([]));

    let response = client
        .request("textDocument/hover", Some(position(&uri, 2, 6)))
        .await;
    assert_eq!(
        response.result().unwrap()["contents"],
        json!([{ "language": "rue", "value": "let value: 42" }])
    );

    let response = client
        .request("textDocument/definition", Some(position(&uri, 2, 6)))
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

    client.change(&uri, 2, &[MISMATCH]).await;
    assert_mismatch(&client.diagnostics(&json!(uri)).await);

    let response = client
        .request("textDocument/formatting", Some(formatting(&uri)))
        .await;
    assert_eq!(response.result().unwrap(), &json!([]));

    let response = client.request("shutdown", None).await;
    assert!(response.is_ok());
}

fn position(uri: &Url, line: u32, character: u32) -> Value {
    json!({
        "textDocument": { "uri": uri },
        "position": { "line": line, "character": character },
    })
}

fn formatting(uri: &Url) -> Value {
    json!({
        "textDocument": { "uri": uri },
        "options": { "tabSize": 4, "insertSpaces": true },
    })
}

/// Has a warning with a suggestion (the redundant `+`), so that there's a code action.
const SOURCE: &str = "fn main() -> Int {\n    let value = +42;\n    value + 1\n}\n";

/// Has a type error, which is the only error these tests expect.
const MISMATCH: &str = "fn main() -> Int {\n    \"hello\"\n}\n";

/// Asserts that the only diagnostic is the type error in `MISMATCH`.
fn assert_mismatch(diagnostics: &Value) {
    let diagnostics = diagnostics["diagnostics"].as_array().unwrap();
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0]["severity"], json!(1));
    assert_eq!(
        diagnostics[0]["range"]["start"],
        json!({ "line": 0, "character": 17 })
    );
}

async fn assert_hover(client: &mut Client, uri: &Url) {
    let response = client
        .request("textDocument/hover", Some(position(uri, 2, 6)))
        .await;
    assert_eq!(
        response.result().unwrap()["contents"],
        json!([{ "language": "rue", "value": "let value: Int" }])
    );
}

#[tokio::test]
async fn test_compile_panic_is_caught() {
    let project = TempProject::new(&[("main.rue", SOURCE), ("other.rue", "")]);
    let main = project.uri("main.rue");
    let other = project.uri("other.rue");

    let mut client = Client::new();
    client.initialize().await;
    client.open(&main, SOURCE).await;
    client.sync().await;

    client.inject_panic(Some(Task::Compile));
    client.change(&main, 2, &[MISMATCH]).await;
    assert_eq!(
        client.panic_message().await,
        "Internal error while compiling: injected panic"
    );

    // The results of the last compile are kept
    client.assert_no_messages().await;
    assert_hover(&mut client, &main).await;

    // The editor's text is used by the next compile, even though compiling it panicked
    client.inject_panic(None);
    client.open(&other, "").await;
    assert_mismatch(&client.diagnostics(&json!(main)).await);
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
            formatting(&uri),
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
        client.inject_panic(Some(task));

        let response = client.request(method, Some(params.clone())).await;
        assert_eq!(response.result(), Some(&Value::Null), "{method}");
        assert_eq!(
            client.panic_message().await,
            format!("Internal error while {task}: injected panic")
        );

        // The server keeps answering once the panic is gone
        client.inject_panic(None);

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

    client.change(&uri, 2, &[MISMATCH]).await;
    assert_mismatch(&client.diagnostics(&json!(uri)).await);

    client.change(&uri, 3, &[SOURCE]).await;
    client.diagnostics(&json!(uri)).await;
    assert_hover(&mut client, &uri).await;
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
        .change(&uri, 3, &[MISMATCH, "fn main() -> Int {\n    42\n}\n"])
        .await;
    let diagnostics = client.diagnostics(&json!(uri)).await;
    assert_eq!(diagnostics["diagnostics"], json!([]));
}
