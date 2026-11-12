//! End-to-end tests that drive the language server through JSON-RPC messages, the same way an
//! editor would.

use std::fs;
use std::path::PathBuf;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::Duration;

use futures::{SinkExt, StreamExt};
use serde_json::{Value, json};
use tokio::sync::mpsc;
use tower::{Service, ServiceExt};
use tower_lsp::LspService;
use tower_lsp::jsonrpc::{Request, Response};
use tower_lsp::lsp_types::Url;

use crate::{Backend, INITIALIZED, SYNCED, Task, lock};

/// How long to wait for a message from the server before failing the test, rather than hanging.
const TIMEOUT: Duration = Duration::from_secs(30);

pub(super) struct Client {
    service: LspService<Backend>,
    notifications: mpsc::UnboundedReceiver<Request>,
    next_id: i64,
}

impl Client {
    pub(super) fn new() -> Self {
        let (service, socket) = LspService::build(Backend::new)
            .custom_method("rue/sync", Backend::sync)
            .finish();
        let (sender, notifications) = mpsc::unbounded_channel();

        // The server blocks on sending messages to the client, so they must be drained
        // concurrently. Requests from the server (such as registering file watchers) succeed.
        tokio::spawn(async move {
            let (mut requests, mut responses) = socket.split();

            while let Some(message) = requests.next().await {
                if let Some(id) = message.id() {
                    let response = Response::from_ok(id.clone(), Value::Null);

                    if responses.send(response).await.is_err() {
                        break;
                    }
                }

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

    pub(super) async fn request(
        &mut self,
        method: &'static str,
        params: Option<Value>,
    ) -> Response {
        self.next_id += 1;

        let mut request = Request::build(method).id(self.next_id);

        if let Some(params) = params {
            request = request.params(params);
        }

        let request = request.finish();

        self.call(request).await.expect("expected a response")
    }

    pub(super) async fn notify(&mut self, method: &'static str, params: Value) {
        let mut request = Request::build(method);

        if !params.is_null() {
            request = request.params(params);
        }

        let request = request.finish();

        let response = self.call(request).await;
        assert!(response.is_none(), "notifications don't have responses");
    }

    async fn call(&mut self, request: Request) -> Option<Response> {
        let method = request.method().to_string();
        let response = async { self.service.ready().await.unwrap().call(request).await };

        tokio::time::timeout(TIMEOUT, response)
            .await
            .unwrap_or_else(|_| panic!("timed out waiting for the server to handle {method}"))
            .unwrap()
    }

    pub(super) fn backend(&self) -> &Backend {
        self.service.inner()
    }

    pub(super) fn inject_panic(&self, task: Option<Task>) {
        *lock(&self.backend().injected_panic) = task;
    }

    /// Sends `initialize` with the given client capabilities and `initialized`, and returns the
    /// messages the server sent in response, other than the log message saying it's initialized.
    pub(super) async fn initialize_with(&mut self, capabilities: Value) -> Vec<Request> {
        let response = self
            .request("initialize", Some(json!({ "capabilities": capabilities })))
            .await;
        assert!(response.is_ok());

        self.notify("initialized", json!({})).await;

        let mut messages = self.sync().await;
        let initialized = messages.remove(0);
        assert_eq!(initialized.params().unwrap()["message"], json!(INITIALIZED));

        messages
    }

    pub(super) async fn initialize(&mut self) {
        let messages = self.initialize_with(json!({})).await;
        assert!(messages.is_empty(), "unexpected messages: {messages:?}");
    }

    pub(super) async fn open(&mut self, uri: &Url, text: &str) {
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

    pub(super) async fn close(&mut self, uri: &Url) {
        self.notify(
            "textDocument/didClose",
            json!({ "textDocument": { "uri": uri } }),
        )
        .await;
    }

    /// Tells the server that files changed on disk, with the LSP's change types (1 is created, 2
    /// is changed and 3 is deleted).
    pub(super) async fn watched(&mut self, changes: &[(&Url, u8)]) {
        let changes = changes
            .iter()
            .map(|(uri, kind)| json!({ "uri": uri, "type": kind }))
            .collect::<Vec<_>>();

        self.notify(
            "workspace/didChangeWatchedFiles",
            json!({ "changes": changes }),
        )
        .await;
    }

    pub(super) async fn hover(&mut self, uri: &Url, line: u32, character: u32) -> Value {
        let response = self
            .request("textDocument/hover", Some(position(uri, line, character)))
            .await;
        response.result().unwrap().clone()
    }

    pub(super) async fn change(&mut self, uri: &Url, version: i32, changes: &[&str]) {
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
    pub(super) async fn diagnostics(&mut self, uri: &Url) -> Value {
        loop {
            let diagnostics = self.notification("textDocument/publishDiagnostics").await;

            if diagnostics["uri"] == json!(uri) {
                return diagnostics;
            }
        }
    }

    /// Returns the messages the server sent since the last one that was received.
    ///
    /// The server logs a message for `rue/sync` (which only exists in tests), and messages arrive
    /// in the order they were sent, so the ones before it are all of those sent for earlier
    /// notifications. This relies on the server finishing its work for a notification before it
    /// returns.
    pub(super) async fn sync(&mut self) -> Vec<Request> {
        self.notify("rue/sync", Value::Null).await;

        let mut messages = Vec::new();

        loop {
            let message = self.recv().await;

            if message.method() == "window/logMessage"
                && message.params().unwrap()["message"] == json!(SYNCED)
            {
                return messages;
            }

            messages.push(message);
        }
    }

    pub(super) async fn assert_no_messages(&mut self) {
        let messages = self.sync().await;
        assert!(messages.is_empty(), "unexpected messages: {messages:?}");
    }

    /// Returns the error the server logs when it catches a panic, which must be the only message
    /// it sent.
    pub(super) async fn panic_message(&mut self) -> String {
        let messages = self.sync().await;
        assert_eq!(messages.len(), 1, "expected one message: {messages:?}");

        let message = &messages[0];
        let params = message.params().unwrap();
        assert_eq!(message.method(), "window/logMessage");
        assert_eq!(params["type"], json!(1));

        params["message"].as_str().unwrap().to_string()
    }
}

/// A temporary project directory with the given files, which is removed when dropped.
pub(super) struct TempProject {
    root: PathBuf,
}

impl TempProject {
    pub(super) fn new(files: &[(&str, &str)]) -> Self {
        Self::with_manifest("[compiler]\nentrypoint = \".\"\nstd = false\n", files)
    }

    pub(super) fn with_manifest(manifest: &str, files: &[(&str, &str)]) -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);

        let root = std::env::temp_dir().join(format!(
            "rue-lsp-protocol-{}-{}",
            std::process::id(),
            COUNTER.fetch_add(1, Ordering::SeqCst)
        ));
        fs::create_dir_all(&root).unwrap();
        let root = root.canonicalize().unwrap();

        let project = Self { root };

        project.write("Rue.toml", manifest);

        for (name, text) in files {
            project.write(name, text);
        }

        project
    }

    pub(super) fn uri(&self, name: &str) -> Url {
        Url::from_file_path(self.root.join(name)).unwrap()
    }

    pub(super) fn write(&self, name: &str, text: &str) {
        fs::write(self.root.join(name), text).unwrap();
    }

    pub(super) fn remove(&self, name: &str) {
        fs::remove_file(self.root.join(name)).unwrap();
    }

    /// Renders the diagnostics published in the given messages as `file: [severities]`, sorted by
    /// file, since the order they're cleared in isn't defined.
    pub(super) fn published(&self, messages: &[Request]) -> Vec<String> {
        let mut published = messages
            .iter()
            .map(|message| {
                assert_eq!(message.method(), "textDocument/publishDiagnostics");

                let params = message.params().unwrap();
                let path = Url::parse(params["uri"].as_str().unwrap())
                    .unwrap()
                    .to_file_path()
                    .unwrap();
                let name = path.strip_prefix(&self.root).unwrap().display();

                let severities = params["diagnostics"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .map(|diagnostic| diagnostic["severity"].as_u64().unwrap())
                    .collect::<Vec<_>>();

                format!("{name}: {severities:?}")
            })
            .collect::<Vec<_>>();

        published.sort();
        published
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

    let diagnostics = client.diagnostics(&uri).await;
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
    assert_mismatch(&client.diagnostics(&uri).await);

    let response = client
        .request("textDocument/formatting", Some(formatting(&uri)))
        .await;
    assert_eq!(response.result().unwrap(), &json!([]));

    let response = client.request("shutdown", None).await;
    assert!(response.is_ok());
}

pub(super) fn position(uri: &Url, line: u32, character: u32) -> Value {
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
pub(super) const SOURCE: &str = "fn main() -> Int {\n    let value = +42;\n    value + 1\n}\n";

/// Has a type error, which is the only error these tests expect.
pub(super) const MISMATCH: &str = "fn main() -> Int {\n    \"hello\"\n}\n";

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
    assert_hover(&mut client, &main).await;

    // The editor's text is used by the next compile, even though compiling it panicked
    client.inject_panic(None);
    client.open(&other, "").await;
    assert_mismatch(&client.diagnostics(&main).await);
}

#[tokio::test]
async fn test_query_panics_are_caught() {
    let project = TempProject::new(&[("main.rue", SOURCE)]);
    let uri = project.uri("main.rue");

    let mut client = Client::new();
    client.initialize().await;
    client.open(&uri, SOURCE).await;

    let diagnostics = client.diagnostics(&uri).await["diagnostics"].clone();
    assert_eq!(diagnostics.as_array().unwrap().len(), 1);
    client.sync().await;

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
    client.diagnostics(&uri).await;

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
    assert_mismatch(&client.diagnostics(&uri).await);

    client.change(&uri, 3, &[SOURCE]).await;
    client.diagnostics(&uri).await;
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
    let diagnostics = client.diagnostics(&uri).await;
    assert_eq!(diagnostics["diagnostics"], json!([]));
}
