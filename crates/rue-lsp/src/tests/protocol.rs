//! End-to-end tests that drive the language server through JSON-RPC messages, the same way an
//! editor would.

use std::fs;
use std::path::PathBuf;
use std::sync::atomic::Ordering;
use std::time::Duration;

use futures::{SinkExt, StreamExt};
use serde_json::{Value, json};
use tokio::sync::mpsc;
use tower::{Service, ServiceExt};
use tower_lsp::LspService;
use tower_lsp::jsonrpc::{Request, Response};
use tower_lsp::lsp_types::{FileChangeType, MessageType, Url};

use super::{MANIFEST, TempDir};
use crate::{Backend, INITIALIZED, Task, lock};

/// Logged for the `rue/sync` notification, which only exists in tests, so that they know that the
/// server has handled everything sent before it.
const SYNCED: &str = "Synced.";

impl Backend {
    async fn sync(&self) {
        self.client.log_message(MessageType::INFO, SYNCED).await;
    }
}

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

    async fn request(&mut self, method: &'static str, params: Option<Value>) -> Response {
        self.next_id += 1;

        let mut request = Request::build(method).id(self.next_id);

        if let Some(params) = params {
            request = request.params(params);
        }

        let request = request.finish();

        self.call(request).await.expect("expected a response")
    }

    async fn notify(&mut self, method: &'static str, params: Value) {
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

    fn backend(&self) -> &Backend {
        self.service.inner()
    }

    /// How many times the server compiled a project.
    pub(super) fn compiles(&self) -> usize {
        self.backend().compiles.load(Ordering::Relaxed)
    }

    pub(super) fn inject_panic(&self, task: Option<Task>) {
        *lock(&self.backend().injected_panic) = task;
    }

    /// Sends `initialize` with the given parameters and `initialized`, and returns the messages
    /// the server sent in response, other than the log message saying it's initialized.
    pub(super) async fn initialize_with(&mut self, params: Value) -> Vec<Request> {
        let response = self.request("initialize", Some(params)).await;
        assert!(response.is_ok());

        self.notify("initialized", json!({})).await;

        let mut messages = self.sync().await;
        let initialized = messages.remove(0);
        assert_eq!(initialized.params().unwrap()["message"], json!(INITIALIZED));

        messages
    }

    /// A client that has initialized the server, with no capabilities.
    pub(super) async fn initialized() -> Self {
        let mut client = Self::new();
        let messages = client.initialize_with(json!({ "capabilities": {} })).await;
        assert!(messages.is_empty(), "unexpected messages: {messages:?}");
        client
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

    /// Tells the server that files changed on disk.
    pub(super) async fn watched(&mut self, changes: &[(&Url, FileChangeType)]) {
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
    async fn diagnostics(&mut self, uri: &Url) -> Value {
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
    _dir: TempDir,
}

impl TempProject {
    pub(super) fn new(files: &[(&str, &str)]) -> Self {
        Self::with_manifest(Some(MANIFEST), files)
    }

    pub(super) fn with_manifest(manifest: Option<&str>, files: &[(&str, &str)]) -> Self {
        let dir = TempDir::new("rue-lsp-protocol");

        let project = Self {
            root: dir.path().to_path_buf(),
            _dir: dir,
        };

        if let Some(manifest) = manifest {
            project.write("Rue.toml", manifest);
        }

        for (name, text) in files {
            project.write(name, text);
        }

        project
    }

    pub(super) fn uri(&self, name: &str) -> Url {
        Url::from_file_path(self.root.join(name)).unwrap()
    }

    pub(super) fn write(&self, name: &str, text: impl AsRef<[u8]>) {
        let path = self.root.join(name);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, text).unwrap();
    }

    /// Creates a symlink at `name` to `target`, relative to the symlink's directory.
    #[cfg(unix)]
    pub(super) fn symlink(&self, name: &str, target: &str) {
        let path = self.root.join(name);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::os::unix::fs::symlink(target, path).unwrap();
    }

    /// Removes a file or a directory.
    pub(super) fn remove(&self, name: &str) {
        let path = self.root.join(name);

        if path.is_dir() {
            fs::remove_dir_all(path).unwrap();
        } else {
            fs::remove_file(path).unwrap();
        }
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

    assert_eq!(
        client.hover(&uri, 2, 6).await["contents"],
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
pub(super) const SOURCE: &str = "fn main() -> Int {\n    let value = +42;\n    value + 1\n}\n";

/// Has a type error, which is the only error these tests expect.
pub(super) const MISMATCH: &str = "fn main() -> Int {\n    \"hello\"\n}\n";

/// Declares an unused function (a warning) with a type error.
const OTHER_MISMATCH: &str = "fn other() -> Int {\n    \"hello\"\n}\n";

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
    assert_eq!(
        client.hover(uri, 2, 6).await["contents"],
        json!([{ "language": "rue", "value": "let value: Int" }])
    );
}

#[tokio::test]
async fn test_compile_panic_is_caught() {
    let project = TempProject::new(&[("main.rue", SOURCE), ("other.rue", "")]);
    let main = project.uri("main.rue");
    let other = project.uri("other.rue");

    let mut client = Client::initialized().await;
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

    let mut client = Client::initialized().await;
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

    let mut client = Client::initialized().await;
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

    let mut client = Client::initialized().await;
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

#[tokio::test]
async fn test_uri_spelling_is_kept() {
    let project = TempProject::new(&[("main.rue", SOURCE)]);

    // The same file, with a letter percent-encoded, as some clients do with other characters
    let uri = project.uri("main.rue");
    let uri = Url::parse(&uri.as_str().replace("main.rue", "%6Dain.rue")).unwrap();

    let mut client = Client::initialized().await;
    client.open(&uri, SOURCE).await;

    // The diagnostics are only published to the URI the editor used
    assert_eq!(
        published_uris(&client.sync().await),
        uri_entries(&[(&uri, &[2]), (&project.uri("Rue.toml"), &[])])
    );

    assert_hover(&mut client, &uri).await;

    let response = client
        .request("textDocument/definition", Some(position(&uri, 2, 6)))
        .await;
    assert_eq!(response.result().unwrap()["uri"], json!(uri));
}

#[tokio::test]
async fn test_workspace_folder_paths() {
    // The project's entrypoint is outside of its directory, but inside the workspace, which
    // clients send as workspace folders or (in older clients) as the root URI
    for root in ["workspaceFolders", "rootUri"] {
        let project = TempProject::with_manifest(
            None,
            &[
                (
                    "proj/Rue.toml",
                    "[compiler]\nentrypoint = \"../lib\"\nstd = false\n",
                ),
                ("lib/main.rue", MISMATCH),
            ],
        );
        let manifest = project.uri("proj/Rue.toml");

        let mut params = json!({ "capabilities": {} });
        params[root] = if root == "rootUri" {
            json!(project.uri(""))
        } else {
            json!([{ "uri": project.uri(""), "name": "workspace" }])
        };

        let mut client = Client::new();
        let messages = client.initialize_with(params).await;
        assert!(messages.is_empty(), "{messages:?}");

        client
            .open(
                &manifest,
                "[compiler]\nentrypoint = \"../lib\"\nstd = false\n",
            )
            .await;
        assert_eq!(
            project.published(&client.sync().await),
            ["lib/main.rue: [1]", "proj/Rue.toml: []"],
            "{root}"
        );
    }
}

/// Renders the URIs that diagnostics were published to, with their severities, sorted.
fn published_uris(messages: &[Request]) -> Vec<String> {
    let mut published = messages
        .iter()
        .map(|message| {
            let params = message.params().unwrap();
            let severities = params["diagnostics"]
                .as_array()
                .unwrap()
                .iter()
                .map(|diagnostic| diagnostic["severity"].as_u64().unwrap())
                .collect::<Vec<_>>();
            format!("{} {severities:?}", params["uri"].as_str().unwrap())
        })
        .collect::<Vec<_>>();

    published.sort();
    published
}

/// Renders URIs and severities like `published_uris`.
fn uri_entries(entries: &[(&Url, &[u64])]) -> Vec<String> {
    let mut entries = entries
        .iter()
        .map(|(uri, severities)| format!("{uri} {severities:?}"))
        .collect::<Vec<_>>();

    entries.sort();
    entries
}

#[cfg(unix)]
#[tokio::test]
async fn test_file_open_through_two_uris() {
    let project = TempProject::new(&[("main.rue", SOURCE)]);
    let link = project.uri("main.rue");
    let real = Url::from_file_path(project.root.canonicalize().unwrap().join("main.rue")).unwrap();
    let manifest = project.uri("Rue.toml");

    let mut client = Client::initialized().await;

    // The unsaved text has a type error, and the file on disk has a warning
    client.open(&link, MISMATCH).await;
    assert_eq!(
        published_uris(&client.sync().await),
        uri_entries(&[(&link, &[1]), (&manifest, &[])])
    );

    // Both documents get the diagnostics
    client.open(&real, MISMATCH).await;
    assert_eq!(
        published_uris(&client.sync().await),
        uri_entries(&[(&link, &[1]), (&real, &[1]), (&manifest, &[])])
    );

    // Once one is closed, its diagnostics are cleared, and the other one's text is still used
    client.close(&link).await;
    assert_eq!(
        published_uris(&client.sync().await),
        uri_entries(&[(&link, &[]), (&real, &[1]), (&manifest, &[])])
    );

    client
        .change(&real, 2, &["fn main() -> Int {\n    42\n}\n"])
        .await;
    assert_eq!(
        published_uris(&client.sync().await),
        uri_entries(&[(&real, &[]), (&manifest, &[])])
    );

    // Once neither is open, the file on disk is compiled, and its diagnostics go to the path the
    // client used for the project
    client.close(&real).await;
    assert_eq!(
        published_uris(&client.sync().await),
        uri_entries(&[(&link, &[2]), (&real, &[]), (&manifest, &[])])
    );
}

#[cfg(unix)]
#[tokio::test]
async fn test_shared_manifest_symlink() {
    // Two projects share their configuration through a symlinked manifest
    let project = TempProject::with_manifest(
        None,
        &[
            ("shared/Rue.toml", MANIFEST),
            ("a/main.rue", MISMATCH),
            ("b/main.rue", MISMATCH),
        ],
    );
    project.symlink("a/Rue.toml", "../shared/Rue.toml");
    project.symlink("b/Rue.toml", "../shared/Rue.toml");

    let a = project.uri("a/main.rue");
    let b = project.uri("b/main.rue");

    let mut client = Client::initialized().await;

    client.open(&a, MISMATCH).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["a/Rue.toml: []", "a/main.rue: [1]"]
    );

    // They're separate projects, so neither clears the other's diagnostics. The manifest's
    // diagnostics are published to both of the paths the client uses for it.
    client.open(&b, MISMATCH).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["a/Rue.toml: []", "b/Rue.toml: []", "b/main.rue: [1]"]
    );
    assert_ne!(client.hover(&a, 0, 4).await, Value::Null);
}

#[cfg(unix)]
#[tokio::test]
async fn test_project_found_through_symlink() {
    // `outer/sub` is part of the project in `outer`, but through `side/link` it's on its own
    let project = TempProject::with_manifest(
        None,
        &[
            ("outer/Rue.toml", MANIFEST),
            ("outer/sub/x.rue", OTHER_MISMATCH),
            ("side/y.rue", OTHER_MISMATCH),
        ],
    );
    project.symlink("side/link", "../outer/sub");

    let x = project.uri("side/link/x.rue");
    let y = project.uri("side/y.rue");

    let mut client = Client::initialized().await;

    client.open(&x, OTHER_MISMATCH).await;
    client.close(&x).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["side/link/x.rue: [2, 1]", "side/link/x.rue: [2, 1]"]
    );

    // It's still the same project when it's looked up again, so it's left alone
    let compiles = client.compiles();
    client.open(&y, OTHER_MISMATCH).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["side/y.rue: [2, 1]"]
    );
    assert_eq!(client.compiles(), compiles + 1);
}

#[cfg(unix)]
#[tokio::test]
async fn test_watched_files_through_canonical_path() {
    let project = TempProject::new(&[("main.rue", SOURCE)]);
    let main = project.uri("main.rue");
    let real = project.root.canonicalize().unwrap();

    let mut client = Client::initialized().await;

    client.open(&main, SOURCE).await;
    client.sync().await;

    // The watcher reports the file's real path, but it's the same project
    project.write("other.rue", MISMATCH.replace("main", "other"));
    let other = Url::from_file_path(real.join("other.rue")).unwrap();
    client.watched(&[(&other, FileChangeType::CREATED)]).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["Rue.toml: []", "main.rue: [2]", "other.rue: [2, 1]"]
    );
}
