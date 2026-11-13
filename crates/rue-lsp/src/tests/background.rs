//! Tests for compiling in the background: edits in quick succession are compiled once, requests
//! see every edit sent before them, and results that are out of date are discarded.

use std::sync::atomic::Ordering;
use std::time::{Duration, Instant};

use serde_json::json;

use super::protocol::{Client, MISMATCH, SOURCE, TempProject, assert_hover, position};
use super::{MANIFEST, TestProject};
use crate::worker::DEBOUNCE;
use crate::{Task, lock};

/// Long enough for a compile that's being held to have started.
const TIMEOUT: Duration = Duration::from_secs(30);

/// Like `SOURCE`, but `value` is a literal, so its hover differs.
const LITERAL: &str = "fn main() -> Int {\n    let value = 42;\n    value + 1\n}\n";

#[tokio::test]
async fn test_edits_are_compiled_once() {
    let project = TempProject::new(&[("main.rue", SOURCE)]);
    let uri = project.uri("main.rue");

    let mut client = Client::initialized().await;
    client.open(&uri, SOURCE).await;
    client.sync().await;

    // Typing quickly only compiles the project once the edits stop, with the last text. The
    // server waits for longer than the test takes, so only syncing compiles the edits.
    client.set_debounce(TIMEOUT);
    let compiles = client.compiles();

    for (version, text) in (2..).zip([LITERAL, SOURCE, LITERAL, MISMATCH]) {
        client.change(&uri, version, &[text]).await;
    }

    assert_eq!(
        project.published(&client.sync().await),
        ["Rue.toml: []", "main.rue: [1]"]
    );
    assert_eq!(client.compiles(), compiles + 1);
}

#[tokio::test]
async fn test_edits_are_compiled_after_a_pause() {
    let project = TempProject::new(&[("main.rue", SOURCE)]);
    let uri = project.uri("main.rue");

    let mut client = Client::initialized().await;
    client.open(&uri, SOURCE).await;
    client.sync().await;

    // Nothing else is sent, so the edit is compiled once the server stops waiting for more
    let start = Instant::now();
    client.change(&uri, 2, &[MISMATCH]).await;

    let diagnostics = client.diagnostics(&uri).await;
    assert_eq!(diagnostics["diagnostics"].as_array().unwrap().len(), 1);
    assert!(start.elapsed() >= DEBOUNCE);
}

#[tokio::test]
async fn test_edit_stops_the_compile_of_an_earlier_edit() {
    let project = TempProject::new(&[("main.rue", SOURCE)]);
    let uri = project.uri("main.rue");

    let mut client = Client::initialized().await;
    client.open(&uri, SOURCE).await;
    client.sync().await;

    // The first edit's compile starts once the edits pause, and the second edit stops it
    let (started, release) = client.hold_compile();
    client.change(&uri, 2, &[MISMATCH]).await;
    started.recv_timeout(TIMEOUT).unwrap();

    client.change(&uri, 3, &[SOURCE]).await;
    release.send(()).unwrap();

    assert_eq!(
        project.published(&client.sync().await),
        ["Rue.toml: []", "main.rue: [2]"]
    );
    assert_eq!(client.cancelled(), 1);
}

#[tokio::test]
async fn test_stopped_compile_does_not_drop_files() {
    // The outer project includes `sub/x.rue`, which is also part of the inner one
    let project = TempProject::new(&[
        ("main.rue", SOURCE),
        ("sub/Rue.toml", MANIFEST),
        ("sub/x.rue", "fn x() -> Int {\n    1\n}\n"),
    ]);
    let main = project.uri("main.rue");
    let x = project.uri("sub/x.rue");

    let mut client = Client::initialized().await;
    client.open(&x, "fn x() -> Int {\n    1\n}\n").await;
    client.open(&main, SOURCE).await;
    client.sync().await;

    // The outer project's compile stops, but it hasn't stopped including `sub/x.rue`, so the
    // inner project isn't compiled and its diagnostics for it aren't published
    let compiles = client.compiles();
    let (started, release) = client.hold_compile();
    client.change(&main, 2, &[MISMATCH]).await;
    started.recv_timeout(TIMEOUT).unwrap();

    client.change(&main, 3, &[SOURCE]).await;
    release.send(()).unwrap();

    assert_eq!(
        project.published(&client.sync().await),
        ["Rue.toml: []", "main.rue: [2]", "sub/x.rue: [2]"]
    );
    assert_eq!(client.compiles(), compiles + 2);
    assert_eq!(client.cancelled(), 1);
}

#[test]
fn test_rebuild_after_a_panic_is_not_stopped() {
    let project = TestProject::new(&[(
        "main.rue",
        "fn main() -> Int {\n    let value = +42;\n    va$0lue + 1\n}\n",
    )]);
    let backend = project.backend();
    let (uri, position) = project.cursor();
    let path = project.path("main.rue");

    *lock(&backend.shared.debounce) = TIMEOUT;

    // The request panics, so the project is compiled again, and an edit arrives meanwhile
    *lock(&backend.shared.injected_panic) = Some(Task::Hover);
    let (started, release) = backend.hold_compile();

    let hover = std::thread::scope(|scope| {
        let request = scope.spawn(|| super::hover(backend, uri.clone(), position));

        started.recv_timeout(TIMEOUT).unwrap();
        backend.set_text(&uri, Some(LITERAL.to_string()));
        backend.worker.edit(path.clone());
        release.send(()).unwrap();

        request.join().unwrap()
    });

    assert_eq!(hover, "<none>\n");
    assert_eq!(backend.shared.cancelled.load(Ordering::Relaxed), 0);

    // The edit is compiled before the next request
    *lock(&backend.shared.injected_panic) = None;
    assert_eq!(super::hover(backend, uri, position), "let value: 42\n");
}

#[tokio::test]
async fn test_request_sees_pending_edit() {
    let project = TempProject::new(&[("main.rue", SOURCE)]);
    let uri = project.uri("main.rue");

    let mut client = Client::initialized().await;
    client.open(&uri, SOURCE).await;
    client.sync().await;
    assert_hover(&mut client, &uri).await;

    // The edit hasn't been compiled yet, but the request compiles it first
    client.set_debounce(TIMEOUT);
    client.change(&uri, 2, &[LITERAL]).await;
    assert_eq!(
        client.hover(&uri, 2, 6).await["contents"],
        json!([{ "language": "rue", "value": "let value: 42" }])
    );
}

#[tokio::test]
async fn test_request_waits_for_compile() {
    let project = TempProject::new(&[("main.rue", SOURCE)]);
    let uri = project.uri("main.rue");

    let mut client = Client::initialized().await;
    client.open(&uri, SOURCE).await;
    client.sync().await;

    // The request arrives while the project is being compiled with the new text, and is answered
    // once it's done
    let (started, release) = client.hold_compile();
    client.change(&uri, 2, &[LITERAL]).await;
    started.recv_timeout(TIMEOUT).unwrap();

    let mut request =
        std::pin::pin!(client.request("textDocument/hover", Some(position(&uri, 2, 6))));
    assert!(futures::poll!(&mut request).is_pending());

    release.send(()).unwrap();

    let response = request.await;
    assert_eq!(
        response.result().unwrap()["contents"],
        json!([{ "language": "rue", "value": "let value: 42" }])
    );
}

#[tokio::test]
async fn test_out_of_date_result_is_discarded() {
    let project = TempProject::new(&[("main.rue", SOURCE)]);
    let uri = project.uri("main.rue");

    let mut client = Client::initialized().await;

    // The document is edited while the project is being compiled with the text it was opened
    // with, so that compile stops, and its diagnostics are never published
    let (started, release) = client.hold_compile();
    let compiles = client.compiles();
    client.open(&uri, MISMATCH).await;
    started.recv_timeout(TIMEOUT).unwrap();

    client.change(&uri, 2, &[SOURCE]).await;
    release.send(()).unwrap();

    assert_eq!(
        project.published(&client.sync().await),
        ["Rue.toml: []", "main.rue: [2]"]
    );
    assert_eq!(client.compiles(), compiles + 2);
    assert_eq!(client.cancelled(), 1);
}

#[test]
fn test_request_uses_newest_compile() {
    let project = TestProject::new(&[(
        "main.rue",
        "fn main() -> Int {\n    let value = +42;\n    va$0lue + 1\n}\n",
    )]);
    let backend = project.backend();
    let (uri, position) = project.cursor();
    let path = project.path("main.rue");

    // Only the request compiles the edits
    *lock(&backend.shared.debounce) = TIMEOUT;
    let (started, release) = backend.hold_compile();

    let edit = |text: &str| {
        backend.set_text(&uri, Some(text.to_string()));
        backend.worker.edit(path.clone());
    };

    let hover = std::thread::scope(|scope| {
        // The request compiles the edit first
        edit(LITERAL);
        let request = scope.spawn(|| super::hover(backend, uri.clone(), position));

        // Another edit arrives during that compile, so its diagnostics are out of date, but it's
        // still newer than the last compile, so it answers the request
        started.recv_timeout(TIMEOUT).unwrap();
        edit(MISMATCH);
        release.send(()).unwrap();

        request.join().unwrap()
    });

    // The request needed that compile, so the edit didn't stop it
    assert_eq!(hover, "let value: 42\n");
    assert_eq!(backend.shared.cancelled.load(Ordering::Relaxed), 0);
}

#[tokio::test]
async fn test_cancelled_request_does_not_compile() {
    let project = TempProject::new(&[("main.rue", SOURCE)]);
    let uri = project.uri("main.rue");

    let mut client = Client::initialized().await;
    client.set_debounce(TIMEOUT);
    client.open(&uri, SOURCE).await;
    client.sync().await;

    // While a compile is running, the editor edits the file, and sends a request that it cancels
    // before the server gets to it
    let (started, release) = client.hold_compile();
    client
        .watched(&[(&uri, tower_lsp_server::ls_types::FileChangeType::CHANGED)])
        .await;
    started.recv_timeout(TIMEOUT).unwrap();

    client.change(&uri, 2, &[LITERAL]).await;

    {
        let mut request =
            std::pin::pin!(client.request("textDocument/hover", Some(position(&uri, 2, 6))));
        assert!(futures::poll!(&mut request).is_pending());
    }

    // The request doesn't compile the edit, which is still waiting for more edits
    let (started, held) = client.hold_compile();
    release.send(()).unwrap();
    assert!(started.recv_timeout(Duration::from_secs(1)).is_err());
    drop(held);
}

#[tokio::test]
async fn test_out_of_date_project_is_compiled_again() {
    // The outer project includes `sub/x.rue`, but on its own, `sub/x.rue` is part of the inner one
    let project = TempProject::new(&[
        ("main.rue", SOURCE),
        ("sub/Rue.toml", MANIFEST),
        ("sub/x.rue", "fn x() -> Int {\n    1\n}\n"),
    ]);
    let main = project.uri("main.rue");
    let x = project.uri("sub/x.rue");

    let mut client = Client::initialized().await;
    client.open(&x, "fn x() -> Int {\n    1\n}\n").await;
    client.sync().await;

    // `sub/x.rue` is edited while the outer project compiles, so its result is out of date, and
    // the edit only finds the inner project, but the outer one is compiled again too
    let (started, release) = client.hold_compile();
    client.open(&main, SOURCE).await;
    started.recv_timeout(TIMEOUT).unwrap();

    client.change(&x, 2, &["fn x() -> Int {\n    2\n}\n"]).await;
    release.send(()).unwrap();

    let published = project.published(&client.sync().await);
    assert!(
        published.contains(&"main.rue: [2]".to_string()),
        "{published:?}"
    );

    // The outer project's compile stopped, since it includes the edited file
    assert_eq!(client.cancelled(), 1);
}

#[tokio::test]
async fn test_other_projects_are_not_out_of_date() {
    let project = TempProject::with_manifest(
        None,
        &[
            ("a/Rue.toml", MANIFEST),
            ("a/main.rue", SOURCE),
            ("b/Rue.toml", MANIFEST),
            ("b/main.rue", MISMATCH),
        ],
    );
    let a = project.uri("a/main.rue");
    let b = project.uri("b/main.rue");

    let mut client = Client::initialized().await;
    client.open(&a, SOURCE).await;
    client.sync().await;

    // An edit to another project doesn't make this one's compile out of date
    let compiles = client.compiles();
    let (started, release) = client.hold_compile();
    client.open(&b, MISMATCH).await;
    started.recv_timeout(TIMEOUT).unwrap();

    client.change(&a, 2, &[MISMATCH]).await;
    release.send(()).unwrap();

    assert_eq!(
        project.published(&client.sync().await),
        [
            "a/Rue.toml: []",
            "a/main.rue: [1]",
            "b/Rue.toml: []",
            "b/main.rue: [1]"
        ]
    );
    assert_eq!(client.compiles(), compiles + 2);
    assert_eq!(client.cancelled(), 0);
}

#[tokio::test]
async fn test_open_compiles_pending_edits() {
    let project = TempProject::new(&[("main.rue", SOURCE), ("other.rue", "")]);
    let main = project.uri("main.rue");
    let other = project.uri("other.rue");

    let mut client = Client::initialized().await;
    client.set_debounce(TIMEOUT);
    client.open(&main, SOURCE).await;
    client.sync().await;

    // Opening another file compiles the project once, with the edit that was waiting
    let compiles = client.compiles();
    client.change(&main, 2, &[MISMATCH]).await;
    client.open(&other, "").await;

    let diagnostics = client.diagnostics(&main).await;
    assert_eq!(diagnostics["diagnostics"][0]["severity"], json!(1));

    // The edit isn't compiled again
    client.sync().await;
    assert_eq!(client.compiles(), compiles + 1);
}

#[test]
fn test_request_for_older_text_is_not_answered() {
    let project = TestProject::new(&[(
        "main.rue",
        "fn main() -> Int {\n    let value = +42;\n    va$0lue + 1\n}\n",
    )]);
    let backend = project.backend();
    let (uri, position) = project.cursor();
    let path = project.path("main.rue");

    *lock(&backend.shared.debounce) = TIMEOUT;

    let edit = |text: &str| {
        backend.set_text(&uri, Some(text.to_string()));
        backend.worker.edit(path.clone());
    };

    // The worker is busy when the request is sent
    let (started, release) = backend.hold_compile();
    drop(backend.worker.compile(Vec::new(), vec![path.clone()]));
    started.recv_timeout(TIMEOUT).unwrap();

    let hover = std::thread::scope(|scope| {
        edit(LITERAL);
        let request = scope.spawn(|| super::hover(backend, uri.clone(), position));

        // The request was sent for the text with `42`, but by the time the worker gets to it,
        // the text has changed again, so the position may not match anymore
        let deadline = Instant::now() + TIMEOUT;

        while backend.shared.queries.load(Ordering::Relaxed) == 0 {
            assert!(Instant::now() < deadline, "the request wasn't sent");
            std::thread::yield_now();
        }

        edit(SOURCE);
        release.send(()).unwrap();

        request.join().unwrap()
    });

    assert_eq!(hover, "<none>\n");
}

/// Binds a value that's never used, which the `unused_binding` lint reports.
const UNUSED: &str = "fn main() -> Int {\n    let unused = 1;\n    2\n}\n";

#[tokio::test]
async fn test_compilers_are_reused_for_the_same_options() {
    let warn = TempProject::new(&[("main.rue", UNUSED)]);
    let deny = TempProject::with_manifest(
        Some(&format!("{MANIFEST}\n[lints]\nunused_binding = \"deny\"\n")),
        &[("main.rue", UNUSED)],
    );
    let warn_uri = warn.uri("main.rue");
    let deny_uri = deny.uri("main.rue");

    let mut client = Client::initialized().await;
    client.open(&warn_uri, UNUSED).await;
    assert_eq!(
        warn.published(&client.sync().await),
        ["Rue.toml: []", "main.rue: [2]"]
    );
    assert_eq!(client.compilers(), 1);

    // Compiling the project again starts from a copy of the compiler built the first time
    client.change(&warn_uri, 2, &[MISMATCH]).await;
    assert_eq!(
        warn.published(&client.sync().await),
        ["Rue.toml: []", "main.rue: [1]"]
    );
    assert_eq!(client.compilers(), 1);

    // A project whose options differ only in its lint levels gets a compiler of its own, so the
    // levels of the other project don't leak into it
    client.open(&deny_uri, UNUSED).await;
    assert_eq!(
        deny.published(&client.sync().await),
        ["Rue.toml: []", "main.rue: [1]"]
    );
    assert_eq!(client.compilers(), 2);

    client.change(&warn_uri, 3, &[UNUSED]).await;
    assert_eq!(
        warn.published(&client.sync().await),
        ["Rue.toml: []", "main.rue: [2]"]
    );
    assert_eq!(client.compilers(), 2);
}

#[tokio::test]
async fn test_standard_library_survives_copying_the_compiler() {
    let source = "fn main() -> List<Int> {\n    merge_list([1, 2], [3])\n}\n";
    let edited = "fn main() -> List<Int> {\n    merge_list([1], [2, 3])\n}\n";
    let project = TempProject::with_manifest(
        Some("[compiler]\nentrypoint = \".\"\n"),
        &[("main.rue", source)],
    );
    let uri = project.uri("main.rue");
    let hover = json!([{
        "language": "rue",
        "value": "fn merge_list<T>(a: List<T>, b: List<T>) -> List<T>"
    }]);

    let mut client = Client::initialized().await;
    client.open(&uri, source).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["Rue.toml: []", "main.rue: []"]
    );
    assert_eq!(client.hover(&uri, 1, 8).await["contents"], hover);

    // The second compile starts from a copy of the first compiler, which still has the standard
    // library in it
    client.change(&uri, 2, &[edited]).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["Rue.toml: []", "main.rue: []"]
    );
    assert_eq!(client.hover(&uri, 1, 8).await["contents"], hover);
    assert_eq!(client.compilers(), 1);
}
