//! Tests for compiling in the background: edits in quick succession are compiled once, requests
//! see every edit sent before them, and results that are out of date are discarded.

use std::time::{Duration, Instant};

use serde_json::json;
use tower_lsp::lsp_types::{
    HoverContents, HoverParams, MarkedString, TextDocumentIdentifier, TextDocumentPositionParams,
};

use super::protocol::{Client, MISMATCH, SOURCE, TempProject, assert_hover, position};
use super::{MANIFEST, TestProject};
use crate::lock;
use crate::worker::{DEBOUNCE, Gate};

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
    // with, so that compile's diagnostics are never published
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
}

#[test]
fn test_request_uses_newest_compile() {
    let project = TestProject::new(&[("main.rue", SOURCE)]);
    let backend = project.backend();
    let uri = project.uri("main.rue");
    let path = project.path("main.rue");

    let (started, started_receiver) = std::sync::mpsc::channel();
    let (release_sender, release) = std::sync::mpsc::channel();
    *lock(&backend.shared.gate) = Some(Gate { started, release });

    let edit = |text: &str| {
        backend.set_text(&uri, Some(text.to_string()));
        backend.worker.edit(path.clone());
    };

    let hover = std::thread::scope(|scope| {
        // The request compiles the edit first
        edit(LITERAL);

        let request = scope.spawn(|| {
            backend.on_hover(&HoverParams {
                text_document_position_params: TextDocumentPositionParams::new(
                    TextDocumentIdentifier::new(uri.clone()),
                    tower_lsp::lsp_types::Position::new(2, 6),
                ),
                work_done_progress_params: tower_lsp::lsp_types::WorkDoneProgressParams::default(),
            })
        });

        // Another edit arrives during that compile, so its diagnostics are out of date, but it's
        // still newer than the last compile, so it answers the request
        started_receiver.recv_timeout(TIMEOUT).unwrap();
        edit(MISMATCH);
        release_sender.send(()).unwrap();

        request.join().unwrap()
    });

    let HoverContents::Array(contents) = hover.unwrap().contents else {
        panic!("unexpected hover contents");
    };

    assert_eq!(
        contents,
        [MarkedString::LanguageString(
            tower_lsp::lsp_types::LanguageString {
                language: "rue".to_string(),
                value: "let value: 42".to_string(),
            }
        )]
    );
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
}
