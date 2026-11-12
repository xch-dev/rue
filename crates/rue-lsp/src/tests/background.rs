//! Tests for compiling in the background: edits in quick succession are compiled once, requests
//! see every edit sent before them, and results that are out of date are discarded.

use std::time::{Duration, Instant};

use serde_json::json;

use super::protocol::{Client, MISMATCH, SOURCE, TempProject, assert_hover, position};
use crate::worker::DEBOUNCE;

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

    // Typing quickly only compiles the project once the edits stop, with the last text
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

    let (response, ()) = tokio::join!(
        client.request("textDocument/hover", Some(position(&uri, 2, 6))),
        async { release.send(()).unwrap() },
    );
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
