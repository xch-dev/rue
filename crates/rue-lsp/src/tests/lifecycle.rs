//! Tests for how the server keeps its state and the editor's diagnostics up to date as documents
//! are opened and closed, and as files change on disk.

use serde_json::{Value, json};

use super::protocol::{Client, MISMATCH, SOURCE, TempProject};

const CREATED: u8 = 1;
const CHANGED: u8 = 2;
const DELETED: u8 = 3;

/// Declares `other`, which is unused (a warning) and has a type error.
const OTHER: &str = "fn other() -> Int {\n    \"hello\"\n}\n";

async fn client() -> Client {
    let mut client = Client::new();
    client.initialize().await;
    client
}

#[tokio::test]
async fn test_close_reverts_to_disk() {
    let project = TempProject::new(&[("main.rue", SOURCE)]);
    let main = project.uri("main.rue");

    let mut client = client().await;

    client.open(&main, MISMATCH).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["Rue.toml: []", "main.rue: [1]"]
    );

    // The unsaved type error is discarded, and the warning in the file on disk is reported
    client.close(&main).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["Rue.toml: []", "main.rue: [2]"]
    );
}

#[tokio::test]
async fn test_deleted_file_is_cleared() {
    let project = TempProject::new(&[("main.rue", SOURCE), ("other.rue", OTHER)]);
    let main = project.uri("main.rue");
    let other = project.uri("other.rue");

    let mut client = client().await;

    client.open(&main, SOURCE).await;
    let published = project.published(&client.sync().await);
    assert!(
        published.contains(&"other.rue: [2, 1]".to_string()),
        "{published:?}"
    );
    assert_ne!(client.hover(&other, 0, 4).await, Value::Null);

    // The file isn't part of the project anymore, so its diagnostics are cleared, and it can't be
    // queried
    project.remove("other.rue");
    client.watched(&[(&other, DELETED)]).await;
    let published = project.published(&client.sync().await);
    assert!(
        published.contains(&"other.rue: []".to_string()),
        "{published:?}"
    );
    assert_eq!(client.hover(&other, 0, 4).await, Value::Null);
    assert_ne!(client.hover(&main, 2, 6).await, Value::Null);
}

#[tokio::test]
async fn test_uncompilable_project_is_cleared() {
    let project = TempProject::with_manifest(
        "[compiler]\nentrypoint = \"main.rue\"\nstd = false\n",
        &[("main.rue", MISMATCH)],
    );
    let main = project.uri("main.rue");

    let mut client = client().await;

    client.open(&main, MISMATCH).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["Rue.toml: []", "main.rue: [1]"]
    );

    // Without its entrypoint, the project can't be compiled at all
    project.remove("main.rue");
    client.watched(&[(&main, DELETED)]).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["Rue.toml: []", "main.rue: []"]
    );
    assert_eq!(client.hover(&main, 1, 4).await, Value::Null);
}

#[tokio::test]
async fn test_watched_files_recompile() {
    let project = TempProject::new(&[("main.rue", SOURCE)]);
    let main = project.uri("main.rue");
    let other = project.uri("other.rue");
    let manifest = project.uri("Rue.toml");

    let mut client = client().await;

    client.open(&main, SOURCE).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["Rue.toml: []", "main.rue: [2]"]
    );

    // A file created outside of the editor is compiled, and the project is only compiled once for
    // all of the changes
    project.write("other.rue", OTHER);
    client.watched(&[(&other, CREATED), (&main, CHANGED)]).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["Rue.toml: []", "main.rue: [2]", "other.rue: [2, 1]"]
    );

    // So is a change to the manifest
    project.write(
        "Rue.toml",
        "[compiler]\nentrypoint = \".\"\nstd = false\n\n[lints]\nunnecessary_plus = \"allow\"\n",
    );
    client.watched(&[(&manifest, CHANGED)]).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["Rue.toml: []", "main.rue: []", "other.rue: [2, 1]"]
    );
}

#[tokio::test]
async fn test_deleted_manifest_clears_project() {
    // Without a manifest or a `main.rue`, the directory isn't a project
    let project = TempProject::new(&[("lib.rue", OTHER)]);
    let lib = project.uri("lib.rue");
    let manifest = project.uri("Rue.toml");

    let mut client = client().await;

    client.open(&lib, OTHER).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["Rue.toml: []", "lib.rue: [2, 1]"]
    );

    project.remove("Rue.toml");
    client.watched(&[(&manifest, DELETED)]).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["Rue.toml: []", "lib.rue: []"]
    );
    assert_eq!(client.hover(&lib, 0, 4).await, Value::Null);
}

#[tokio::test]
async fn test_registers_file_watchers() {
    let mut client = Client::new();

    let messages = client
        .initialize_with(json!({
            "workspace": { "didChangeWatchedFiles": { "dynamicRegistration": true } },
        }))
        .await;

    assert_eq!(messages.len(), 1, "{messages:?}");
    assert_eq!(messages[0].method(), "client/registerCapability");
    assert_eq!(
        messages[0].params().unwrap()["registrations"],
        json!([{
            "id": "rue-watched-files",
            "method": "workspace/didChangeWatchedFiles",
            "registerOptions": {
                "watchers": [
                    { "globPattern": "**/*.rue" },
                    { "globPattern": "**/Rue.toml" },
                ],
            },
        }])
    );
}
