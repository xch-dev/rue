//! Tests for how the server keeps its state and the editor's diagnostics up to date as documents
//! are opened and closed, and as files change on disk.

use serde_json::{Value, json};
use tower_lsp::lsp_types::Url;

use super::protocol::{Client, MISMATCH, SOURCE, TempProject};
use crate::Task;

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
async fn test_close_after_delete_discards_text() {
    let project = TempProject::new(&[("main.rue", SOURCE), ("other.rue", OTHER)]);
    let other = project.uri("other.rue");

    let mut client = client().await;

    // The unsaved text fixes the type error
    client
        .open(&other, "fn other() -> Int {\n    42\n}\n")
        .await;
    assert_eq!(
        project.published(&client.sync().await),
        ["Rue.toml: []", "main.rue: [2]", "other.rue: [2]"]
    );

    // Switching branches deletes the file, and then the editor closes it
    project.remove("other.rue");
    client.watched(&[(&other, DELETED)]).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["Rue.toml: []", "main.rue: [2]", "other.rue: []"]
    );

    client.close(&other).await;
    client.sync().await;

    // Once the file is back, what's on disk is compiled, not the text the editor had
    project.write("other.rue", OTHER);
    client.watched(&[(&other, CREATED)]).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["Rue.toml: []", "main.rue: [2]", "other.rue: [2, 1]"]
    );
}

#[tokio::test]
async fn test_deleted_file_is_cleared() {
    let project = TempProject::new(&[("main.rue", SOURCE), ("other.rue", OTHER)]);
    let main = project.uri("main.rue");
    let other = project.uri("other.rue");

    let mut client = client().await;

    client.open(&main, SOURCE).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["Rue.toml: []", "main.rue: [2]", "other.rue: [2, 1]"]
    );
    assert_ne!(client.hover(&other, 0, 4).await, Value::Null);

    // The file isn't part of the project anymore, so its diagnostics are cleared, and it can't be
    // queried
    project.remove("other.rue");
    client.watched(&[(&other, DELETED)]).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["Rue.toml: []", "main.rue: [2]", "other.rue: []"]
    );
    assert_eq!(client.hover(&other, 0, 4).await, Value::Null);
    assert_ne!(client.hover(&main, 2, 6).await, Value::Null);
}

#[tokio::test]
async fn test_uncompilable_project_is_cleared() {
    let project = TempProject::with_manifest(
        Some("[compiler]\nentrypoint = \"main.rue\"\nstd = false\n"),
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
    assert_eq!(client.hover(&main, 0, 4).await, Value::Null);

    // Once it's back, so are the diagnostics
    project.write("main.rue", MISMATCH);
    client.watched(&[(&main, CREATED)]).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["Rue.toml: []", "main.rue: [1]"]
    );
    assert_ne!(client.hover(&main, 0, 4).await, Value::Null);
}

#[tokio::test]
async fn test_invalid_manifest_clears_project() {
    let project = TempProject::new(&[("main.rue", SOURCE), ("other.rue", OTHER)]);
    let main = project.uri("main.rue");
    let other = project.uri("other.rue");
    let manifest = project.uri("Rue.toml");

    let mut client = client().await;

    client.open(&main, SOURCE).await;
    client.sync().await;

    project.write("Rue.toml", "[compiler\n");
    client.watched(&[(&manifest, CHANGED)]).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["Rue.toml: [1]", "main.rue: []", "other.rue: []"]
    );
    assert_eq!(client.hover(&other, 0, 4).await, Value::Null);

    project.write("Rue.toml", "[compiler]\nentrypoint = \".\"\nstd = false\n");
    client.watched(&[(&manifest, CHANGED)]).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["Rue.toml: []", "main.rue: [2]", "other.rue: [2, 1]"]
    );
    assert_ne!(client.hover(&other, 0, 4).await, Value::Null);
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
    let compiles = client.compiles();
    project.write("other.rue", OTHER);
    client.watched(&[(&other, CREATED), (&main, CHANGED)]).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["Rue.toml: []", "main.rue: [2]", "other.rue: [2, 1]"]
    );
    assert_eq!(client.compiles(), compiles + 1);

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

    // Files that aren't part of a project are ignored
    let compiles = client.compiles();
    let outside =
        Url::from_file_path(std::env::temp_dir().join("rue-lsp-nonexistent/main.rue")).unwrap();
    let untitled = Url::parse("untitled:Untitled-1").unwrap();
    client
        .watched(&[(&outside, CHANGED), (&untitled, CHANGED)])
        .await;
    client.assert_no_messages().await;
    assert_eq!(client.compiles(), compiles);
}

#[tokio::test]
async fn test_deleted_manifest_leaves_project() {
    // Without the manifest, `lib.rue` is compiled on its own, since there's no `main.rue`
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
        ["Rue.toml: []", "lib.rue: [2, 1]"]
    );
    assert_ne!(client.hover(&lib, 0, 4).await, Value::Null);
}

#[tokio::test]
async fn test_deleted_manifest_in_any_order() {
    for manifest_first in [false, true] {
        let project = TempProject::new(&[("main.rue", SOURCE)]);
        let main = project.uri("main.rue");
        let manifest = project.uri("Rue.toml");

        let mut client = client().await;

        client.open(&main, SOURCE).await;
        client.sync().await;

        // Without the manifest, the directory is a project because it has a `main.rue`
        project.remove("Rue.toml");

        let mut changes = [(&main, CHANGED), (&manifest, DELETED)];

        if manifest_first {
            changes.reverse();
        }

        client.watched(&changes).await;
        assert_eq!(
            project.published(&client.sync().await),
            ["Rue.toml: []", "main.rue: [2]"]
        );
        assert_ne!(client.hover(&main, 2, 6).await, Value::Null);
    }
}

#[tokio::test]
async fn test_deleted_entrypoint_without_manifest() {
    let project = TempProject::with_manifest(None, &[("main.rue", SOURCE), ("other.rue", OTHER)]);
    let main = project.uri("main.rue");
    let other = project.uri("other.rue");

    let mut client = client().await;

    client.open(&other, OTHER).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["main.rue: [2]", "other.rue: [2, 1]"]
    );

    // Without `main.rue`, the directory isn't a project anymore, so `other.rue` is compiled on its
    // own
    project.remove("main.rue");
    client.watched(&[(&main, DELETED)]).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["main.rue: []", "other.rue: [2, 1]"]
    );
    assert_eq!(client.hover(&main, 2, 6).await, Value::Null);
}

#[tokio::test]
async fn test_watched_files_panic_keeps_state() {
    let project = TempProject::new(&[("main.rue", SOURCE), ("other.rue", OTHER)]);
    let main = project.uri("main.rue");
    let other = project.uri("other.rue");

    let mut client = client().await;

    client.open(&main, SOURCE).await;
    client.sync().await;

    project.remove("other.rue");

    client.inject_panic(Some(Task::Compile));
    client.watched(&[(&other, DELETED)]).await;
    assert_eq!(
        client.panic_message().await,
        "Internal error while compiling: injected panic"
    );

    // Nothing was recorded, so the file is still cleared once compiling works again
    client.inject_panic(None);
    client.watched(&[(&other, DELETED)]).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["Rue.toml: []", "main.rue: [2]", "other.rue: []"]
    );
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
