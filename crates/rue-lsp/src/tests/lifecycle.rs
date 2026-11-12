//! Tests for how the server keeps its state and the editor's diagnostics up to date as documents
//! are opened and closed, and as files change on disk.

use serde_json::{Value, json};
use tower_lsp::lsp_types::{FileChangeType, Url};

use super::MANIFEST;
use super::protocol::{Client, MISMATCH, SOURCE, TempProject};
use crate::Task;

const CREATED: FileChangeType = FileChangeType::CREATED;
const CHANGED: FileChangeType = FileChangeType::CHANGED;
const DELETED: FileChangeType = FileChangeType::DELETED;

/// Declares `other`, which is unused (a warning) and has a type error.
const OTHER: &str = "fn other() -> Int {\n    \"hello\"\n}\n";

#[tokio::test]
async fn test_close_reverts_to_disk() {
    let project = TempProject::new(&[("main.rue", SOURCE)]);
    let main = project.uri("main.rue");

    let mut client = Client::initialized().await;

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

    let mut client = Client::initialized().await;

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

    let mut client = Client::initialized().await;

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

    let mut client = Client::initialized().await;

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

    let mut client = Client::initialized().await;

    client.open(&main, SOURCE).await;
    client.sync().await;

    project.write("Rue.toml", "[compiler\n");
    client.watched(&[(&manifest, CHANGED)]).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["Rue.toml: [1]", "main.rue: []", "other.rue: []"]
    );
    assert_eq!(client.hover(&other, 0, 4).await, Value::Null);

    project.write("Rue.toml", MANIFEST);
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

    let mut client = Client::initialized().await;

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
        format!("{MANIFEST}\n[lints]\nunnecessary_plus = \"allow\"\n"),
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

    let mut client = Client::initialized().await;

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

        let mut client = Client::initialized().await;

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
    let project = TempProject::with_manifest(
        None,
        &[
            ("main.rue", SOURCE),
            ("other.rue", OTHER),
            ("third.rue", "fn third() -> Int {\n    1\n}\n"),
        ],
    );
    let main = project.uri("main.rue");
    let other = project.uri("other.rue");

    let mut client = Client::initialized().await;

    client.open(&other, OTHER).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["main.rue: [2]", "other.rue: [2, 1]", "third.rue: [2]"]
    );

    // Without `main.rue`, the directory isn't a project anymore, so `other.rue` is compiled on its
    // own since it's open, and `third.rue` is cleared since it isn't
    project.remove("main.rue");
    client.watched(&[(&main, DELETED)]).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["main.rue: []", "other.rue: [2, 1]", "third.rue: []"]
    );
    assert_eq!(client.hover(&main, 2, 6).await, Value::Null);
}

#[tokio::test]
async fn test_watched_files_panic_keeps_state() {
    let project = TempProject::new(&[("main.rue", SOURCE), ("other.rue", OTHER)]);
    let main = project.uri("main.rue");
    let other = project.uri("other.rue");

    let mut client = Client::initialized().await;

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
async fn test_manifest_created_in_parent_takes_over() {
    let project = TempProject::with_manifest(
        None,
        &[
            ("b/main.rue", SOURCE),
            ("b/other.rue", OTHER),
            ("src/main.rue", MISMATCH),
        ],
    );
    let other = project.uri("b/other.rue");
    let manifest = project.uri("Rue.toml");

    let mut client = Client::initialized().await;

    client.open(&other, OTHER).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["b/main.rue: [2]", "b/other.rue: [2, 1]"]
    );

    // The new manifest's project only includes `src`, so `b` isn't part of any project anymore
    project.write(
        "Rue.toml",
        "[compiler]\nentrypoint = \"src\"\nstd = false\n",
    );
    client.watched(&[(&manifest, CREATED)]).await;
    assert_eq!(
        project.published(&client.sync().await),
        [
            "Rue.toml: []",
            "b/main.rue: []",
            "b/other.rue: []",
            "src/main.rue: [1]"
        ]
    );
    assert_eq!(client.hover(&other, 0, 4).await, Value::Null);
}

#[tokio::test]
async fn test_unopened_projects_are_ignored() {
    let project = TempProject::new(&[("main.rue", SOURCE)]);
    let standalone = TempProject::with_manifest(None, &[("lone.rue", OTHER)]);

    let mut client = Client::initialized().await;

    // Neither project has an open file, so neither is compiled
    standalone.write("new.rue", OTHER);
    client
        .watched(&[
            (&project.uri("main.rue"), CHANGED),
            (&standalone.uri("lone.rue"), CHANGED),
            (&standalone.uri("new.rue"), CREATED),
        ])
        .await;
    client.assert_no_messages().await;
    assert_eq!(client.compiles(), 0);
}

#[tokio::test]
async fn test_close_deleted_standalone_file() {
    // Without watchers, the server only finds out that the file was deleted when it's closed
    let project = TempProject::with_manifest(None, &[("lone.rue", OTHER)]);
    let lone = project.uri("lone.rue");

    let mut client = Client::initialized().await;

    client.open(&lone, OTHER).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["lone.rue: [2, 1]"]
    );

    project.remove("lone.rue");
    client.close(&lone).await;
    assert_eq!(project.published(&client.sync().await), ["lone.rue: []"]);
}

#[tokio::test]
async fn test_panic_keeps_removed_project() {
    let project = TempProject::new(&[("lib.rue", OTHER)]);
    let lib = project.uri("lib.rue");
    let manifest = project.uri("Rue.toml");

    let mut client = Client::initialized().await;

    client.open(&lib, OTHER).await;
    client.sync().await;

    project.remove("Rue.toml");

    client.inject_panic(Some(Task::Compile));
    client.watched(&[(&manifest, DELETED)]).await;
    assert_eq!(
        client.panic_message().await,
        "Internal error while compiling: injected panic"
    );

    // The project wasn't removed, so it still answers queries, and it's removed once compiling
    // works again
    assert_ne!(client.hover(&lib, 0, 4).await, Value::Null);

    client.inject_panic(None);
    client.watched(&[(&manifest, DELETED)]).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["Rue.toml: []", "lib.rue: [2, 1]"]
    );
}

#[tokio::test]
async fn test_nested_project_keeps_shared_files() {
    // The inner project allows unused functions, so its diagnostics differ from the outer one's
    let project = TempProject::new(&[
        ("main.rue", SOURCE),
        (
            "sub/Rue.toml",
            &format!("{MANIFEST}\n[lints]\nunused_function = \"allow\"\n"),
        ),
        ("sub/x.rue", OTHER),
    ]);
    let main = project.uri("main.rue");
    let x = project.uri("sub/x.rue");

    let mut client = Client::initialized().await;

    // Both projects include `sub/x.rue`, since the outer one compiles every directory, and the
    // outer one was compiled last
    client.open(&x, OTHER).await;
    client.open(&main, SOURCE).await;
    let published = project.published(&client.sync().await);
    assert!(
        published.contains(&"sub/x.rue: [2, 1]".to_string()),
        "{published:?}"
    );

    // The outer project can't be compiled anymore, so the inner one reports the file again
    project.write("Rue.toml", "[compiler\n");
    client.close(&main).await;
    assert_eq!(
        project.published(&client.sync().await),
        [
            "Rue.toml: [1]",
            "main.rue: []",
            "sub/Rue.toml: []",
            "sub/x.rue: [1]"
        ]
    );
    assert_ne!(client.hover(&x, 0, 4).await, Value::Null);
}

#[tokio::test]
async fn test_close_after_directory_deleted() {
    let project = TempProject::new(&[("main.rue", SOURCE), ("sub/x.rue", OTHER)]);
    let x = project.uri("sub/x.rue");

    let mut client = Client::initialized().await;

    // The unsaved text fixes the type error
    client.open(&x, "fn other() -> Int {\n    42\n}\n").await;
    client.sync().await;

    project.remove("sub");
    client.watched(&[(&x, DELETED)]).await;
    client.close(&x).await;
    client.sync().await;

    // Once the file is back, what's on disk is compiled, not the text the editor had
    project.write("sub/x.rue", OTHER);
    client.watched(&[(&x, CREATED)]).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["Rue.toml: []", "main.rue: [2]", "sub/x.rue: [2, 1]"]
    );
}

#[tokio::test]
async fn test_failed_compile_stays_tracked() {
    let project = TempProject::with_manifest(None, &[("main.rue", SOURCE)]);
    let main = project.uri("main.rue");
    let bad = project.uri("bad.rue");

    let mut client = Client::initialized().await;

    client.open(&main, SOURCE).await;
    client.sync().await;

    // A file that isn't valid UTF-8 can't be read, so the project can't be compiled
    project.write("bad.rue", [0xff, 0xfe]);
    client.watched(&[(&bad, CREATED)]).await;
    assert_eq!(project.published(&client.sync().await), ["main.rue: []"]);

    project.remove("bad.rue");
    client.watched(&[(&bad, DELETED)]).await;
    assert_eq!(project.published(&client.sync().await), ["main.rue: [2]"]);
}

#[tokio::test]
async fn test_watched_change_keeps_unsaved_text() {
    let project = TempProject::new(&[("main.rue", SOURCE)]);
    let main = project.uri("main.rue");

    let mut client = Client::initialized().await;

    client.open(&main, MISMATCH).await;
    client.sync().await;

    // The file changed on disk, but the editor's text is still what's compiled
    project.write("main.rue", "fn main() -> Int {\n    42\n}\n");
    client.watched(&[(&main, CHANGED)]).await;
    assert_eq!(
        project.published(&client.sync().await),
        ["Rue.toml: []", "main.rue: [1]"]
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
