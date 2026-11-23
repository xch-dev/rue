//! `rue build` and `rue test` reject a project whose `Rue.toml` is pinned to another compiler
//! version.

use std::{fs, path::Path, process::Command};

const SOURCE: &str =
    "fn main() -> Int {\n    1\n}\n\ntest fn passes() {\n    assert main() == 1;\n}\n";

/// Runs `rue <command>` in the project, and gives whether it succeeded and what it printed to
/// stderr.
fn run(dir: &Path, command: &str) -> (bool, String) {
    let output = Command::new(env!("CARGO_BIN_EXE_rue"))
        .arg(command)
        .current_dir(dir)
        .output()
        .unwrap();
    (
        output.status.success(),
        String::from_utf8_lossy(&output.stderr).to_string(),
    )
}

fn write_project(dir: &Path, version: &str) {
    fs::write(
        dir.join("Rue.toml"),
        format!("[compiler]\nversion = \"{version}\"\nentrypoint = \"src\"\n"),
    )
    .unwrap();
}

#[test]
fn build_and_test_reject_another_compiler_version() {
    let dir = tempfile::tempdir().unwrap();
    fs::create_dir(dir.path().join("src")).unwrap();
    fs::write(dir.path().join("src/main.rue"), SOURCE).unwrap();

    write_project(dir.path(), "0.0.0");

    for command in ["build", "test"] {
        let (success, stderr) = run(dir.path(), command);
        assert!(!success, "`rue {command}` succeeded");
        assert!(
            stderr.contains("Project version mismatch"),
            "`rue {command}` printed: {stderr}"
        );
    }

    write_project(dir.path(), env!("CARGO_PKG_VERSION"));

    for command in ["build", "test"] {
        let (success, stderr) = run(dir.path(), command);
        assert!(success, "`rue {command}` failed: {stderr}");
    }
}
