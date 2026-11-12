//! The client and the compiler can refer to the same file by different paths. The compiler
//! identifies files by their canonical path, but the client uses whatever path the user opened,
//! which differs when a project is reached through a symlink (on macOS, `/tmp` is a symlink to
//! `/private/tmp`), and URIs can encode the same path in different ways (Windows drive letters can
//! be `c%3A`, `c:` or `C:`). Requests are looked up by canonical path, and everything sent to the
//! client uses the paths it knows.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

use rue_compiler::normalize_path;
use rue_diagnostic::SourceKind;
use tower_lsp::lsp_types::{Location, Url};

use crate::lock;

#[derive(Debug, Default)]
pub struct Paths {
    /// The directories the client has used, as (canonical path, client path) pairs.
    roots: Mutex<Vec<(PathBuf, PathBuf)>>,
    /// The URIs of the documents open in the editor.
    documents: Mutex<HashMap<SourceKind, Url>>,
}

impl Paths {
    /// Remembers the path the client uses for a directory, so that the files in it are reported
    /// with that path.
    pub fn add_root(&self, client: &Path) {
        let Ok(canonical) = client.canonicalize() else {
            return;
        };

        let mut roots = lock(&self.roots);

        if !roots.iter().any(|(root, _)| *root == canonical) {
            roots.push((canonical, client.to_path_buf()));
        }
    }

    /// Remembers the URI of a document the editor opened, and returns its path and source kind.
    pub fn open(&self, uri: &Url) -> Option<(PathBuf, SourceKind)> {
        let (path, kind) = document(uri)?;

        if let Some(parent) = path.parent() {
            self.add_root(parent);
        }

        lock(&self.documents).insert(kind.clone(), uri.clone());

        Some((path, kind))
    }

    pub fn close(&self, uri: &Url) {
        if let Some((_, kind)) = document(uri) {
            lock(&self.documents).remove(&kind);
        }
    }

    /// The documents open in the editor, by source kind.
    pub fn documents(&self) -> HashMap<SourceKind, Url> {
        lock(&self.documents).clone()
    }

    /// The URI that the client uses for a file, given its canonical path.
    pub fn uri(&self, path: &str) -> Option<Url> {
        if let Some(uri) = lock(&self.documents).get(&SourceKind::File(path.to_string())) {
            return Some(uri.clone());
        }

        let path = Path::new(path);

        // The innermost directory is the most specific
        let client = lock(&self.roots)
            .iter()
            .filter_map(|(canonical, client)| {
                let rest = path.strip_prefix(canonical).ok()?;
                Some((canonical.components().count(), client.join(rest)))
            })
            .max_by_key(|(depth, _)| *depth)
            .map_or_else(|| path.to_path_buf(), |(_, client)| client);

        Url::from_file_path(client).ok()
    }

    /// Converts a location in a file, which the compiler reports with its canonical path, to the
    /// path that the client uses.
    pub fn location(&self, location: Location) -> Location {
        let uri = location
            .uri
            .to_file_path()
            .ok()
            .and_then(|path| self.uri(&path.to_string_lossy()))
            .unwrap_or(location.uri);

        Location::new(uri, location.range)
    }
}

/// The path of a file URI. Windows drive letters are accepted whether or not their colon is
/// percent-encoded (VS Code sends `file:///c%3A/...`).
pub fn uri_to_path(uri: &Url) -> Option<PathBuf> {
    decode_drive_letter(uri).to_file_path().ok()
}

fn decode_drive_letter(uri: &Url) -> Url {
    let path = uri.path();
    let bytes = path.as_bytes();

    let encoded = bytes.len() >= 5
        && bytes[0] == b'/'
        && bytes[1].is_ascii_alphabetic()
        && path[2..5].eq_ignore_ascii_case("%3a");

    if !encoded {
        return uri.clone();
    }

    let mut uri = uri.clone();
    let decoded = format!("/{}:{}", &path[1..2], &path[5..]);
    uri.set_path(&decoded);
    uri
}

/// The path of a document, and the source kind that the compiler identifies it by, which is its
/// canonical path. If the file (or the directories it's in) doesn't exist yet or anymore, the
/// nearest directory that does is canonicalized instead.
pub fn document(uri: &Url) -> Option<(PathBuf, SourceKind)> {
    let path = uri_to_path(uri)?;

    if let Ok(kind) = normalize_path(&path) {
        return Some((path, kind));
    }

    let (canonical, rest) = path.ancestors().skip(1).find_map(|ancestor| {
        Some((
            ancestor.canonicalize().ok()?,
            path.strip_prefix(ancestor).ok()?,
        ))
    })?;

    let kind = SourceKind::File(canonical.join(rest).to_string_lossy().to_string());

    Some((path, kind))
}

#[cfg(test)]
mod tests {
    use std::fs;

    use super::*;
    use crate::tests::TempDir;

    #[test]
    fn test_decode_drive_letter() {
        for (uri, expected) in [
            (
                "file:///c%3A/Users/rue/main.rue",
                "file:///c:/Users/rue/main.rue",
            ),
            ("file:///C%3a/main.rue", "file:///C:/main.rue"),
            ("file:///c:/main.rue", "file:///c:/main.rue"),
            ("file:///home/rue/main.rue", "file:///home/rue/main.rue"),
            ("file:///home/a%3Ab.rue", "file:///home/a%3Ab.rue"),
            ("file:///c%3A", "file:///c:"),
        ] {
            let uri = Url::parse(uri).unwrap();
            assert_eq!(decode_drive_letter(&uri).as_str(), expected);
        }
    }

    #[test]
    fn test_document_that_does_not_exist() {
        let dir = TempDir::new("rue-lsp-paths");
        let canonical = dir.path().canonicalize().unwrap();

        // Neither the file nor its directory exists, so the temporary directory is canonicalized
        let uri = Url::from_file_path(dir.path().join("sub/new.rue")).unwrap();
        let (path, kind) = document(&uri).unwrap();

        assert_eq!(path, dir.path().join("sub/new.rue"));
        assert_eq!(
            kind,
            SourceKind::File(canonical.join("sub/new.rue").to_string_lossy().to_string())
        );

        // Once it exists, it's identified the same way
        fs::create_dir_all(dir.path().join("sub")).unwrap();
        fs::write(dir.path().join("sub/new.rue"), "").unwrap();
        assert_eq!(document(&uri).unwrap().1, kind);
    }

    #[test]
    fn test_uri_uses_client_paths() {
        let dir = TempDir::new("rue-lsp-paths");
        let canonical = dir.path().canonicalize().unwrap();
        let canonical_file = canonical.join("sub/main.rue");
        let canonical_file = canonical_file.to_string_lossy();

        let paths = Paths::default();

        // Without knowing the client's path, the canonical one is used
        assert_eq!(
            paths.uri(&canonical_file),
            Some(Url::from_file_path(canonical.join("sub/main.rue")).unwrap())
        );

        // Files in a directory the client used are reported with its path
        paths.add_root(dir.path());
        assert_eq!(
            paths.uri(&canonical_file),
            Some(Url::from_file_path(dir.path().join("sub/main.rue")).unwrap())
        );

        // An open document is reported with the exact URI the editor used
        fs::create_dir_all(dir.path().join("sub")).unwrap();
        fs::write(dir.path().join("sub/main.rue"), "").unwrap();
        let uri = Url::from_file_path(dir.path().join("sub/main.rue")).unwrap();
        let uri = Url::parse(&uri.as_str().replace("main.rue", "%6Dain.rue")).unwrap();
        paths.open(&uri);
        assert_eq!(paths.uri(&canonical_file), Some(uri.clone()));

        paths.close(&uri);
        assert_eq!(
            paths.uri(&canonical_file),
            Some(Url::from_file_path(dir.path().join("sub/main.rue")).unwrap())
        );
    }
}
