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
    /// The URIs of the documents open in the editor. The same file can be open through more than
    /// one URI (for example, through a symlink and through its target).
    documents: Mutex<HashMap<SourceKind, Vec<Url>>>,
}

impl Paths {
    /// Remembers the path the client uses for a directory, so that the files in it are reported
    /// with that path. The first path the client used for a directory is kept.
    pub fn add_root(&self, client: &Path) {
        let Ok(canonical) = client.canonicalize() else {
            return;
        };

        let mut roots = lock(&self.roots);

        if !roots.iter().any(|(root, _)| *root == canonical) {
            roots.push((canonical, client.to_path_buf()));
        }
    }

    /// Remembers the URI of a document the editor opened.
    pub fn open(&self, uri: &Url) {
        let Some((path, kind)) = document(uri) else {
            return;
        };

        if let Some(parent) = path.parent() {
            self.add_root(parent);
        }

        let mut documents = lock(&self.documents);
        let uris = documents.entry(kind).or_default();

        if !uris.contains(uri) {
            uris.push(uri.clone());
        }
    }

    /// Forgets the URI of a document the editor closed. The URI is matched as it was opened,
    /// since the file it refers to may have changed (or been deleted) since then.
    pub fn close(&self, uri: &Url) {
        lock(&self.documents).retain(|_, uris| {
            uris.retain(|open| open != uri);
            !uris.is_empty()
        });
    }

    /// The path the client uses for a file that's open in the editor.
    pub fn open_path(&self, kind: &SourceKind) -> Option<PathBuf> {
        uri_to_path(lock(&self.documents).get(kind)?.first()?)
    }

    /// The URI that the client uses for a file.
    pub fn uri(&self, kind: &SourceKind) -> Option<Url> {
        self.uris(kind).into_iter().next()
    }

    /// The URIs that the client uses for a file: those of the documents open for it, or if there
    /// are none, its path in the innermost directory the client used.
    pub fn uris(&self, kind: &SourceKind) -> Vec<Url> {
        let SourceKind::File(path) = kind else {
            return Vec::new();
        };

        if let Some(uris) = lock(&self.documents).get(kind) {
            return uris.clone();
        }

        let path = Path::new(path);

        let client = lock(&self.roots)
            .iter()
            .filter_map(|(canonical, client)| {
                let rest = path.strip_prefix(canonical).ok()?;
                Some((canonical.components().count(), client.join(rest)))
            })
            .max_by_key(|(depth, _)| *depth)
            .map_or_else(|| path.to_path_buf(), |(_, client)| client);

        Url::from_file_path(client).into_iter().collect()
    }

    /// Converts a location in a file, which the compiler reports with its canonical path, to the
    /// path that the client uses.
    pub fn location(&self, location: Location) -> Location {
        // The kind is found again, rather than taken from the URI's path, since on Windows the
        // canonical path is a verbatim path (`\\?\C:\...`), which a URI can't represent
        let uri = document(&location.uri)
            .and_then(|(_, kind)| self.uri(&kind))
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
        let file = SourceKind::File(canonical.join("sub/main.rue").to_string_lossy().to_string());
        let client_uri = Url::from_file_path(dir.path().join("sub/main.rue")).unwrap();

        let paths = Paths::default();

        // Without knowing the client's path, the canonical one is used
        assert_eq!(
            paths.uri(&file),
            Some(Url::from_file_path(canonical.join("sub/main.rue")).unwrap())
        );

        // Files in a directory the client used are reported with its path
        paths.add_root(dir.path());
        assert_eq!(paths.uri(&file), Some(client_uri.clone()));

        // An open document is reported with the exact URIs the editor used for it
        fs::create_dir_all(dir.path().join("sub")).unwrap();
        fs::write(dir.path().join("sub/main.rue"), "").unwrap();
        let first = Url::parse(&client_uri.as_str().replace("main.rue", "%6Dain.rue")).unwrap();
        let second = Url::from_file_path(canonical.join("sub/main.rue")).unwrap();
        paths.open(&first);
        paths.open(&second);
        assert_eq!(paths.uris(&file), [first.clone(), second.clone()]);

        // Closing one leaves the other
        paths.close(&first);
        assert_eq!(paths.uris(&file), std::slice::from_ref(&second));

        // A closed document is forgotten even if its file was deleted
        fs::remove_file(dir.path().join("sub/main.rue")).unwrap();
        paths.close(&second);
        assert_eq!(paths.uris(&file), [client_uri]);
    }

    #[test]
    fn test_uri_uses_innermost_root() {
        let outer = TempDir::new("rue-lsp-paths");
        let inner = TempDir::new("rue-lsp-paths");

        // The inner directory is inside the outer one, but the client reaches it another way
        let nested = outer.path().join("nested");
        fs::create_dir_all(&nested).unwrap();

        let paths = Paths::default();
        paths.add_root(outer.path());
        paths.add_root(inner.path());

        let kind = |path: &Path| {
            SourceKind::File(
                path.canonicalize()
                    .unwrap()
                    .join("a.rue")
                    .to_string_lossy()
                    .to_string(),
            )
        };

        assert_eq!(
            paths.uri(&kind(&nested)),
            Some(Url::from_file_path(nested.join("a.rue")).unwrap())
        );

        // Once the client uses another path for the nested directory, that's used
        let link = inner.path().join("link");
        #[cfg(unix)]
        std::os::unix::fs::symlink(&nested, &link).unwrap();
        #[cfg(unix)]
        {
            paths.add_root(&link);
            assert_eq!(
                paths.uri(&kind(&nested)),
                Some(Url::from_file_path(link.join("a.rue")).unwrap())
            );
            assert_eq!(
                paths.uri(&kind(outer.path())),
                Some(Url::from_file_path(outer.path().join("a.rue")).unwrap())
            );
        }
    }
}
