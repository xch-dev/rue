//! The client and the compiler can refer to the same file by different paths. The compiler
//! identifies files by their canonical path, but the client uses whatever path the user opened,
//! which differs when a project is reached through a symlink (on macOS, `/tmp` is a symlink to
//! `/private/tmp`), and URIs can encode the same path in different ways (Windows drive letters can
//! be `c%3A`, `c:` or `C:`). Requests are looked up by canonical path, and everything sent to the
//! client uses the paths it knows.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

use rue_diagnostic::SourceKind;
use tower_lsp_server::ls_types::{Location, Uri};

use crate::lock;

#[derive(Debug, Default)]
pub struct Paths {
    /// The directories the client has used, as (canonical path, client path) pairs.
    roots: Mutex<Vec<(PathBuf, PathBuf)>>,
    /// The URIs of the documents open in the editor. The same file can be open through more than
    /// one URI (for example, through a symlink and through its target).
    documents: Mutex<HashMap<SourceKind, Vec<Uri>>>,
    /// The paths the client uses for files that are symlinks, which the directories they're in
    /// don't give (for example, a manifest shared between projects).
    aliases: Mutex<HashMap<SourceKind, Vec<PathBuf>>>,
    /// The URIs that diagnostics were last published to for each file.
    published: Mutex<HashMap<SourceKind, Vec<Uri>>>,
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

    /// Remembers the path the client uses for a file, which may be a symlink.
    pub fn add_alias(&self, client: &Path) {
        let Some(canonical) = canonical(client) else {
            return;
        };

        let kind = SourceKind::File(canonical.to_string_lossy().to_string());
        let mut aliases = lock(&self.aliases);
        let paths = aliases.entry(kind).or_default();

        if !paths.iter().any(|path| path == client) {
            paths.push(client.to_path_buf());
        }
    }

    /// Remembers the URI of a document the editor opened.
    pub fn open(&self, uri: &Uri) {
        let Some((_, kind)) = document(uri) else {
            return;
        };

        let mut documents = lock(&self.documents);
        let uris = documents.entry(kind).or_default();

        if !uris.contains(uri) {
            uris.push(uri.clone());
        }
    }

    /// Forgets the URI of a document the editor closed. The URI is matched as it was opened,
    /// since the file it refers to may have changed (or been deleted) since then.
    pub fn close(&self, uri: &Uri) {
        lock(&self.documents).retain(|_, uris| {
            uris.retain(|open| open != uri);
            !uris.is_empty()
        });
    }

    /// Whether a file is open in the editor, through any URI.
    pub fn is_open(&self, kind: &SourceKind) -> bool {
        lock(&self.documents).contains_key(kind)
    }

    /// The path the client uses for a file that's open in the editor.
    pub fn open_path(&self, kind: &SourceKind) -> Option<PathBuf> {
        uri_to_path(lock(&self.documents).get(kind)?.first()?)
    }

    /// The URI that the client uses for a file.
    pub fn uri(&self, kind: &SourceKind) -> Option<Uri> {
        self.uris(kind).into_iter().next()
    }

    /// The URIs that the client uses for a file: those of the documents open for it, or if there
    /// are none, the paths the client used for it, or its path in the innermost directory the
    /// client used.
    pub fn uris(&self, kind: &SourceKind) -> Vec<Uri> {
        let SourceKind::File(path) = kind else {
            return Vec::new();
        };

        if let Some(uris) = lock(&self.documents).get(kind) {
            return uris.clone();
        }

        let path = Path::new(path);

        // Aliases that aren't symlinks to the file anymore are forgotten
        let mut paths = lock(&self.aliases)
            .get_mut(kind)
            .map(|aliases| {
                aliases.retain(|alias| canonical(alias).is_some_and(|alias| alias == path));
                aliases.clone()
            })
            .unwrap_or_default();

        let client = lock(&self.roots)
            .iter()
            .filter_map(|(canonical, client)| {
                let rest = path.strip_prefix(canonical).ok()?;
                Some((canonical.components().count(), client.join(rest)))
            })
            .max_by_key(|(depth, _)| *depth)
            .map(|(_, client)| client);

        if let Some(client) = client
            && !paths.contains(&client)
        {
            paths.push(client);
        }

        if paths.is_empty() {
            paths.push(path.to_path_buf());
        }

        paths.iter().filter_map(|path| path_to_uri(path)).collect()
    }

    /// The URIs to publish a file's diagnostics to, and those that it was published to last time
    /// but won't be anymore, whose diagnostics need to be cleared (for example, after the editor
    /// closed a document that it had opened through a symlink).
    pub fn publish(&self, kind: &SourceKind) -> (Vec<Uri>, Vec<Uri>) {
        let uris = self.uris(kind);

        let previous = lock(&self.published)
            .insert(kind.clone(), uris.clone())
            .unwrap_or_default();

        let stale = previous
            .into_iter()
            .filter(|uri| !uris.contains(uri))
            .collect();

        (uris, stale)
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

/// The path of a file URI, or `None` if it isn't one (such as `untitled:Untitled-1`). Windows
/// drive letters are accepted whether or not their colon is percent-encoded (VS Code sends
/// `file:///c%3A/...`), since the whole path is decoded.
///
/// `Uri::to_file_path` doesn't check the scheme, ignores the host on Unix, and doesn't turn a host
/// into a UNC path on Windows, so the conversion is done here, the way `url::Url::to_file_path`
/// did it before.
pub fn uri_to_path(uri: &Uri) -> Option<PathBuf> {
    if !uri.scheme().as_str().eq_ignore_ascii_case("file") {
        return None;
    }

    let host = uri
        .authority()
        .map(|authority| authority.host())
        .unwrap_or_default();
    let local = host.is_empty() || host.eq_ignore_ascii_case("localhost");

    file_path((!local).then_some(host), &uri.path().decode().to_bytes())
}

#[cfg(not(windows))]
fn file_path(host: Option<&str>, path: &[u8]) -> Option<PathBuf> {
    use std::ffi::OsStr;
    use std::os::unix::ffi::OsStrExt;

    // Files on other machines can't be opened
    if host.is_some() || !path.starts_with(b"/") {
        return None;
    }

    Some(PathBuf::from(OsStr::from_bytes(path)))
}

#[cfg(windows)]
fn file_path(host: Option<&str>, path: &[u8]) -> Option<PathBuf> {
    let path = std::str::from_utf8(path).ok()?.replace('/', "\\");

    match host {
        // `file://server/share/...` is `\\server\share\...`
        Some(host) => Some(PathBuf::from(format!("\\\\{host}{path}"))),
        // `file:///c:/...` is `c:\...`
        None => path.strip_prefix('\\').map(PathBuf::from),
    }
}

/// The URI of a file. On Windows, canonical paths are verbatim paths (`\\?\C:\...`), which a URI
/// can't represent, so they're turned back into ordinary ones first.
pub fn path_to_uri(path: &Path) -> Option<Uri> {
    if !path.is_absolute() {
        return None;
    }

    #[cfg(windows)]
    let path = &without_verbatim_prefix(path);

    Uri::from_file_path(path)
}

#[cfg(windows)]
fn without_verbatim_prefix(path: &Path) -> PathBuf {
    use std::path::{Component, Prefix};

    let mut components = path.components();

    let prefix = match components.next() {
        Some(Component::Prefix(prefix)) => match prefix.kind() {
            Prefix::VerbatimDisk(disk) => format!("{}:", disk as char),
            Prefix::VerbatimUNC(server, share) => format!(
                "\\\\{}\\{}",
                server.to_string_lossy(),
                share.to_string_lossy()
            ),
            _ => return path.to_path_buf(),
        },
        _ => return path.to_path_buf(),
    };

    let mut result = PathBuf::from(prefix);
    result.extend(components);
    result
}

/// The path of a document, and the source kind that the compiler identifies it by, which is its
/// canonical path. If the file (or the directories it's in) doesn't exist yet or anymore, the
/// nearest directory that does is canonicalized instead.
pub fn document(uri: &Uri) -> Option<(PathBuf, SourceKind)> {
    let path = uri_to_path(uri)?;
    let kind = SourceKind::File(canonical(&path)?.to_string_lossy().to_string());
    Some((path, kind))
}

/// The canonical form of a path, even if it doesn't exist (anymore): the nearest directory that
/// does is canonicalized, and the rest of the path is kept as it is.
pub fn canonical(path: &Path) -> Option<PathBuf> {
    path.ancestors().find_map(|ancestor| {
        let rest = path.strip_prefix(ancestor).ok()?;
        let ancestor = ancestor.canonicalize().ok()?;

        Some(if rest.as_os_str().is_empty() {
            ancestor
        } else {
            ancestor.join(rest)
        })
    })
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::str::FromStr;

    use super::*;
    use crate::tests::TempDir;

    #[test]
    fn test_uri_to_path() {
        let path = |uri: &str| uri_to_path(&uri.parse().unwrap());

        assert_eq!(
            path("file:///home/rue/main.rue"),
            Some(PathBuf::from("/home/rue/main.rue"))
        );
        assert_eq!(
            path("file://localhost/home/rue/main.rue"),
            Some(PathBuf::from("/home/rue/main.rue"))
        );
        assert_eq!(
            path("FILE:///home/a%20b%3Ac.rue"),
            Some(PathBuf::from("/home/a b:c.rue"))
        );

        // Only local files have paths
        assert_eq!(path("untitled:Untitled-1"), None);
        assert_eq!(path("https://example.com/main.rue"), None);
        assert_eq!(path("file://server/main.rue"), None);
    }

    #[cfg(windows)]
    #[test]
    fn test_uri_to_path_windows() {
        let path = |uri: &str| uri_to_path(&uri.parse().unwrap());

        for uri in [
            "file:///c%3A/Users/rue/main.rue",
            "file:///c:/Users/rue/main.rue",
        ] {
            assert_eq!(path(uri), Some(PathBuf::from(r"c:\Users\rue\main.rue")));
        }

        assert_eq!(
            path("file://server/share/main.rue"),
            Some(PathBuf::from(r"\\server\share\main.rue"))
        );
        assert_eq!(
            path_to_uri(Path::new(r"\\?\C:\Users\main.rue")),
            path_to_uri(Path::new(r"C:\Users\main.rue"))
        );
    }

    #[test]
    fn test_path_to_uri_round_trips() {
        let dir = TempDir::new("rue-lsp-paths");

        for name in ["main.rue", "a b.rue", "%41.rue", "файл.rue"] {
            let path = dir.path().join(name);
            assert_eq!(uri_to_path(&path_to_uri(&path).unwrap()), Some(path));
        }

        assert_eq!(path_to_uri(Path::new("relative.rue")), None);
    }

    #[test]
    fn test_document_that_does_not_exist() {
        let dir = TempDir::new("rue-lsp-paths");
        let canonical = dir.path().canonicalize().unwrap();

        // Neither the file nor its directory exists, so the temporary directory is canonicalized
        let uri = path_to_uri(&dir.path().join("sub/new.rue")).unwrap();
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
        let client_uri = path_to_uri(&dir.path().join("sub/main.rue")).unwrap();

        let paths = Paths::default();

        // Without knowing the client's path, the canonical one is used
        assert_eq!(
            paths.uri(&file),
            Some(path_to_uri(&canonical.join("sub/main.rue")).unwrap())
        );

        // Files in a directory the client used are reported with its path
        paths.add_root(dir.path());
        assert_eq!(paths.uri(&file), Some(client_uri.clone()));

        // An open document is reported with the exact URIs the editor used for it
        fs::create_dir_all(dir.path().join("sub")).unwrap();
        fs::write(dir.path().join("sub/main.rue"), "").unwrap();
        let first = Uri::from_str(&client_uri.as_str().replace("main.rue", "%6Dain.rue")).unwrap();
        let second = path_to_uri(&canonical.join("sub/main.rue")).unwrap();
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

    #[cfg(unix)]
    #[test]
    fn test_uri_uses_innermost_root() {
        let outer = TempDir::new("rue-lsp-paths");
        let other = TempDir::new("rue-lsp-paths");

        // `nested` is inside `outer`, but the client also reaches it through a symlink in `other`
        let nested = outer.path().join("nested");
        let link = other.path().join("link");
        fs::create_dir_all(&nested).unwrap();
        std::os::unix::fs::symlink(&nested, &link).unwrap();

        let kind = |path: &Path| {
            SourceKind::File(
                path.canonicalize()
                    .unwrap()
                    .join("a.rue")
                    .to_string_lossy()
                    .to_string(),
            )
        };

        let paths = Paths::default();
        paths.add_root(outer.path());
        assert_eq!(
            paths.uri(&kind(&nested)),
            Some(path_to_uri(&nested.join("a.rue")).unwrap())
        );

        paths.add_root(&link);
        assert_eq!(
            paths.uri(&kind(&nested)),
            Some(path_to_uri(&link.join("a.rue")).unwrap())
        );
        assert_eq!(
            paths.uri(&kind(outer.path())),
            Some(path_to_uri(&outer.path().join("a.rue")).unwrap())
        );
    }

    #[cfg(unix)]
    #[test]
    fn test_aliases_are_rechecked() {
        let dir = TempDir::new("rue-lsp-paths");
        fs::write(dir.path().join("shared.toml"), "").unwrap();
        std::os::unix::fs::symlink("shared.toml", dir.path().join("a.toml")).unwrap();

        let shared = SourceKind::File(
            canonical(&dir.path().join("shared.toml"))
                .unwrap()
                .to_string_lossy()
                .to_string(),
        );

        let paths = Paths::default();
        paths.add_alias(&dir.path().join("a.toml"));
        assert_eq!(
            paths.uris(&shared),
            [path_to_uri(&dir.path().join("a.toml")).unwrap()]
        );

        // Once it's a file of its own, it isn't an alias anymore
        fs::remove_file(dir.path().join("a.toml")).unwrap();
        fs::write(dir.path().join("a.toml"), "").unwrap();
        assert_eq!(
            paths.uris(&shared),
            [path_to_uri(&canonical(&dir.path().join("shared.toml")).unwrap()).unwrap()]
        );
    }

    #[test]
    fn test_canonical() {
        let dir = TempDir::new("rue-lsp-paths");
        let real = dir.path().canonicalize().unwrap();

        assert_eq!(canonical(dir.path()), Some(real.clone()));
        assert_eq!(
            canonical(&dir.path().join("missing/a.rue")),
            Some(real.join("missing/a.rue"))
        );
    }
}
