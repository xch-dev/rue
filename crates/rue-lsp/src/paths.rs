//! The client and the compiler can refer to the same file by different paths. The compiler
//! identifies files by their canonical path, but the client uses whatever path the user opened,
//! which differs when a project is reached through a symlink (on macOS, `/tmp` is a symlink to
//! `/private/tmp`), and URIs can encode the same path in different ways (Windows drive letters can
//! be `c%3A`, `c:` or `C:`). Requests are looked up by canonical path, and everything sent to the
//! client uses the paths it knows.

use std::collections::HashMap;
use std::fmt::Write;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

use rue_diagnostic::SourceKind;
use serde_json::Value;
use tower_lsp_server::jsonrpc::Request;
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
        .filter(|host| !host.is_empty() && !host.eq_ignore_ascii_case("localhost"));
    let path = uri.path().decode().to_bytes();

    #[cfg(not(windows))]
    {
        use std::ffi::OsStr;
        use std::os::unix::ffi::OsStrExt;

        // Files on other machines can't be opened
        (host.is_none() && path.starts_with(b"/")).then(|| PathBuf::from(OsStr::from_bytes(&path)))
    }

    #[cfg(windows)]
    windows::path(host, std::str::from_utf8(&path).ok()?).map(PathBuf::from)
}

/// The URI of an absolute path, which `uri_to_path` turns back into the same path.
///
/// `Uri::from_file_path` isn't used, since on Windows it keeps the verbatim prefix of canonical
/// paths (`\\?\C:\...`), which a URI can't represent, and turns UNC paths into `file://///...`.
pub fn path_to_uri(path: &Path) -> Option<Uri> {
    if !path.is_absolute() {
        return None;
    }

    #[cfg(not(windows))]
    let uri = {
        use std::os::unix::ffi::OsStrExt;

        format!(
            "file://{}",
            encode(path.as_os_str().as_bytes(), is_path_char, false)
        )
    };

    #[cfg(windows)]
    let uri = {
        let (host, path) = windows::uri_parts(path.to_str()?);
        format!(
            "file://{host}{}",
            encode(path.as_bytes(), is_path_char, false)
        )
    };

    uri.parse().ok()
}

/// Whether a byte is left as it is in the path of a URI built for a file: it's one of RFC 3986's
/// `pchar`s (unreserved characters, sub-delimiters, `:` and `@`) or a `/`.
fn is_path_char(byte: u8) -> bool {
    byte.is_ascii_alphanumeric() || b"-._~!$&'()*+,;=:@/".contains(&byte)
}

/// Whether a byte can appear in a URI as it is, other than `%` (which only starts an escape) and
/// `[` and `]` (which only an IP address in the host can use).
fn is_uri_char(byte: u8) -> bool {
    is_path_char(byte) || b"?#".contains(&byte)
}

/// Percent-encodes the bytes that aren't kept. With `escapes`, the text is already partly
/// encoded, and a `%` followed by two hex digits is kept as the escape it is, while any other `%`
/// is literal (as `url` treated it), so it's encoded.
fn encode(bytes: &[u8], keep: impl Fn(u8) -> bool, escapes: bool) -> String {
    let mut encoded = String::with_capacity(bytes.len());

    for (i, &byte) in bytes.iter().enumerate() {
        let escape = escapes
            && byte == b'%'
            && bytes
                .get(i + 1..i + 3)
                .is_some_and(|hex| hex.iter().all(u8::is_ascii_hexdigit));

        if escape || keep(byte) {
            encoded.push(byte as char);
        } else {
            write!(encoded, "%{byte:02X}").unwrap();
        }
    }

    encoded
}

/// Percent-encodes the characters that URIs can't contain in the URIs a client sent, which the
/// protocol types would otherwise reject, failing the whole message. Clients built on the `url`
/// crate (such as Helix and Zed) leave `[`, `]`, `^` and `|` in paths as they are, and `tower-lsp`
/// 0.20 accepted them. URIs that are valid are left exactly as they were sent.
pub fn encode_uris(request: Request) -> Request {
    let (method, id, params) = request.into_parts();

    let mut builder = Request::build(method);

    if let Some(mut params) = params {
        encode_uri_fields(&mut params);
        builder = builder.params(params);
    }

    if let Some(id) = id {
        builder = builder.id(id);
    }

    builder.finish()
}

fn encode_uri_fields(value: &mut Value) {
    match value {
        Value::Object(fields) => {
            for (key, value) in fields {
                if let Value::String(uri) = value
                    && (key == "uri" || key.ends_with("Uri"))
                {
                    if uri.parse::<Uri>().is_err() {
                        *uri = encode(uri.as_bytes(), is_uri_char, true);
                    }
                } else {
                    encode_uri_fields(value);
                }
            }
        }
        Value::Array(values) => values.iter_mut().for_each(encode_uri_fields),
        _ => {}
    }
}

/// How Windows paths and file URIs correspond, which `url::Url` handled before. These only work
/// with strings, so that they're tested on every platform.
#[cfg(any(windows, test))]
mod windows {
    /// The path of a file URI's decoded path: `/c:/a` is `c:\a`, and with a host, `/share/a` is
    /// `\\host\share\a`. Without a host, the path has to start with a drive letter.
    pub fn path(host: Option<&str>, path: &str) -> Option<String> {
        let path = path.replace('/', "\\");

        if let Some(host) = host {
            return Some(format!(r"\\{host}{path}"));
        }

        let path = path.strip_prefix('\\')?;

        match path.as_bytes() {
            [drive, b':'] if drive.is_ascii_alphabetic() => Some(format!(r"{path}\")),
            [drive, b':', b'\\', ..] if drive.is_ascii_alphabetic() => Some(path.to_string()),
            _ => None,
        }
    }

    /// The host and path of the URI of an absolute path, whose path isn't percent-encoded yet.
    /// Verbatim paths (`\\?\C:\a` and `\\?\UNC\server\share\a`), which canonical paths are, have
    /// the same URIs as the ordinary paths they're the same as.
    pub fn uri_parts(path: &str) -> (&str, String) {
        let (host, path) = if let Some(rest) = path
            .strip_prefix(r"\\?\UNC\")
            .or_else(|| path.strip_prefix(r"\\"))
            .filter(|rest| !rest.starts_with(r"?\"))
        {
            let (host, rest) = rest.split_once('\\').unwrap_or((rest, ""));
            (host, format!(r"\{rest}"))
        } else {
            let path = path.strip_prefix(r"\\?\").unwrap_or(path);
            ("", format!(r"\{path}"))
        };

        (host, path.replace('\\', "/"))
    }
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

    use serde_json::json;

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

        // Only local files with absolute paths have paths
        assert_eq!(path("untitled:Untitled-1"), None);
        assert_eq!(path("https://example.com/main.rue"), None);
        assert_eq!(path("file://server/main.rue"), None);
        assert_eq!(path("file:main.rue"), None);
        assert_eq!(path("git:/home/rue/main.rue"), None);
    }

    #[test]
    fn test_path_to_uri() {
        let uri = |path: &str| path_to_uri(Path::new(path)).map(|uri| uri.as_str().to_string());

        // Characters that URIs can't contain are encoded, and `pchar`s aren't
        assert_eq!(
            uri("/a b/[id]/%41^|/x:y@z+(1).rue"),
            Some("file:///a%20b/%5Bid%5D/%2541%5E%7C/x:y@z+(1).rue".to_string())
        );
        assert_eq!(
            uri("/home/файл.rue"),
            Some("file:///home/%D1%84%D0%B0%D0%B9%D0%BB.rue".to_string())
        );
        assert_eq!(uri("relative.rue"), None);
    }

    #[test]
    fn test_path_to_uri_round_trips() {
        let dir = TempDir::new("rue-lsp-paths");

        for name in ["main.rue", "a b.rue", "%41.rue", "[id]^|.rue", "файл.rue"] {
            let path = dir.path().join(name);
            assert_eq!(uri_to_path(&path_to_uri(&path).unwrap()), Some(path));
        }
    }

    #[test]
    fn test_windows_paths() {
        // URIs to paths, from the decoded path of the URI
        for uri in ["/c:/Users/rue/main.rue", r"/c:\Users\rue\main.rue"] {
            assert_eq!(
                windows::path(None, uri).as_deref(),
                Some(r"c:\Users\rue\main.rue")
            );
        }

        assert_eq!(windows::path(None, "/C:").as_deref(), Some(r"C:\"));
        assert_eq!(
            windows::path(Some("server"), "/share/main.rue").as_deref(),
            Some(r"\\server\share\main.rue")
        );

        // Without a host, the path has to start with a drive letter
        for uri in ["/Users/main.rue", "/", "", "/1:/main.rue", "/c:main.rue"] {
            assert_eq!(windows::path(None, uri), None, "{uri}");
        }

        // Paths to URIs, which verbatim paths have the same as ordinary paths
        for path in [r"C:\Users\main.rue", r"\\?\C:\Users\main.rue"] {
            assert_eq!(
                windows::uri_parts(path),
                ("", "/C:/Users/main.rue".to_string())
            );
        }

        for path in [r"\\server\share\main.rue", r"\\?\UNC\server\share\main.rue"] {
            assert_eq!(
                windows::uri_parts(path),
                ("server", "/share/main.rue".to_string())
            );
        }

        // Both directions agree
        let (host, path) = windows::uri_parts(r"\\?\UNC\server\share\main.rue");
        assert_eq!(
            windows::path(Some(host), &path).as_deref(),
            Some(r"\\server\share\main.rue")
        );
    }

    #[test]
    fn test_encode_uris() {
        let request = Request::build("textDocument/didOpen")
            .params(json!({
                "textDocument": { "uri": "file:///a/[id]/x^y|z.rue", "text": "[x]" },
                "rootUri": "file:///a/[id]",
                "workspaceFolders": [{ "uri": "file:///a/[id]", "name": "[id]" }],
                "changes": [
                    { "uri": "file:///a/%5B.rue" },
                    { "uri": "file://[::1]/a.rue" },
                    { "uri": "file:///a/100%/%41%4.rue" },
                ],
            }))
            .finish();

        assert_eq!(
            encode_uris(request).params(),
            Some(&json!({
                "textDocument": { "uri": "file:///a/%5Bid%5D/x%5Ey%7Cz.rue", "text": "[x]" },
                "rootUri": "file:///a/%5Bid%5D",
                "workspaceFolders": [{ "uri": "file:///a/%5Bid%5D", "name": "[id]" }],
                "changes": [
                    { "uri": "file:///a/%5B.rue" },
                    { "uri": "file://[::1]/a.rue" },
                    { "uri": "file:///a/100%25/%41%254.rue" },
                ],
            }))
        );

        // The method and id are kept
        let request = encode_uris(Request::build("shutdown").id(7).finish());
        assert_eq!(request.method(), "shutdown");
        assert_eq!(request.id(), Some(&7.into()));
        assert_eq!(request.params(), None);
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
        let first = client_uri
            .as_str()
            .replace("main.rue", "%6Dain.rue")
            .parse::<Uri>()
            .unwrap();
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
