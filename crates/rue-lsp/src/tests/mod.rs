mod completion;
mod diagnostics;
mod formatting;
mod hover;
mod lifecycle;
mod navigation;
mod protocol;

use std::fmt::Write;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};

use rue_compiler::normalize_path;
use tower_lsp::LspService;
use tower_lsp::lsp_types::{
    CodeActionContext, CodeActionOrCommand, CodeActionParams, CompletionParams, CompletionResponse,
    DocumentFormattingParams, FormattingOptions, GotoDefinitionParams, GotoDefinitionResponse,
    HoverContents, HoverParams, Location, MarkedString, NumberOrString, PartialResultParams,
    Position, Range, ReferenceContext, ReferenceParams, TextDocumentIdentifier,
    TextDocumentPositionParams, TextEdit, Url, WorkDoneProgressParams,
};

use crate::{Backend, code_actions, position::position_to_offset};

const CURSOR: &str = "$0";

/// A manifest for a project that doesn't use the standard library, so that results only include
/// what's declared in the test itself.
const MANIFEST: &str = "[compiler]\nentrypoint = \".\"\nstd = false\n";

/// An empty temporary directory that's unique to the test, and removed when dropped. On Unix,
/// it's reached through a symlink, like a project in `/tmp` on macOS, so that the paths that the
/// tests use as a client aren't the canonical paths that the compiler uses.
pub struct TempDir {
    path: PathBuf,
    real: PathBuf,
}

impl TempDir {
    pub fn new(prefix: &str) -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);

        let path = std::env::temp_dir().join(format!(
            "{prefix}-{}-{}",
            std::process::id(),
            COUNTER.fetch_add(1, Ordering::SeqCst)
        ));
        let real = path.with_file_name(format!(
            "{}-real",
            path.file_name().unwrap().to_string_lossy()
        ));

        fs::remove_file(&path).ok();
        fs::remove_dir_all(&real).ok();
        fs::create_dir_all(&real).unwrap();

        #[cfg(unix)]
        std::os::unix::fs::symlink(&real, &path).unwrap();

        #[cfg(not(unix))]
        let path = real.clone();

        Self { path, real }
    }

    pub fn path(&self) -> &Path {
        &self.path
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        if self.path != self.real {
            fs::remove_file(&self.path).ok();
        }

        fs::remove_dir_all(&self.real).ok();
    }
}

/// A temporary project directory on disk, along with a language server instance that has
/// opened its files. Any `$0` marker in a file's text is stripped and recorded as the cursor.
pub struct TestProject {
    root: PathBuf,
    _dir: TempDir,
    service: LspService<Backend>,
    cursor: Option<(Url, Position)>,
}

impl TestProject {
    /// Creates a project that doesn't use the standard library, so that results only include
    /// what's declared in the test itself.
    pub fn new(files: &[(&str, &str)]) -> Self {
        Self::with_manifest(Some(MANIFEST), files)
    }

    pub fn with_manifest(manifest: Option<&str>, files: &[(&str, &str)]) -> Self {
        let dir = TempDir::new("rue-lsp-test");
        let root = dir.path().to_path_buf();

        if let Some(manifest) = manifest {
            fs::write(root.join("Rue.toml"), manifest).unwrap();
        }

        let mut cursor = None;
        let mut texts = Vec::new();

        for (name, text) in files {
            let path = root.join(name);
            fs::create_dir_all(path.parent().unwrap()).unwrap();

            let text = if let Some(index) = text.find(CURSOR) {
                assert!(cursor.is_none(), "only one cursor is allowed");
                cursor = Some((
                    Url::from_file_path(&path).unwrap(),
                    position_of(text, index),
                ));
                text.replacen(CURSOR, "", 1)
            } else {
                (*text).to_string()
            };

            fs::write(&path, &text).unwrap();
            texts.push((path, text));
        }

        let (service, _) = LspService::new(Backend::new);

        let project = Self {
            root,
            _dir: dir,
            service,
            cursor,
        };

        // Opening any file compiles the whole project, but each file is opened to mirror what an
        // editor would do and to populate the in-memory file cache.
        for (path, text) in texts {
            project
                .backend()
                .on_change_impl(&Url::from_file_path(path).unwrap(), Some(text));
        }

        project
    }

    pub fn backend(&self) -> &Backend {
        self.service.inner()
    }

    pub fn path(&self, name: &str) -> PathBuf {
        self.root.join(name)
    }

    pub fn uri(&self, name: &str) -> Url {
        Url::from_file_path(self.path(name)).unwrap()
    }

    pub fn cursor(&self) -> (Url, Position) {
        self.cursor.clone().expect("missing $0 cursor marker")
    }

    /// Renders a URI and range relative to the project root, e.g. `main.rue:1:4-1:7`.
    pub fn location(&self, uri: &Url, range: Range) -> String {
        let path = uri.to_file_path().unwrap();
        let path = path.strip_prefix(&self.root).unwrap_or(&path);
        format!("{}:{}", path.display(), range_string(range))
    }

    pub fn hover(&self) -> String {
        let (uri, position) = self.cursor();

        let Some(hover) = self.backend().on_hover(&HoverParams {
            text_document_position_params: TextDocumentPositionParams::new(
                TextDocumentIdentifier::new(uri),
                position,
            ),
            work_done_progress_params: WorkDoneProgressParams::default(),
        }) else {
            return "<none>\n".to_string();
        };

        let HoverContents::Array(contents) = hover.contents else {
            panic!("unexpected hover contents: {:?}", hover.contents);
        };

        let mut output = String::new();

        for (i, content) in contents.into_iter().enumerate() {
            let MarkedString::LanguageString(content) = content else {
                panic!("unexpected marked string: {content:?}");
            };

            if i > 0 {
                output.push_str("---\n");
            }

            writeln!(output, "{}", content.value).unwrap();
        }

        output
    }

    pub fn definitions(&self) -> String {
        let (uri, position) = self.cursor();

        let response = self.backend().on_goto_definition(&GotoDefinitionParams {
            text_document_position_params: TextDocumentPositionParams::new(
                TextDocumentIdentifier::new(uri),
                position,
            ),
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        });

        let locations = match response {
            None => Vec::new(),
            Some(GotoDefinitionResponse::Scalar(location)) => vec![location],
            Some(GotoDefinitionResponse::Array(locations)) => locations,
            Some(GotoDefinitionResponse::Link(links)) => panic!("unexpected links: {links:?}"),
        };

        self.locations(&locations)
    }

    pub fn references(&self) -> String {
        let (uri, position) = self.cursor();

        let locations = self
            .backend()
            .on_references(&ReferenceParams {
                text_document_position: TextDocumentPositionParams::new(
                    TextDocumentIdentifier::new(uri),
                    position,
                ),
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
                context: ReferenceContext {
                    include_declaration: true,
                },
            })
            .unwrap_or_default();

        self.locations(&locations)
    }

    fn locations(&self, locations: &[Location]) -> String {
        if locations.is_empty() {
            return "<none>\n".to_string();
        }

        let mut output = String::new();

        for location in locations {
            writeln!(output, "{}", self.location(&location.uri, location.range)).unwrap();
        }

        output
    }

    pub fn completions(&self) -> String {
        let (uri, position) = self.cursor();

        let response = self.backend().on_completion(&CompletionParams {
            text_document_position: TextDocumentPositionParams::new(
                TextDocumentIdentifier::new(uri),
                position,
            ),
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
            context: None,
        });

        let items = match response {
            None => return "<none>\n".to_string(),
            Some(CompletionResponse::Array(items)) => items,
            Some(CompletionResponse::List(list)) => list.items,
        };

        let mut output = String::new();

        for item in items {
            let kind = item
                .kind
                .map_or_else(|| "?".to_string(), |kind| format!("{kind:?}"));

            let detail = item
                .label_details
                .and_then(|details| details.detail)
                .unwrap_or_default();

            writeln!(output, "{} [{kind}]{detail}", item.label).unwrap();
        }

        output
    }

    pub fn formatting(&self, name: &str) -> Result<Vec<TextEdit>, String> {
        self.backend()
            .on_formatting(&DocumentFormattingParams {
                text_document: TextDocumentIdentifier::new(self.uri(name)),
                options: FormattingOptions::default(),
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .map_err(|error| error.message.to_string())
    }

    /// Recompiles the project after a file changes, and renders the code actions for its
    /// diagnostics, along with the text of the file after applying each of them.
    pub fn code_actions(&self, name: &str, text: &str) -> String {
        let uri = self.uri(name);
        let diagnostics = self
            .backend()
            .on_change_impl(&uri, Some(text.to_string()))
            .into_iter()
            .find(|(kind, _)| Some(kind) == normalize_path(&self.path(name)).ok().as_ref())
            .map(|(_, diagnostics)| diagnostics)
            .unwrap_or_default();

        let actions = code_actions(&CodeActionParams {
            text_document: TextDocumentIdentifier::new(uri.clone()),
            range: Range::default(),
            context: CodeActionContext {
                diagnostics,
                only: None,
                trigger_kind: None,
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        })
        .unwrap_or_default();

        let mut output = String::new();

        for action in actions {
            let CodeActionOrCommand::CodeAction(action) = action else {
                continue;
            };

            let mut edits = action.edit.unwrap().changes.unwrap().remove(&uri).unwrap();
            edits.sort_by_key(|edit| (edit.range.start.line, edit.range.start.character));

            let mut fixed = text.to_string();

            for edit in edits.into_iter().rev() {
                let start = position_to_offset(text, edit.range.start);
                let end = position_to_offset(text, edit.range.end);
                fixed.replace_range(start..end, &edit.new_text);
            }

            writeln!(
                output,
                "{} (preferred: {}):\n{fixed}",
                action.title,
                action.is_preferred.unwrap()
            )
            .unwrap();
        }

        output
    }

    /// Recompiles the project after a file changes, and renders the resulting diagnostics.
    pub fn change(&self, name: &str, text: Option<&str>) -> String {
        let diagnostics = self
            .backend()
            .on_change_impl(&self.uri(name), text.map(ToString::to_string));

        // The order of files depends on the order of directory entries, so sort them.
        let mut diagnostics = diagnostics
            .into_iter()
            .filter_map(|(kind, diagnostics)| match kind {
                // Files are reported with the paths the client uses
                rue_diagnostic::SourceKind::File(_) => Some((
                    self.backend().paths.uri(&kind)?.to_file_path().ok()?,
                    diagnostics,
                )),
                rue_diagnostic::SourceKind::Std(_) => None,
            })
            .collect::<Vec<_>>();

        diagnostics.sort_by(|(a, _), (b, _)| a.cmp(b));

        let mut output = String::new();

        for (path, diagnostics) in diagnostics {
            let path = path.strip_prefix(&self.root).unwrap_or(&path);

            if diagnostics.is_empty() {
                writeln!(output, "{}: <none>", path.display()).unwrap();
            }

            for diagnostic in diagnostics {
                let code = match diagnostic.code {
                    Some(NumberOrString::String(code)) => format!("[{code}]"),
                    Some(NumberOrString::Number(code)) => format!("[{code}]"),
                    None => String::new(),
                };

                writeln!(
                    output,
                    "{}:{} {:?}{code}: {}",
                    path.display(),
                    range_string(diagnostic.range),
                    diagnostic.severity.unwrap(),
                    diagnostic.message
                )
                .unwrap();

                for related in diagnostic.related_information.unwrap_or_default() {
                    writeln!(
                        output,
                        "    {}: {}",
                        self.location(&related.location.uri, related.location.range),
                        related.message
                    )
                    .unwrap();
                }
            }
        }

        output
    }
}

fn position_of(text: &str, index: usize) -> Position {
    let before = &text[..index];
    let line = before.matches('\n').count();
    let line_start = before.rfind('\n').map_or(0, |i| i + 1);
    let character = before[line_start..].encode_utf16().count();
    Position::new(line as u32, character as u32)
}

fn range_string(range: Range) -> String {
    format!(
        "{}:{}-{}:{}",
        range.start.line, range.start.character, range.end.line, range.end.character
    )
}
