mod completion;
mod diagnostics;
mod formatting;
mod hover;
mod navigation;
mod protocol;

use std::fmt::Write;
use std::fs;
use std::path::PathBuf;
use std::sync::atomic::{AtomicUsize, Ordering};

use tower_lsp::LspService;
use tower_lsp::lsp_types::{
    CompletionParams, CompletionResponse, DocumentFormattingParams, FormattingOptions,
    GotoDefinitionParams, GotoDefinitionResponse, HoverContents, HoverParams, Location,
    MarkedString, NumberOrString, PartialResultParams, Position, Range, ReferenceContext,
    ReferenceParams, TextDocumentIdentifier, TextDocumentPositionParams, TextEdit, Url,
    WorkDoneProgressParams,
};

use crate::Backend;

const CURSOR: &str = "$0";

/// A temporary project directory on disk, along with a language server instance that has
/// opened its files. Any `$0` marker in a file's text is stripped and recorded as the cursor.
pub struct TestProject {
    root: PathBuf,
    service: LspService<Backend>,
    cursor: Option<(Url, Position)>,
}

impl TestProject {
    /// Creates a project that doesn't use the standard library, so that results only include
    /// what's declared in the test itself.
    pub fn new(files: &[(&str, &str)]) -> Self {
        Self::with_manifest(Some("[compiler]\nentrypoint = \".\"\nstd = false\n"), files)
    }

    pub fn with_manifest(manifest: Option<&str>, files: &[(&str, &str)]) -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);

        let root = std::env::temp_dir().join(format!(
            "rue-lsp-test-{}-{}",
            std::process::id(),
            COUNTER.fetch_add(1, Ordering::SeqCst)
        ));

        if root.exists() {
            fs::remove_dir_all(&root).unwrap();
        }

        fs::create_dir_all(&root).unwrap();

        // Canonicalize so that paths match the ones produced by the compiler (on macOS, the temp
        // directory is behind a symlink).
        let root = root.canonicalize().unwrap();

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

    /// Recompiles the project after a file changes, and renders the resulting diagnostics.
    pub fn change(&self, name: &str, text: Option<&str>) -> String {
        let diagnostics = self
            .backend()
            .on_change_impl(&self.uri(name), text.map(ToString::to_string));

        // The order of files depends on the order of directory entries, so sort them.
        let mut diagnostics = diagnostics
            .into_iter()
            .filter_map(|(kind, diagnostics)| match kind {
                rue_diagnostic::SourceKind::File(path) => Some((PathBuf::from(path), diagnostics)),
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
            }
        }

        output
    }
}

impl Drop for TestProject {
    fn drop(&mut self) {
        fs::remove_dir_all(&self.root).ok();
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
