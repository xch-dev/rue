mod ambiguity;
mod background;
mod completion;
mod diagnostics;
mod formatting;
mod hover;
mod inlay_hints;
mod lifecycle;
mod line_endings;
mod navigation;
mod protocol;
mod rename;
mod shadowing;
mod signature_help;
mod std_library;
mod sweep;
mod syntax_errors;
mod workspace;

use std::fmt::Write;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};

use futures::executor::block_on;
use rue_compiler::normalize_path;
use rue_diagnostic::LineIndex;
use tower_lsp_server::LanguageServer;
use tower_lsp_server::LspService;
use tower_lsp_server::ls_types::{
    CodeActionContext, CodeActionOrCommand, CodeActionParams, CompletionParams, CompletionResponse,
    Diagnostic, DocumentChangeOperation, DocumentChanges, DocumentFormattingParams,
    FormattingOptions, GotoDefinitionParams, GotoDefinitionResponse, HoverContents, HoverParams,
    InlayHintLabel, InlayHintParams, InlayHintTooltip, Location, MarkedString, NumberOrString,
    OneOf, ParameterLabel, PartialResultParams, Position, PrepareRenameResponse, Range,
    ReferenceContext, ReferenceParams, RenameParams, ResourceOp, SignatureHelpParams,
    TextDocumentIdentifier, TextDocumentPositionParams, TextEdit, Uri, WorkDoneProgressParams,
    WorkspaceEdit,
};

use crate::paths::{path_to_uri, uri_to_path};
use crate::position::{offset_to_position, position_to_offset};
use crate::{Backend, code_actions};

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

/// Blocking versions of the request handlers, for tests that call the server directly.
impl Backend {
    fn on_hover(&self, params: &HoverParams) -> Option<tower_lsp_server::ls_types::Hover> {
        block_on(self.hover(params.clone())).unwrap()
    }

    fn on_goto_definition(&self, params: &GotoDefinitionParams) -> Option<GotoDefinitionResponse> {
        block_on(self.goto_definition(params.clone())).unwrap()
    }

    fn on_references(&self, params: &ReferenceParams) -> Option<Vec<Location>> {
        block_on(self.references(params.clone())).unwrap()
    }

    fn on_completion(&self, params: &CompletionParams) -> Option<CompletionResponse> {
        block_on(self.completion(params.clone())).unwrap()
    }

    /// Makes the next compile wait once it has started. The first channel says when it started,
    /// and sending to the second lets it continue.
    fn hold_compile(&self) -> (std::sync::mpsc::Receiver<()>, std::sync::mpsc::Sender<()>) {
        let (started, started_receiver) = std::sync::mpsc::channel();
        let (release_sender, release) = std::sync::mpsc::channel();
        *crate::lock(&self.shared.gate) = Some(crate::worker::Gate { started, release });
        (started_receiver, release_sender)
    }
}

/// A temporary project directory on disk, along with a language server instance that has
/// opened its files. Any `$0` marker in a file's text is stripped and recorded as the cursor.
pub struct TestProject {
    root: PathBuf,
    _dir: TempDir,
    service: LspService<Backend>,
    cursor: Option<(Uri, Position)>,
    /// The name and text of each file, without the cursor.
    files: Vec<(String, String)>,
}

impl TestProject {
    /// Creates a project that doesn't use the standard library, so that results only include
    /// what's declared in the test itself.
    pub fn new(files: &[(&str, &str)]) -> Self {
        Self::with_manifest(Some(MANIFEST), files)
    }

    pub fn with_manifest(manifest: Option<&str>, files: &[(&str, &str)]) -> Self {
        let dir = TempDir::new("rue-lsp-test");

        if let Some(manifest) = manifest {
            fs::write(dir.path().join("Rue.toml"), manifest).unwrap();
        }

        Self::in_dir(dir, files)
    }

    /// Creates a project in a directory that may already have files in it, such as a manifest.
    pub fn in_dir(dir: TempDir, files: &[(&str, &str)]) -> Self {
        let root = dir.path().to_path_buf();

        let mut cursor = None;
        let mut texts = Vec::new();

        for (name, text) in files {
            let path = root.join(name);
            fs::create_dir_all(path.parent().unwrap()).unwrap();

            let text = if let Some(index) = text.find(CURSOR) {
                assert!(cursor.is_none(), "only one cursor is allowed");
                cursor = Some((
                    path_to_uri(&path).unwrap(),
                    offset_to_position(&LineIndex::with_carriage_returns(text), text, index),
                ));
                text.replacen(CURSOR, "", 1)
            } else {
                (*text).to_string()
            };

            fs::write(&path, &text).unwrap();
            texts.push((path, text));
        }

        let files = files
            .iter()
            .zip(&texts)
            .map(|((name, _), (_, text))| ((*name).to_string(), text.clone()))
            .collect();

        let (service, _) = LspService::new(Backend::new);

        let project = Self {
            root,
            _dir: dir,
            service,
            cursor,
            files,
        };

        // Opening any file compiles the whole project, but each file is opened to mirror what an
        // editor would do and to populate the in-memory file cache.
        for (path, text) in texts {
            project
                .backend()
                .on_change_impl(&path_to_uri(&path).unwrap(), Some(text));
        }

        project
    }

    pub fn backend(&self) -> &Backend {
        self.service.inner()
    }

    pub fn path(&self, name: &str) -> PathBuf {
        self.root.join(name)
    }

    pub fn uri(&self, name: &str) -> Uri {
        path_to_uri(&self.path(name)).unwrap()
    }

    pub fn cursor(&self) -> (Uri, Position) {
        self.cursor.clone().expect("missing $0 cursor marker")
    }

    /// Renders a URI and range relative to the project root, e.g. `main.rue:1:4-1:7`.
    pub fn location(&self, uri: &Uri, range: Range) -> String {
        let path = uri_to_path(uri).unwrap();
        let path = path.strip_prefix(&self.root).unwrap_or(&path);
        format!("{}:{}", path.display(), range_string(range))
    }

    pub fn hover(&self) -> String {
        let (uri, position) = self.cursor();
        hover(self.backend(), uri, position)
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
        self.references_with(true)
    }

    pub fn references_with(&self, include_declaration: bool) -> String {
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
                    include_declaration,
                },
            })
            .unwrap_or_default();

        self.locations(&locations)
    }

    /// Renders the hover, definitions and references at the cursor together, for tests that
    /// check that all of them agree on which item the cursor is on.
    pub fn queries(&self) -> String {
        format!(
            "hover:\n{}definitions:\n{}references:\n{}",
            self.hover(),
            self.definitions(),
            self.references()
        )
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

    /// Renders the file with its inlay hints written into it, between `«` and `»`, followed by the
    /// tooltips of the hints that have one.
    pub fn inlay_hints(&self, name: &str) -> String {
        let text = &self.text(name);
        let end = offset_to_position(&LineIndex::with_carriage_returns(text), text, text.len());
        self.inlay_hints_in(name, Range::new(Position::new(0, 0), end))
    }

    /// Renders the file with the inlay hints of a range, like [`Self::inlay_hints`].
    pub fn inlay_hints_in(&self, name: &str, range: Range) -> String {
        let text = &self.text(name);

        let hints = block_on(self.backend().inlay_hint(InlayHintParams {
            text_document: TextDocumentIdentifier::new(self.uri(name)),
            range,
            work_done_progress_params: WorkDoneProgressParams::default(),
        }))
        .unwrap()
        .expect("the server answers for a file that it compiled");

        let lines = LineIndex::with_carriage_returns(text);
        let mut output = text.clone();
        let mut last = None;

        // The hints are in order, so they're written from the end of the text, which keeps the
        // positions of the earlier ones valid
        for hint in hints.iter().rev() {
            assert!(
                last.is_none_or(|last| hint.position <= last),
                "hints are in order"
            );
            last = Some(hint.position);

            let InlayHintLabel::String(label) = &hint.label else {
                panic!("unexpected label: {:?}", hint.label);
            };

            // A hint that's padded is shown with a space, as the editor shows it
            let label = if hint.padding_right == Some(true) {
                format!("«{label}» ")
            } else {
                format!("«{label}»")
            };

            let offset = position_to_offset(&lines, text, hint.position);
            assert_eq!(offset_to_position(&lines, text, offset), hint.position);
            output.insert_str(offset, &label);
        }

        if !output.ends_with('\n') {
            output.push('\n');
        }

        for hint in hints {
            let InlayHintLabel::String(label) = hint.label else {
                unreachable!();
            };

            match hint.tooltip {
                None => {}
                Some(InlayHintTooltip::String(tooltip)) => {
                    writeln!(output, "tooltip of «{label}»: {tooltip}").unwrap();
                }
                Some(tooltip) => panic!("unexpected tooltip: {tooltip:?}"),
            }
        }

        output
    }

    /// The text of a file, as the project was created with it.
    fn text(&self, name: &str) -> String {
        let (_, text) = self
            .files
            .iter()
            .find(|(file, _)| file == name)
            .expect("missing file");

        text.clone()
    }

    /// Renders the signatures at the cursor, with the active parameter of each in brackets and
    /// the active signature marked, or `<none>`.
    pub fn signature_help(&self) -> String {
        let (uri, position) = self.cursor();

        let Some(help) = block_on(self.backend().signature_help(SignatureHelpParams {
            context: None,
            text_document_position_params: TextDocumentPositionParams::new(
                TextDocumentIdentifier::new(uri),
                position,
            ),
            work_done_progress_params: WorkDoneProgressParams::default(),
        }))
        .unwrap() else {
            return "<none>\n".to_string();
        };

        // The active parameter is one of the signature's
        for signature in &help.signatures {
            assert!(
                signature.active_parameter.is_none_or(
                    |active| (active as usize) < signature.parameters.as_ref().unwrap().len()
                ),
                "{signature:?}"
            );
        }

        let active = help.active_signature.unwrap() as usize;
        assert_eq!(
            help.active_parameter,
            help.signatures[active].active_parameter
        );

        let mut output = String::new();

        for (i, signature) in help.signatures.into_iter().enumerate() {
            // The parameters are parts of the label, in UTF-16 code units
            let units = signature.label.encode_utf16().collect::<Vec<_>>();
            let text = |range: std::ops::Range<u32>| {
                String::from_utf16(&units[range.start as usize..range.end as usize]).unwrap()
            };

            let mut label = String::new();
            let mut end = 0;

            for (j, parameter) in signature.parameters.unwrap().into_iter().enumerate() {
                let ParameterLabel::LabelOffsets([start, stop]) = parameter.label else {
                    panic!("unexpected label: {:?}", parameter.label);
                };

                assert!(end <= start && start < stop, "parameters are in order");

                label.push_str(&text(end..start));

                if signature.active_parameter == Some(j as u32) {
                    write!(label, "[{}]", text(start..stop)).unwrap();
                } else {
                    label.push_str(&text(start..stop));
                }

                end = stop;
            }

            label.push_str(&text(end..units.len() as u32));

            let marker = if i == active { ">" } else { " " };
            writeln!(output, "{marker} {label}").unwrap();
        }

        output
    }

    /// Renders the diagnostics of the project as it was created.
    pub fn diagnostics(&self) -> String {
        let (name, text) = &self.files[0];
        self.change(name, Some(text))
    }

    /// Renders the name at the cursor that would be renamed, or why it can't be.
    pub fn prepare_rename(&self) -> String {
        let (uri, position) = self.cursor();

        let response = block_on(
            self.backend()
                .prepare_rename(TextDocumentPositionParams::new(
                    TextDocumentIdentifier::new(uri.clone()),
                    position,
                )),
        );

        match response {
            Ok(None) => "<none>\n".to_string(),
            Ok(Some(PrepareRenameResponse::RangeWithPlaceholder { range, placeholder })) => {
                format!("{} `{placeholder}`\n", self.location(&uri, range))
            }
            Ok(Some(response)) => panic!("unexpected response: {response:?}"),
            Err(error) => format!("error: {}\n", error.message),
        }
    }

    /// Renames what's at the cursor, and renders each file that changed with the edits applied to
    /// it, along with the file that's renamed, or why it can't be renamed.
    pub fn rename(&self, new_name: &str) -> String {
        let (uri, position) = self.cursor();

        let response = block_on(self.backend().rename(RenameParams {
            text_document_position: TextDocumentPositionParams::new(
                TextDocumentIdentifier::new(uri),
                position,
            ),
            new_name: new_name.to_string(),
            work_done_progress_params: WorkDoneProgressParams::default(),
        }));

        match response {
            Ok(None) => "<none>\n".to_string(),
            Ok(Some(edit)) => self.render_workspace_edit(edit),
            Err(error) => format!("error: {}\n", error.message),
        }
    }

    fn render_workspace_edit(&self, edit: WorkspaceEdit) -> String {
        let mut changes = edit.changes.unwrap_or_default();
        let mut renamed = Vec::new();

        match edit.document_changes {
            None => {}
            Some(DocumentChanges::Edits(edits)) => panic!("unexpected edits: {edits:?}"),
            Some(DocumentChanges::Operations(operations)) => {
                for operation in operations {
                    match operation {
                        DocumentChangeOperation::Edit(edit) => {
                            assert!(
                                renamed.is_empty(),
                                "files are edited before they're renamed"
                            );

                            changes.entry(edit.text_document.uri).or_default().extend(
                                edit.edits.into_iter().map(|edit| match edit {
                                    OneOf::Left(edit) => edit,
                                    OneOf::Right(edit) => edit.text_edit,
                                }),
                            );
                        }
                        DocumentChangeOperation::Op(ResourceOp::Rename(rename)) => {
                            renamed.push(rename);
                        }
                        DocumentChangeOperation::Op(operation) => {
                            panic!("unexpected operation: {operation:?}")
                        }
                    }
                }
            }
        }

        let mut output = String::new();

        for (name, text) in &self.files {
            let Some(edits) = changes.remove(&self.uri(name)) else {
                continue;
            };

            // Each edit replaces a name, so the ranges are shown as what they replace
            for edit in &edits {
                assert_eq!(edit.range.start.line, edit.range.end.line);
            }

            writeln!(output, "{name}:\n{}", apply_edits(text, edits).trim_end()).unwrap();
        }

        assert!(changes.is_empty(), "edits to unexpected files: {changes:?}");

        for rename in renamed {
            let relative = |uri: &Uri| {
                let path = uri_to_path(uri).unwrap();
                path.strip_prefix(&self.root).unwrap().display().to_string()
            };

            writeln!(
                output,
                "renamed: {} -> {}",
                relative(&rename.old_uri),
                relative(&rename.new_uri)
            )
            .unwrap();
        }

        if output.is_empty() {
            output.push_str("<no changes>\n");
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
        let diagnostics = self
            .backend()
            .on_change_impl(&self.uri(name), Some(text.to_string()))
            .into_iter()
            .find(|(kind, _)| Some(kind) == normalize_path(&self.path(name)).ok().as_ref())
            .map(|(_, diagnostics)| diagnostics)
            .unwrap_or_default();

        self.render_code_actions(diagnostics, &[(name, text)])
    }

    /// Renders the code actions for diagnostics, along with the text of each of the files after
    /// applying the edits of each action to it. Every edit must be to one of the files.
    pub fn render_code_actions(
        &self,
        diagnostics: Vec<Diagnostic>,
        files: &[(&str, &str)],
    ) -> String {
        let actions = code_actions(&CodeActionParams {
            text_document: TextDocumentIdentifier::new(self.uri(files[0].0)),
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
                panic!("unexpected command: {action:?}");
            };

            writeln!(
                output,
                "{} (preferred: {}):",
                action.title,
                action.is_preferred.unwrap()
            )
            .unwrap();

            let mut changes = action.edit.unwrap().changes.unwrap();

            for (name, text) in files {
                let Some(edits) = changes.remove(&self.uri(name)) else {
                    continue;
                };

                let fixed = apply_edits(text, edits);

                if files.len() > 1 {
                    writeln!(output, "{name}:").unwrap();
                }

                // Carriage returns are shown, since they'd be invisible in the snapshot
                writeln!(output, "{}", fixed.replace('\r', "\\r")).unwrap();
            }

            assert!(changes.is_empty(), "edits to unexpected files: {changes:?}");
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
                    self.backend()
                        .shared
                        .paths
                        .uri(&kind)
                        .and_then(|uri| uri_to_path(&uri))?,
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

/// Applies edits to a text. The edits don't overlap, so they're applied from the end of the text,
/// which keeps the positions of the earlier ones valid.
fn apply_edits(text: &str, mut edits: Vec<TextEdit>) -> String {
    edits.sort_by_key(|edit| edit.range.start);

    for pair in edits.windows(2) {
        assert!(pair[0].range.end <= pair[1].range.start, "edits overlap");
    }

    let mut fixed = text.to_string();
    let lines = LineIndex::with_carriage_returns(text);

    for edit in edits.into_iter().rev() {
        let start = position_to_offset(&lines, text, edit.range.start);
        let end = position_to_offset(&lines, text, edit.range.end);
        fixed.replace_range(start..end, &edit.new_text);
    }

    fixed
}

/// Renders the hover at a position, or `<none>`.
fn hover(backend: &Backend, uri: Uri, position: Position) -> String {
    let Some(hover) = backend.on_hover(&HoverParams {
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

fn range_string(range: Range) -> String {
    format!(
        "{}:{}-{}:{}",
        range.start.line, range.start.character, range.end.line, range.end.character
    )
}
