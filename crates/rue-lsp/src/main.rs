mod completion;

use std::collections::HashMap;
use std::sync::Arc;

use completion::CompletionProvider;
use rue_compiler::{analyze_file, analyze_file_with_context};
use rue_diagnostic::{Source, SourceKind};
use rue_options::CompilerOptions;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionList, CompletionOptions, CompletionParams, CompletionResponse,
    Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidOpenTextDocumentParams,
    InitializeParams, InitializeResult, InitializedParams, MessageType, Position, Range,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, TextEdit, Url,
    WorkDoneProgressOptions,
};
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug, Clone)]
struct DocumentState {
    text: String,
    version: i32,
    // Store pre-computed completions as fallback when compilation fails
    completions: Vec<CompletionItem>,
}

#[derive(Debug, Clone)]
struct Backend {
    client: Client,
    documents: Arc<RwLock<HashMap<Url, DocumentState>>>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                    ..Default::default()
                }),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let documents = self.documents.read().await;
        let doc_state = match documents.get(&uri) {
            Some(state) => state,
            None => return Ok(None),
        };

        // Extract the partial word and its range for text editing
        let (_partial_word, edit_range) = self.extract_partial_word(position, &doc_state.text);

        // Recompile to get fresh compiler access with scope information
        let source = Source::new(
            Arc::from(doc_state.text.as_str()),
            SourceKind::File(
                uri.to_file_path()
                    .unwrap_or_default()
                    .file_name()
                    .and_then(|n| n.to_str())
                    .unwrap_or("untitled.rue")
                    .to_string(),
            ),
        );

        // Try to get completions from fresh compilation with scope info
        if let Ok(ctx) = analyze_file_with_context(source, CompilerOptions::default()) {
            let provider = CompletionProvider::new(&ctx.compiler, ctx.std_scope, ctx.file_scope);
            let completions = provider.get_completions_at_position(
                position,
                &doc_state.text,
                Some(&ctx.scope_map),
            );
            let completions_with_edits = self.apply_text_edits(completions, edit_range);
            return Ok(Some(CompletionResponse::List(CompletionList {
                is_incomplete: true,
                items: completions_with_edits,
            })));
        }

        // Fallback to cached completions if compilation fails
        let completions = doc_state.completions.clone();
        let completions_with_edits = self.apply_text_edits(completions, edit_range);

        Ok(Some(CompletionResponse::List(CompletionList {
            is_incomplete: true,
            items: completions_with_edits,
        })))
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Rue language server initialized.")
            .await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.on_change(
            params.text_document.uri,
            params.text_document.text,
            params.text_document.version,
        )
        .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.on_change(
            params.text_document.uri,
            params.content_changes[0].text.clone(),
            params.text_document.version,
        )
        .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

impl Backend {
    /// Extract the partial word at the cursor position and return it with its range
    fn extract_partial_word(&self, position: Position, text: &str) -> (String, Range) {
        let lines: Vec<&str> = text.lines().collect();
        let line_idx = position.line as usize;
        let char_idx = position.character as usize;

        if line_idx >= lines.len() {
            return (
                String::new(),
                Range {
                    start: position,
                    end: position,
                },
            );
        }

        let current_line = lines[line_idx];

        // Find the start of the identifier (going backwards from cursor)
        let mut start_idx = char_idx;
        let chars: Vec<char> = current_line.chars().collect();

        while start_idx > 0 {
            let prev_char = chars.get(start_idx - 1);
            match prev_char {
                Some(ch) if ch.is_alphanumeric() || *ch == '_' => {
                    start_idx -= 1;
                }
                _ => break,
            }
        }

        // Find the end of the identifier (going forward from cursor)
        let mut end_idx = char_idx;
        while end_idx < chars.len() {
            let next_char = chars.get(end_idx);
            match next_char {
                Some(ch) if ch.is_alphanumeric() || *ch == '_' => {
                    end_idx += 1;
                }
                _ => break,
            }
        }

        // Extract the partial word
        let partial_word: String = chars[start_idx..char_idx].iter().collect();

        let range = Range {
            start: Position {
                line: position.line,
                character: start_idx as u32,
            },
            end: Position {
                line: position.line,
                character: end_idx as u32,
            },
        };

        (partial_word, range)
    }

    /// Apply text edits to completion items based on the replacement range
    fn apply_text_edits(
        &self,
        mut items: Vec<CompletionItem>,
        range: Range,
    ) -> Vec<CompletionItem> {
        for item in &mut items {
            item.text_edit = Some(tower_lsp::lsp_types::CompletionTextEdit::Edit(TextEdit {
                range,
                new_text: item.label.clone(),
            }));
        }
        items
    }

    async fn on_change(&self, uri: Url, text: String, version: i32) {
        let source = Source::new(
            Arc::from(text.as_str()),
            SourceKind::File(
                uri.to_file_path()
                    .unwrap_or_default()
                    .file_name()
                    .and_then(|n| n.to_str())
                    .unwrap_or("untitled.rue")
                    .to_string(),
            ),
        );

        // Try to get completions and diagnostics
        let (diagnostics, completions) =
            match analyze_file_with_context(source, CompilerOptions::default()) {
                Ok(ctx) => {
                    let diags = ctx.compilation.diagnostics.clone();

                    let provider =
                        CompletionProvider::new(&ctx.compiler, ctx.std_scope, ctx.file_scope);

                    // Pre-compute basic completions as fallback
                    let all_comps = provider.get_all_completions();

                    (diags, all_comps)
                }
                Err(_) => {
                    // Fallback to just diagnostics if full compilation fails
                    let source = Source::new(
                        Arc::from(text.as_str()),
                        SourceKind::File(
                            uri.to_file_path()
                                .unwrap_or_default()
                                .file_name()
                                .and_then(|n| n.to_str())
                                .unwrap_or("untitled.rue")
                                .to_string(),
                        ),
                    );
                    match analyze_file(source, CompilerOptions::default()) {
                        Ok(compilation) => (compilation.diagnostics, Vec::new()),
                        Err(_) => (Vec::new(), Vec::new()),
                    }
                }
            };

        // Update document cache
        let mut documents = self.documents.write().await;
        documents.insert(
            uri.clone(),
            DocumentState {
                text,
                version,
                completions,
            },
        );
        drop(documents);

        let lsp_diagnostics: Vec<Diagnostic> = diagnostics.iter().map(diagnostic).collect();

        self.client
            .publish_diagnostics(uri, lsp_diagnostics, None)
            .await;
    }
}

fn diagnostic(diagnostic: &rue_diagnostic::Diagnostic) -> Diagnostic {
    let start = diagnostic.start();
    let end = diagnostic.end();

    Diagnostic {
        range: Range {
            start: Position {
                line: start.line.try_into().unwrap(),
                character: start.col.try_into().unwrap(),
            },
            end: Position {
                line: end.line.try_into().unwrap(),
                character: end.col.try_into().unwrap(),
            },
        },
        message: diagnostic.kind.to_string(),
        severity: Some(match diagnostic.kind.severity() {
            rue_diagnostic::DiagnosticSeverity::Error => DiagnosticSeverity::ERROR,
            rue_diagnostic::DiagnosticSeverity::Warning => DiagnosticSeverity::WARNING,
        }),
        ..Diagnostic::default()
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        documents: Arc::new(RwLock::new(HashMap::new())),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}

#[cfg(test)]
mod tests {
    use super::*;
    use tower_lsp::lsp_types::{
        CompletionParams, PartialResultParams, TextDocumentIdentifier, TextDocumentPositionParams,
        WorkDoneProgressParams,
    };

    // Import shared test utilities
    // Note: These are in the `tests/` directory which is separate from `src/`
    // so we define local versions here for integration tests
    
    /// Parse cursor position from code with | marker
    /// Returns (code_without_marker, line, character)
    fn parse_cursor_position(code: &str) -> (String, u32, u32) {
        let mut line = 0u32;
        let mut character = 0u32;
        let mut found = false;
        let mut result = String::new();

        for (line_idx, line_text) in code.lines().enumerate() {
            if let Some(pos) = line_text.find('|') {
                // Found the cursor marker
                line = line_idx as u32;
                character = pos as u32;
                found = true;
                // Remove the | from this line
                result.push_str(&line_text.replace('|', ""));
                result.push('\n');
            } else {
                result.push_str(line_text);
                result.push('\n');
            }
        }

        if !found {
            panic!("No cursor marker | found in test code");
        }

        // Remove trailing newline if original didn't have one
        if !code.ends_with('\n') && result.ends_with('\n') {
            result.pop();
        }

        (result, line, character)
    }

    /// Create a test backend instance
    fn create_test_backend() -> Backend {
        // Create a service and extract the inner Backend
        let (service, _socket) = tower_lsp::LspService::build(|client| Backend {
            client,
            documents: Arc::new(RwLock::new(HashMap::new())),
        })
        .finish();
        
        // Extract the inner Backend from the service
        service.inner().clone()
    }

    /// Helper to create a test URI
    fn test_uri() -> Url {
        Url::parse("file:///test.rue").unwrap()
    }

    /// Helper to create completion params from code with | cursor marker
    fn completion_params_with_cursor(code: &str) -> (CompletionParams, String) {
        let (code_without_marker, line, character) = parse_cursor_position(code);
        let uri = test_uri();
        
        let params = CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: Position { line, character },
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
            context: None,
        };

        (params, code_without_marker)
    }

    #[tokio::test]
    async fn test_scope_aware_completion_with_local_binding() {
        let backend = create_test_backend();
        let uri = test_uri();

        // Open a document with a local binding
        let code = r#"
fn main() -> Int {
    let my_value = 42;
    m|
}
"#;

        let (params, code_without_marker) = completion_params_with_cursor(code);

        // Simulate document open
        {
            let mut documents = backend.documents.write().await;
            documents.insert(
                uri.clone(),
                DocumentState {
                    text: code_without_marker,
                    version: 1,
                    completions: Vec::new(),
                },
            );
        }

        // Request completions at the cursor (should include 'my_value')
        let result = backend.completion(params).await.unwrap();

        assert!(result.is_some());
        if let Some(CompletionResponse::List(list)) = result {
            let labels: Vec<_> = list.items.iter().map(|item| &item.label).collect();
            assert!(
                labels.iter().any(|l| l.as_str() == "my_value"),
                "Expected 'my_value' in completions, got: {:?}",
                labels
            );
        } else {
            panic!("Expected CompletionList response");
        }
    }

    #[tokio::test]
    async fn test_scope_aware_completion_with_function_parameter() {
        let backend = create_test_backend();
        let uri = test_uri();

        // Open a document with function parameters
        let code = r#"
fn add(first_num: Int, second_num: Int) -> Int {
    fir|
}
"#;

        let (params, code_without_marker) = completion_params_with_cursor(code);

        {
            let mut documents = backend.documents.write().await;
            documents.insert(
                uri.clone(),
                DocumentState {
                    text: code_without_marker,
                    version: 1,
                    completions: Vec::new(),
                },
            );
        }

        // Request completions at cursor (should include 'first_num')
        let result = backend.completion(params).await.unwrap();

        assert!(result.is_some());
        if let Some(CompletionResponse::List(list)) = result {
            let labels: Vec<_> = list.items.iter().map(|item| &item.label).collect();
            assert!(
                labels.iter().any(|l| l.as_str() == "first_num"),
                "Expected 'first_num' in completions, got: {:?}",
                labels
            );
        } else {
            panic!("Expected CompletionList response");
        }
    }

    #[tokio::test]
    async fn test_scope_aware_field_completion_on_pair() {
        let backend = create_test_backend();
        let uri = test_uri();

        // Open a document with a pair variable
        let code = r#"
fn main() -> Int {
    let my_pair: (Int, Int) = (1, 2);
    my_pair.|
}
"#;

        let (params, code_without_marker) = completion_params_with_cursor(code);

        {
            let mut documents = backend.documents.write().await;
            documents.insert(
                uri.clone(),
                DocumentState {
                    text: code_without_marker,
                    version: 1,
                    completions: Vec::new(),
                },
            );
        }

        // Request completions after the dot (should include 'first' and 'rest')
        let result = backend.completion(params).await.unwrap();

        assert!(result.is_some());
        if let Some(CompletionResponse::List(list)) = result {
            let labels: Vec<_> = list.items.iter().map(|item| &item.label).collect();
            assert!(
                labels.iter().any(|l| l.as_str() == "first"),
                "Expected 'first' in completions, got: {:?}",
                labels
            );
            assert!(
                labels.iter().any(|l| l.as_str() == "rest"),
                "Expected 'rest' in completions, got: {:?}",
                labels
            );
        } else {
            panic!("Expected CompletionList response");
        }
    }

    #[tokio::test]
    async fn test_scope_aware_completion_nested_scopes() {
        let backend = create_test_backend();
        let uri = test_uri();

        // Open a document with nested scopes
        let code = r#"
fn main() -> Int {
    let outer = 1;
    if true {
        let inner = 2;
        |
    }
}
"#;

        let (params, code_without_marker) = completion_params_with_cursor(code);

        {
            let mut documents = backend.documents.write().await;
            documents.insert(
                uri.clone(),
                DocumentState {
                    text: code_without_marker,
                    version: 1,
                    completions: Vec::new(),
                },
            );
        }

        // Request completions inside the if block (should include both 'outer' and 'inner')
        let result = backend.completion(params).await.unwrap();

        assert!(result.is_some());
        if let Some(CompletionResponse::List(list)) = result {
            let labels: Vec<_> = list.items.iter().map(|item| &item.label).collect();
            assert!(
                labels.iter().any(|l| l.as_str() == "outer"),
                "Expected 'outer' in completions, got: {:?}",
                labels
            );
            assert!(
                labels.iter().any(|l| l.as_str() == "inner"),
                "Expected 'inner' in completions, got: {:?}",
                labels
            );
        } else {
            panic!("Expected CompletionList response");
        }
    }

    #[tokio::test]
    async fn test_scope_aware_completion_shadowing() {
        let backend = create_test_backend();
        let uri = test_uri();

        // Open a document with shadowing
        let code = r#"
fn main() -> Int {
    let value = 1;
    if true {
        let value = 2;
        val|
    }
}
"#;

        let (params, code_without_marker) = completion_params_with_cursor(code);

        {
            let mut documents = backend.documents.write().await;
            documents.insert(
                uri.clone(),
                DocumentState {
                    text: code_without_marker,
                    version: 1,
                    completions: Vec::new(),
                },
            );
        }

        // Request completions inside the if block
        // Should include 'value' (the inner one shadows the outer)
        let result = backend.completion(params).await.unwrap();

        assert!(result.is_some());
        if let Some(CompletionResponse::List(list)) = result {
            let labels: Vec<_> = list.items.iter().map(|item| &item.label).collect();
            // Should have exactly one 'value' (not duplicated)
            let value_count = labels.iter().filter(|l| l.as_str() == "value").count();
            assert_eq!(
                value_count, 1,
                "Expected exactly one 'value' in completions (shadowing), got {} instances in {:?}",
                value_count, labels
            );
        } else {
            panic!("Expected CompletionList response");
        }
    }

    #[tokio::test]
    async fn test_completion_fallback_on_invalid_syntax() {
        let backend = create_test_backend();
        let uri = test_uri();

        // Open a document with invalid syntax
        let code = r#"
fn main( {
    let broken|
}
"#;

        let (params, code_without_marker) = completion_params_with_cursor(code);

        {
            let mut documents = backend.documents.write().await;
            documents.insert(
                uri.clone(),
                DocumentState {
                    text: code_without_marker,
                    version: 1,
                    completions: Vec::new(),
                },
            );
        }

        // Request completions - should not crash and return some completions
        let result = backend.completion(params).await.unwrap();

        // Should return some result (even if just fallback completions)
        assert!(result.is_some());
    }

    #[tokio::test]
    async fn test_type_position_completions() {
        let backend = create_test_backend();
        let uri = test_uri();

        // Open a document where we're typing a type annotation
        let code = r#"
fn main(value: |) -> Int {
    42
}
"#;

        let (params, code_without_marker) = completion_params_with_cursor(code);

        {
            let mut documents = backend.documents.write().await;
            documents.insert(
                uri.clone(),
                DocumentState {
                    text: code_without_marker,
                    version: 1,
                    completions: Vec::new(),
                },
            );
        }

        // Request completions after the colon (type position)
        let result = backend.completion(params).await.unwrap();

        assert!(result.is_some());
        if let Some(CompletionResponse::List(list)) = result {
            let labels: Vec<_> = list.items.iter().map(|item| &item.label).collect();
            // Should include type names like Int, Bool, Bytes, etc.
            assert!(
                labels.iter().any(|l| l.as_str() == "Int"),
                "Expected 'Int' in type completions, got: {:?}",
                labels
            );
            assert!(
                labels.iter().any(|l| l.as_str() == "Bool"),
                "Expected 'Bool' in type completions, got: {:?}",
                labels
            );
        } else {
            panic!("Expected CompletionList response");
        }
    }

    #[tokio::test]
    async fn test_top_level_completions() {
        let backend = create_test_backend();
        let uri = test_uri();

        // Open a document at top level
        let code = r#"
|
"#;

        let (params, code_without_marker) = completion_params_with_cursor(code);

        {
            let mut documents = backend.documents.write().await;
            documents.insert(
                uri.clone(),
                DocumentState {
                    text: code_without_marker,
                    version: 1,
                    completions: Vec::new(),
                },
            );
        }

        // Request completions at top level
        let result = backend.completion(params).await.unwrap();

        assert!(result.is_some());
        if let Some(CompletionResponse::List(list)) = result {
            let labels: Vec<_> = list.items.iter().map(|item| &item.label).collect();
            // Should include top-level keywords
            assert!(
                labels.iter().any(|l| l.as_str() == "fn"),
                "Expected 'fn' in top-level completions, got: {:?}",
                labels
            );
            assert!(
                labels.iter().any(|l| l.as_str() == "const"),
                "Expected 'const' in top-level completions, got: {:?}",
                labels
            );
            assert!(
                labels.iter().any(|l| l.as_str() == "export"),
                "Expected 'export' in top-level completions, got: {:?}",
                labels
            );
        } else {
            panic!("Expected CompletionList response");
        }
    }
}
