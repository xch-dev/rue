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
    // Store pre-computed completions (all completions)
    completions: Vec<CompletionItem>,
    // Store compilation context for context-aware completions
    context_data: Option<ContextData>,
}

#[derive(Debug, Clone)]
struct ContextData {
    all_completions: Vec<CompletionItem>,
    type_completions: Vec<CompletionItem>,
    value_completions: Vec<CompletionItem>,
    toplevel_completions: Vec<CompletionItem>,
}

#[derive(Debug)]
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

        // Determine the completion context first
        let context = self.determine_context(position, &doc_state.text);

        // Handle field completions (after dot)
        if context == completion::CompletionContext::AfterDot {
            // Recompile to get compiler access for field completion
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

            if let Ok(ctx) = analyze_file_with_context(source, CompilerOptions::default()) {
                // Use the scope_map from THIS compilation context, not from doc_state
                // Find the scope at the cursor position
                if let Some(scope_id) = ctx.scope_map.find_scope_at_position(
                    position.line,
                    position.character,
                    &doc_state.text,
                ) {
                    let scope_chain = ctx.scope_map.get_scope_chain(scope_id);
                    let provider =
                        CompletionProvider::new(&ctx.compiler, ctx.std_scope, ctx.file_scope);

                    // Use CompletionProvider's field completion method
                    if let Some(completions) = provider.get_field_completions_with_scope(
                        position,
                        &doc_state.text,
                        &scope_chain,
                    ) {
                        let completions_with_edits = self.apply_text_edits(completions, edit_range);
                        return Ok(Some(CompletionResponse::List(CompletionList {
                            is_incomplete: true,
                            items: completions_with_edits,
                        })));
                    }
                }
            }
        }

        // Handle TypePosition context - always recompile to get fresh type completions
        if context == completion::CompletionContext::TypePosition {
            // Recompile to get compiler access for type completions
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

            if let Ok(ctx) = analyze_file_with_context(source, CompilerOptions::default()) {
                let provider =
                    CompletionProvider::new(&ctx.compiler, ctx.std_scope, ctx.file_scope);

                let completions = provider.get_type_completions();
                let completions_with_edits = self.apply_text_edits(completions, edit_range);
                return Ok(Some(CompletionResponse::List(CompletionList {
                    is_incomplete: true,
                    items: completions_with_edits,
                })));
            }
        }

        // Try scope-aware completions for expression/value contexts
        match context {
            completion::CompletionContext::ExpressionValue
            | completion::CompletionContext::Unknown => {
                // Recompile to get compiler access for scope-aware completions
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

                if let Ok(ctx) = analyze_file_with_context(source, CompilerOptions::default()) {
                    // Use the scope_map from THIS compilation context, not from doc_state
                    if let Some(scope_id) = ctx.scope_map.find_scope_at_position(
                        position.line,
                        position.character,
                        &doc_state.text,
                    ) {
                        // Get the scope chain (innermost to outermost)
                        let scope_chain = ctx.scope_map.get_scope_chain(scope_id);

                        let provider =
                            CompletionProvider::new(&ctx.compiler, ctx.std_scope, ctx.file_scope);

                        let completions = provider.get_scope_completions(&scope_chain);
                        let completions_with_edits = self.apply_text_edits(completions, edit_range);
                        return Ok(Some(CompletionResponse::List(CompletionList {
                            is_incomplete: true,
                            items: completions_with_edits,
                        })));
                    }
                }
            }
            _ => {
                // For other contexts (TopLevel), use pre-computed completions
            }
        }

        // Fallback to context-aware completions if available
        let completions = if let Some(context_data) = &doc_state.context_data {
            self.get_context_aware_completions(position, &doc_state.text, context_data)
        } else {
            // Final fallback to all completions
            doc_state.completions.clone()
        };

        // Apply text edits to all completions
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

    /// Determine context and return appropriate completions
    fn get_context_aware_completions(
        &self,
        position: Position,
        text: &str,
        context_data: &ContextData,
    ) -> Vec<CompletionItem> {
        use completion::CompletionContext;

        let context = self.determine_context(position, text);

        match context {
            CompletionContext::TypePosition => context_data.type_completions.clone(),
            CompletionContext::TopLevel => context_data.toplevel_completions.clone(),
            CompletionContext::ExpressionValue => context_data.value_completions.clone(),
            CompletionContext::AfterDot | CompletionContext::Unknown => {
                context_data.all_completions.clone()
            }
        }
    }

    /// Determine the completion context from cursor position
    fn determine_context(&self, position: Position, text: &str) -> completion::CompletionContext {
        use completion::CompletionContext;

        let lines: Vec<&str> = text.lines().collect();
        let line_idx = position.line as usize;
        let char_idx = position.character as usize;

        if line_idx >= lines.len() {
            return CompletionContext::Unknown;
        }

        let current_line = lines[line_idx];
        let before_cursor = if char_idx <= current_line.len() {
            &current_line[..char_idx]
        } else {
            current_line
        };

        // Check if we're after a dot
        if before_cursor.trim_end().ends_with('.') {
            return CompletionContext::AfterDot;
        }

        // Check if we're in a type position
        if Self::is_in_type_position(before_cursor) {
            return CompletionContext::TypePosition;
        }

        // Check if we're at top level
        let trimmed = before_cursor.trim_start();
        if trimmed.is_empty()
            || trimmed.starts_with("export")
            || trimmed.starts_with("fn")
            || trimmed.starts_with("const")
        {
            return CompletionContext::TopLevel;
        }

        // Check if we're likely in an expression
        if before_cursor.contains('=') || before_cursor.contains('(') {
            return CompletionContext::ExpressionValue;
        }

        CompletionContext::Unknown
    }

    /// Check if position is in type annotation context
    fn is_in_type_position(text_before_cursor: &str) -> bool {
        // Check for ": " pattern
        if text_before_cursor.ends_with(": ") {
            return true;
        }

        // Check for "-> " pattern
        if text_before_cursor.ends_with("-> ") {
            return true;
        }

        // Check for incomplete type annotation
        let trimmed = text_before_cursor.trim_end();
        if trimmed.ends_with(':') && !trimmed.ends_with("::") {
            return true;
        }

        false
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

        // Try to get full context for completions
        let (diagnostics, completions, context_data) =
            match analyze_file_with_context(source, CompilerOptions::default()) {
                Ok(ctx) => {
                    let diags = ctx.compilation.diagnostics.clone();

                    let provider =
                        CompletionProvider::new(&ctx.compiler, ctx.std_scope, ctx.file_scope);

                    // Pre-compute different types of completions for context-aware filtering
                    let all_comps = provider.get_all_completions();
                    let context = ContextData {
                        all_completions: all_comps.clone(),
                        type_completions: provider.get_type_completions(),
                        value_completions: provider.get_value_completions(),
                        toplevel_completions: provider.get_toplevel_completions(),
                    };

                    (diags, all_comps, Some(context))
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
                        Ok(compilation) => (compilation.diagnostics, Vec::new(), None),
                        Err(_) => (Vec::new(), Vec::new(), None),
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
                context_data,
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
