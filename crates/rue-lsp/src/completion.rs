use rue_compiler::Compiler;
use rue_hir::{Builtin, FunctionSymbol, ScopeId, Symbol, SymbolId};
use rue_types::{Type, TypeId};
use std::collections::HashSet;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, Documentation, MarkupContent, MarkupKind, Position,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompletionContext {
    /// Top-level context (export, fn, const, etc.)
    TopLevel,
    /// In a type annotation position (after : or ->)
    TypePosition,
    /// In an expression/value position
    ExpressionValue,
    /// After a dot (module access or field access)
    AfterDot,
    /// Unknown/default context
    Unknown,
}

pub struct CompletionProvider<'a> {
    compiler: &'a Compiler,
    std_scope: ScopeId,
    file_scope: ScopeId,
}

impl<'a> CompletionProvider<'a> {
    pub fn new(compiler: &'a Compiler, std_scope: ScopeId, file_scope: ScopeId) -> Self {
        Self {
            compiler,
            std_scope,
            file_scope,
        }
    }

    pub fn get_all_completions(&self) -> Vec<CompletionItem> {
        let mut completions = Vec::new();

        // Add standard library completions
        completions.extend(self.get_std_lib_completions());

        // Add builtin completions
        completions.extend(self.get_builtin_completions());

        // Add file scope completions
        completions.extend(self.get_file_scope_completions());

        // Sort with ranking: by kind, then alphabetically
        completions.sort_by(|a, b| {
            // Priority order: FUNCTION > CONSTANT > STRUCT/KEYWORD > others
            let priority_a = match a.kind {
                Some(CompletionItemKind::FUNCTION) => 0,
                Some(CompletionItemKind::CONSTANT) => 1,
                Some(CompletionItemKind::STRUCT) | Some(CompletionItemKind::KEYWORD) => 2,
                _ => 3,
            };

            let priority_b = match b.kind {
                Some(CompletionItemKind::FUNCTION) => 0,
                Some(CompletionItemKind::CONSTANT) => 1,
                Some(CompletionItemKind::STRUCT) | Some(CompletionItemKind::KEYWORD) => 2,
                _ => 3,
            };

            // First sort by priority, then alphabetically
            match priority_a.cmp(&priority_b) {
                std::cmp::Ordering::Equal => a.label.cmp(&b.label),
                other => other,
            }
        });

        completions
    }

    /// Get completions based on context at cursor position
    pub fn get_completions_at_position(
        &self,
        position: Position,
        text: &str,
    ) -> Vec<CompletionItem> {
        let context = self.determine_context(position, text);

        match context {
            CompletionContext::TypePosition => self.get_type_completions(),
            CompletionContext::TopLevel => self.get_toplevel_completions(),
            CompletionContext::ExpressionValue => self.get_value_completions(),
            CompletionContext::AfterDot => {
                // Try to provide field completions
                if let Some(_identifier) = self.extract_identifier_before_dot(position, text) {
                    // We need a scope to look up the identifier, but we don't have it here
                    // This is a limitation of the current API - for now, return all completions
                    // The LSP Backend handles field completions with scope information
                    self.get_all_completions()
                } else {
                    self.get_all_completions()
                }
            }
            CompletionContext::Unknown => self.get_all_completions(),
        }
    }

    /// Extract the identifier before the dot (e.g., "variable" from "variable.|")
    fn extract_identifier_before_dot(&self, position: Position, text: &str) -> Option<String> {
        let lines: Vec<&str> = text.lines().collect();
        let line_idx = position.line as usize;
        let char_idx = position.character as usize;

        if line_idx >= lines.len() {
            return None;
        }

        let current_line = lines[line_idx];
        let before_cursor = if char_idx <= current_line.len() {
            &current_line[..char_idx]
        } else {
            current_line
        };

        // Find the dot and extract the identifier before it
        let trimmed = before_cursor.trim_end();
        if !trimmed.ends_with('.') {
            return None;
        }

        // Remove the dot and extract the identifier
        let before_dot = &trimmed[..trimmed.len() - 1];

        // Find the last identifier (word characters including underscore)
        let mut identifier = String::new();
        for ch in before_dot.chars().rev() {
            if ch.is_alphanumeric() || ch == '_' {
                identifier.insert(0, ch);
            } else {
                break;
            }
        }

        if identifier.is_empty() {
            None
        } else {
            Some(identifier)
        }
    }

    /// Get field completions with scope information
    /// This is the main entry point for scope-aware field completions
    pub fn get_field_completions_with_scope(
        &self,
        position: Position,
        text: &str,
        scope_chain: &[ScopeId],
    ) -> Option<Vec<CompletionItem>> {
        // Extract the identifier before the dot
        let identifier = self.extract_identifier_before_dot(position, text)?;

        // Look up the identifier's type in the scope chain
        let type_id = self.lookup_identifier_type(&identifier, scope_chain)?;

        // Get field completions for that type
        let completions = self.get_field_completions_for_type(type_id);

        if completions.is_empty() {
            None
        } else {
            Some(completions)
        }
    }

    /// Look up the type of an identifier in the scope chain
    fn lookup_identifier_type(&self, identifier: &str, scope_chain: &[ScopeId]) -> Option<TypeId> {
        // Search through the scope chain for the identifier
        for &scope_id in scope_chain {
            let scope = self.compiler.scope(scope_id);

            // Try to find the symbol in this scope
            if let Some(symbol_id) = scope.symbol(identifier) {
                let symbol = self.compiler.symbol(symbol_id);

                // Get the type from the symbol
                return match symbol {
                    Symbol::Parameter(param) => Some(param.ty),
                    Symbol::Binding(binding) => Some(binding.value.ty),
                    Symbol::Constant(constant) => Some(constant.value.ty),
                    Symbol::Function(func) => Some(func.ty),
                    _ => None,
                };
            }
        }

        None
    }

    /// Determine the completion context from cursor position
    fn determine_context(&self, position: Position, text: &str) -> CompletionContext {
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

        // Check if we're in a type position (after : or ->)
        // Look for patterns like "name: " or "-> "
        if self.is_in_type_position(before_cursor) {
            return CompletionContext::TypePosition;
        }

        // Check if we're at top level
        let trimmed = before_cursor.trim_start();
        if trimmed.starts_with("export")
            || trimmed.starts_with("fn")
            || trimmed.starts_with("const")
        {
            return CompletionContext::TopLevel;
        }

        // Check if current line starts with top-level keywords (cursor might be before keyword)
        let current_line_trimmed = current_line.trim_start();
        if before_cursor.trim().is_empty()
            && (current_line_trimmed.starts_with("export")
                || current_line_trimmed.starts_with("fn")
                || current_line_trimmed.starts_with("const")
                || current_line_trimmed.starts_with("struct")
                || current_line_trimmed.starts_with("type")
                || current_line_trimmed.starts_with("mod"))
        {
            return CompletionContext::TopLevel;
        }

        // Check if we're inside a function/block by looking at all text before cursor
        let all_text_before_cursor: String = lines[..=line_idx]
            .iter()
            .enumerate()
            .map(|(i, line)| {
                if i == line_idx {
                    &line[..char_idx]
                } else {
                    *line
                }
            })
            .collect::<Vec<_>>()
            .join("\n");

        // Count opening and closing braces to determine if we're inside a block
        let open_braces = all_text_before_cursor.matches('{').count();
        let close_braces = all_text_before_cursor.matches('}').count();

        // If we're inside braces, we're in an expression context
        if open_braces > close_braces {
            return CompletionContext::ExpressionValue;
        }

        // Check if we're likely in an expression by looking for common patterns
        if before_cursor.contains('=') || before_cursor.contains('(') {
            return CompletionContext::ExpressionValue;
        }

        // Default to top level if we're at the file level (not inside any braces)
        if open_braces == close_braces {
            return CompletionContext::TopLevel;
        }

        // Default to unknown
        CompletionContext::Unknown
    }

    /// Check if position is in type annotation context
    fn is_in_type_position(&self, text_before_cursor: &str) -> bool {
        // Check for ": " pattern (variable/parameter type)
        if text_before_cursor.ends_with(": ") {
            return true;
        }

        // Check for "-> " pattern (return type)
        if text_before_cursor.ends_with("-> ") {
            return true;
        }

        // Check for incomplete type annotation (just ":")
        let trimmed = text_before_cursor.trim_end();
        if trimmed.ends_with(':') {
            // Make sure it's not "::" or other operators
            if !trimmed.ends_with("::") {
                return true;
            }
        }

        false
    }

    /// Get completions for top-level declarations
    pub fn get_toplevel_completions(&self) -> Vec<CompletionItem> {
        let items = vec![
            CompletionItem {
                label: "export".to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                detail: Some("export declaration".to_string()),
                ..Default::default()
            },
            CompletionItem {
                label: "inline".to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                detail: Some("inline modifier".to_string()),
                ..Default::default()
            },
            CompletionItem {
                label: "extern".to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                detail: Some("extern modifier".to_string()),
                ..Default::default()
            },
            CompletionItem {
                label: "test".to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                detail: Some("test modifier".to_string()),
                ..Default::default()
            },
            CompletionItem {
                label: "fn".to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                detail: Some("function declaration".to_string()),
                ..Default::default()
            },
            CompletionItem {
                label: "const".to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                detail: Some("constant declaration".to_string()),
                ..Default::default()
            },
            CompletionItem {
                label: "struct".to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                detail: Some("struct declaration".to_string()),
                ..Default::default()
            },
            CompletionItem {
                label: "type".to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                detail: Some("type alias declaration".to_string()),
                ..Default::default()
            },
            CompletionItem {
                label: "mod".to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                detail: Some("module declaration".to_string()),
                ..Default::default()
            },
        ];

        items
    }

    /// Get completions for value/expression context
    pub fn get_value_completions(&self) -> Vec<CompletionItem> {
        let mut items = Vec::new();

        // Add functions and constants (but not types)
        let std_scope = self.compiler.scope(self.std_scope);
        for (name, symbol_id) in std_scope.exported_symbols() {
            let symbol = self.compiler.symbol(symbol_id);
            // Include functions, constants, but skip modules
            match symbol {
                Symbol::Function(_) | Symbol::Constant(_) | Symbol::Builtin(_) => {
                    items.push(self.symbol_to_completion_item(name, symbol_id));
                }
                _ => {}
            }
        }

        // Add builtins
        items.extend(self.get_builtin_completions());

        // Add keywords
        for keyword in [
            "let", "if", "else", "raise", "return", "true", "false", "nil",
        ] {
            items.push(CompletionItem {
                label: keyword.to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                ..Default::default()
            });
        }

        // Sort with intelligent ranking
        self.sort_completions(&mut items);
        items
    }

    /// Sort completions with intelligent ranking
    fn sort_completions(&self, items: &mut [CompletionItem]) {
        items.sort_by(|a, b| {
            // Priority order: FUNCTION > CONSTANT > STRUCT/KEYWORD > others
            let priority_a = match a.kind {
                Some(CompletionItemKind::FUNCTION) => 0,
                Some(CompletionItemKind::CONSTANT) => 1,
                Some(CompletionItemKind::STRUCT) | Some(CompletionItemKind::KEYWORD) => 2,
                _ => 3,
            };

            let priority_b = match b.kind {
                Some(CompletionItemKind::FUNCTION) => 0,
                Some(CompletionItemKind::CONSTANT) => 1,
                Some(CompletionItemKind::STRUCT) | Some(CompletionItemKind::KEYWORD) => 2,
                _ => 3,
            };

            // First sort by priority, then alphabetically
            match priority_a.cmp(&priority_b) {
                std::cmp::Ordering::Equal => a.label.cmp(&b.label),
                other => other,
            }
        });
    }

    pub fn get_type_completions(&self) -> Vec<CompletionItem> {
        let mut items = Vec::new();

        // Add built-in types
        let builtins = self.compiler.builtins();

        // Manually add builtin types since scope might not iterate all
        for (name, type_id) in [
            ("Atom", builtins.types.atom),
            ("Bytes", builtins.types.bytes),
            ("Bytes32", builtins.types.bytes32),
            ("String", builtins.types.string),
            ("PublicKey", builtins.types.public_key),
            ("Signature", builtins.types.signature),
            ("Int", builtins.types.int),
            ("Bool", builtins.types.bool),
            ("Any", builtins.types.permissive_any),
        ] {
            items.push(self.type_to_completion_item(name, type_id));
        }

        // Add std lib types
        let std_scope = self.compiler.scope(self.std_scope);
        for (name, type_id) in std_scope.exported_types() {
            items.push(self.type_to_completion_item(name, type_id));
        }

        // Sort with intelligent ranking
        self.sort_completions(&mut items);
        items
    }

    fn get_std_lib_completions(&self) -> Vec<CompletionItem> {
        let mut items = Vec::new();
        let std_scope = self.compiler.scope(self.std_scope);

        // Get exported symbols
        for (name, symbol_id) in std_scope.exported_symbols() {
            items.push(self.symbol_to_completion_item(name, symbol_id));
        }

        // Get exported types
        for (name, type_id) in std_scope.exported_types() {
            items.push(self.type_to_completion_item(name, type_id));
        }

        items
    }

    fn get_builtin_completions(&self) -> Vec<CompletionItem> {
        let mut items = Vec::new();

        // Add builtin functions
        items.push(CompletionItem {
            label: "sha256".to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("fn(Bytes) -> Bytes32".to_string()),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: "Computes the SHA-256 hash of the input.".to_string(),
            })),
            ..Default::default()
        });

        items.push(CompletionItem {
            label: "sha256_inline".to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("fn(Bytes) -> Bytes32".to_string()),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: "Computes the SHA-256 hash inline (optimized).".to_string(),
            })),
            ..Default::default()
        });

        items.push(CompletionItem {
            label: "keccak256".to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("fn(Bytes) -> Bytes32".to_string()),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: "Computes the Keccak-256 hash of the input.".to_string(),
            })),
            ..Default::default()
        });

        items.push(CompletionItem {
            label: "keccak256_inline".to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("fn(Bytes) -> Bytes32".to_string()),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: "Computes the Keccak-256 hash inline (optimized).".to_string(),
            })),
            ..Default::default()
        });

        items.push(CompletionItem {
            label: "concat".to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("builtin function".to_string()),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: "Concatenates values.".to_string(),
            })),
            ..Default::default()
        });

        items.push(CompletionItem {
            label: "coinid".to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("builtin function".to_string()),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: "Calculates the coin ID.".to_string(),
            })),
            ..Default::default()
        });

        items.push(CompletionItem {
            label: "substr".to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("builtin function".to_string()),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: "Extracts a substring from a byte string.".to_string(),
            })),
            ..Default::default()
        });

        items.push(CompletionItem {
            label: "sum".to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("builtin function".to_string()),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: "Adds two numbers.".to_string(),
            })),
            ..Default::default()
        });

        items.push(CompletionItem {
            label: "difference".to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("builtin function".to_string()),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: "Subtracts two numbers.".to_string(),
            })),
            ..Default::default()
        });

        items.push(CompletionItem {
            label: "product".to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("builtin function".to_string()),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: "Multiplies two numbers.".to_string(),
            })),
            ..Default::default()
        });

        items.push(CompletionItem {
            label: "divmod".to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("builtin function".to_string()),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: "Divides two numbers and returns quotient and remainder.".to_string(),
            })),
            ..Default::default()
        });

        items
    }

    fn get_file_scope_completions(&self) -> Vec<CompletionItem> {
        let mut items = Vec::new();
        let file_scope = self.compiler.scope(self.file_scope);

        // Get symbols from file scope
        for (name, symbol_id) in file_scope.exported_symbols() {
            items.push(self.symbol_to_completion_item(name, symbol_id));
        }

        // Get types from file scope
        for (name, type_id) in file_scope.exported_types() {
            items.push(self.type_to_completion_item(name, type_id));
        }

        items
    }

    fn symbol_to_completion_item(&self, name: &str, symbol_id: SymbolId) -> CompletionItem {
        let symbol = self.compiler.symbol(symbol_id);

        let kind = match symbol {
            Symbol::Function(_) => CompletionItemKind::FUNCTION,
            Symbol::Constant(_) => CompletionItemKind::CONSTANT,
            Symbol::Builtin(_) => CompletionItemKind::FUNCTION,
            Symbol::Module(_) => CompletionItemKind::MODULE,
            Symbol::Parameter(_) => CompletionItemKind::VARIABLE,
            Symbol::Binding(_) => CompletionItemKind::VARIABLE,
            Symbol::Unresolved => CompletionItemKind::TEXT,
        };

        let detail = self.get_symbol_detail(symbol_id, symbol);
        let documentation = self.get_symbol_documentation(name, symbol);

        CompletionItem {
            label: name.to_string(),
            kind: Some(kind),
            detail,
            documentation,
            ..Default::default()
        }
    }

    fn type_to_completion_item(&self, name: &str, _type_id: TypeId) -> CompletionItem {
        CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::STRUCT),
            detail: Some(format!("type {}", name)),
            ..Default::default()
        }
    }

    fn get_symbol_detail(&self, _symbol_id: SymbolId, symbol: &Symbol) -> Option<String> {
        match symbol {
            Symbol::Function(func) => Some(self.format_function_signature(func)),
            Symbol::Constant(_constant) => Some("constant".to_string()),
            Symbol::Builtin(builtin) => Some(format!("builtin: {}", self.format_builtin(builtin))),
            Symbol::Parameter(_param) => Some("parameter".to_string()),
            Symbol::Binding(_binding) => Some("binding".to_string()),
            _ => None,
        }
    }

    fn format_function_signature(&self, func: &FunctionSymbol) -> String {
        let param_count = func.parameters.len();
        format!("fn({} params)", param_count)
    }

    fn format_builtin(&self, builtin: &Builtin) -> String {
        match builtin {
            Builtin::Sha256 { inline: false } => "sha256",
            Builtin::Sha256 { inline: true } => "sha256_inline",
            Builtin::Keccak256 { inline: false } => "keccak256",
            Builtin::Keccak256 { inline: true } => "keccak256_inline",
            Builtin::Concat => "concat",
            Builtin::CoinId => "coinid",
            Builtin::Substr => "substr",
            Builtin::Sum => "sum",
            Builtin::Difference => "difference",
            Builtin::Product => "product",
            Builtin::Divmod => "divmod",
            Builtin::Modpow => "modpow",
            Builtin::Any => "any",
            Builtin::All => "all",
            Builtin::PubkeyForExp => "pubkey_for_exp",
            Builtin::G1Sum => "g1_sum",
            Builtin::G1Difference => "g1_difference",
            Builtin::G2Sum => "g2_sum",
            Builtin::G2Difference => "g2_difference",
            Builtin::G1Map => "g1_map",
            Builtin::G2Map => "g2_map",
            Builtin::BlsPairingIdentity => "bls_pairing_identity",
            Builtin::BlsVerify => "bls_verify",
            Builtin::Secp256K1Verify => "secp256k1_verify",
            Builtin::Secp256R1Verify => "secp256r1_verify",
        }
        .to_string()
    }

    fn get_symbol_documentation(&self, name: &str, _symbol: &Symbol) -> Option<Documentation> {
        let description = match name {
            "tree_hash" => {
                "Computes the tree hash of a value (recursively handles atoms and pairs)."
            }
            "tree_hash_atom" => "Computes the tree hash of an atom using SHA-256(1 || value).",
            "tree_hash_pair" => {
                "Computes the tree hash of a pair using SHA-256(2 || first || rest)."
            }
            "curry_tree_hash" => "Computes the tree hash of a curried module with parameters.",
            "merge_list" => "Merges two lists by appending the second to the first.",
            "deep_equal" => "Performs deep equality comparison between two values.",
            "CREATE_COIN" => "Condition opcode for creating a new coin (51).",
            "REMARK" => "Condition opcode for adding a remark (1).",
            "RESERVE_FEE" => "Condition opcode for reserving fees (52).",
            "AGG_SIG_PARENT" => "Condition opcode for signature aggregation with parent (43).",
            "AGG_SIG_PUZZLE" => "Condition opcode for signature aggregation with puzzle (44).",
            "AGG_SIG_AMOUNT" => "Condition opcode for signature aggregation with amount (45).",
            "AGG_SIG_ME" => "Condition opcode for signature aggregation with current coin (50).",
            "ASSERT_MY_COIN_ID" => "Condition opcode to assert the current coin ID (70).",
            "ASSERT_MY_PUZZLE_HASH" => "Condition opcode to assert the current puzzle hash (72).",
            "ASSERT_MY_AMOUNT" => "Condition opcode to assert the current coin amount (73).",
            "ASSERT_HEIGHT_RELATIVE" => "Condition opcode to assert relative block height (82).",
            "ASSERT_HEIGHT_ABSOLUTE" => "Condition opcode to assert absolute block height (83).",
            "Condition" => "Union type of all condition types.",
            "CreateCoin" => "Struct for CREATE_COIN condition.",
            "ReserveFee" => "Struct for RESERVE_FEE condition.",
            "Remark" => "Struct for REMARK condition.",
            _ => return None,
        };

        Some(Documentation::MarkupContent(MarkupContent {
            kind: MarkupKind::Markdown,
            value: description.to_string(),
        }))
    }

    /// Get completions from a chain of scopes (innermost to outermost)
    /// Handles shadowing: inner scopes hide outer scopes
    pub fn get_scope_completions(&self, scope_chain: &[ScopeId]) -> Vec<CompletionItem> {
        let mut items = Vec::new();
        let mut seen_symbols = HashSet::new();
        let mut seen_types = HashSet::new();

        // Walk scopes from innermost to outermost
        for &scope_id in scope_chain {
            let scope = self.compiler.scope(scope_id);

            // Add all symbols from this scope (including non-exported)
            for (name, symbol_id) in scope.symbols() {
                // Skip if already seen (shadowed by inner scope)
                if seen_symbols.contains(name) {
                    continue;
                }
                seen_symbols.insert(name.to_string());

                items.push(self.symbol_to_completion_item(name, symbol_id));
            }

            // Add all types from this scope (including non-exported)
            for (name, type_id) in scope.types() {
                // Skip if already seen (shadowed by inner scope)
                if seen_types.contains(name) {
                    continue;
                }
                seen_types.insert(name.to_string());

                items.push(self.type_to_completion_item(name, type_id));
            }
        }

        // Also add stdlib and builtins
        items.extend(self.get_std_lib_completions());
        items.extend(self.get_builtin_completions());

        // Add keywords
        for keyword in [
            "let", "if", "else", "raise", "return", "true", "false", "nil",
        ] {
            items.push(CompletionItem {
                label: keyword.to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                ..Default::default()
            });
        }

        self.sort_completions(&mut items);
        items
    }

    /// Get field completions for a given type
    pub fn get_field_completions_for_type(&self, type_id: TypeId) -> Vec<CompletionItem> {
        // Note: We work with the type directly without substitution
        // This means we might not fully resolve all type aliases in some edge cases,
        // but it allows us to work with an immutable compiler reference
        self.get_field_completions_for_type_impl(type_id)
    }

    fn get_field_completions_for_type_impl(&self, type_id: TypeId) -> Vec<CompletionItem> {
        let ty = self.compiler.ty(type_id);

        match ty {
            Type::Struct(struct_ty) => {
                // Return completions for each field in the struct
                let mut items = Vec::new();
                for field_name in struct_ty.fields.iter() {
                    items.push(CompletionItem {
                        label: field_name.clone(),
                        kind: Some(CompletionItemKind::FIELD),
                        detail: Some("field".to_string()),
                        ..Default::default()
                    });
                }
                items
            }
            Type::Pair(_) => {
                // Pairs support .first and .rest
                vec![
                    CompletionItem {
                        label: "first".to_string(),
                        kind: Some(CompletionItemKind::FIELD),
                        detail: Some("first element of pair".to_string()),
                        documentation: Some(Documentation::MarkupContent(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: "Access the first element of the pair.".to_string(),
                        })),
                        ..Default::default()
                    },
                    CompletionItem {
                        label: "rest".to_string(),
                        kind: Some(CompletionItemKind::FIELD),
                        detail: Some("rest element of pair".to_string()),
                        documentation: Some(Documentation::MarkupContent(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: "Access the rest element of the pair.".to_string(),
                        })),
                        ..Default::default()
                    },
                ]
            }
            Type::Atom(_) => {
                // Atoms support .length
                vec![CompletionItem {
                    label: "length".to_string(),
                    kind: Some(CompletionItemKind::FIELD),
                    detail: Some("length of atom".to_string()),
                    documentation: Some(Documentation::MarkupContent(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: "Get the length of the atom in bytes.".to_string(),
                    })),
                    ..Default::default()
                }]
            }
            Type::Union(union_ty) => {
                // For unions, return fields common to all variants
                let mut common_fields: Option<HashSet<String>> = None;

                for &variant_ty in &union_ty.types {
                    let variant_fields = self.get_field_completions_for_type_impl(variant_ty);
                    let variant_field_names: HashSet<String> =
                        variant_fields.into_iter().map(|item| item.label).collect();

                    match &mut common_fields {
                        None => common_fields = Some(variant_field_names),
                        Some(fields) => {
                            fields.retain(|f| variant_field_names.contains(f));
                        }
                    }
                }

                // Convert common field names back to completion items
                common_fields
                    .unwrap_or_default()
                    .into_iter()
                    .map(|name| CompletionItem {
                        label: name,
                        kind: Some(CompletionItemKind::FIELD),
                        detail: Some("field".to_string()),
                        ..Default::default()
                    })
                    .collect()
            }
            Type::Alias(alias) => {
                // Recurse into the aliased type
                self.get_field_completions_for_type_impl(alias.inner)
            }
            Type::Ref(inner) => {
                // Recurse into the referenced type
                self.get_field_completions_for_type_impl(*inner)
            }
            _ => Vec::new(),
        }
    }
}
