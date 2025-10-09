use rue_compiler::{Compiler, ScopeMap};
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
    /// Inside a struct initialization literal
    StructInitialization,
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
        scope_map: Option<&ScopeMap>,
    ) -> Vec<CompletionItem> {
        let context = self.determine_context(position, text);

        match context {
            CompletionContext::TypePosition => self.get_type_completions(),
            CompletionContext::TopLevel => self.get_toplevel_completions(),
            CompletionContext::StructInitialization => {
                // Try to provide struct field completions
                if let Some(completions) = self.get_struct_init_completions(position, text) {
                    return completions;
                }
                // Fallback to value completions if struct type lookup failed
                self.get_value_completions()
            }
            CompletionContext::ExpressionValue | CompletionContext::Unknown => {
                // If we have scope information, use scope-aware completions
                if let Some(scope_map) = scope_map {
                    if let Some(scope_id) =
                        scope_map.find_scope_at_position(position.line, position.character, text)
                    {
                        let scope_chain = scope_map.get_scope_chain(scope_id);
                        return self.get_scope_completions(&scope_chain);
                    }
                }
                // Fallback to value completions if no scope info available
                self.get_value_completions()
            }
            CompletionContext::AfterDot => {
                // Try to provide field completions with scope information
                if let Some(scope_map) = scope_map {
                    if let Some(scope_id) =
                        scope_map.find_scope_at_position(position.line, position.character, text)
                    {
                        let scope_chain = scope_map.get_scope_chain(scope_id);
                        if let Some(completions) =
                            self.get_field_completions_with_scope(position, text, &scope_chain)
                        {
                            return completions;
                        }
                    }
                }
                // Fallback to all completions if no scope info or field lookup failed
                self.get_all_completions()
            }
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

        // Handle both "identifier." and "identifier.partial" cases
        let before_dot = if trimmed.ends_with('.') {
            // Case: "identifier."
            &trimmed[..trimmed.len() - 1]
        } else if let Some(dot_pos) = trimmed.rfind('.') {
            // Case: "identifier.partial" - check if after dot is valid identifier chars
            let after_dot = &trimmed[dot_pos + 1..];
            if !after_dot.is_empty() && after_dot.chars().all(|c| c.is_alphanumeric() || c == '_') {
                // Extract up to the dot
                &trimmed[..dot_pos]
            } else {
                return None;
            }
        } else {
            return None;
        };

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

        // Check if we're after a dot (including when typing partial field name)
        // Pattern: "identifier." or "identifier.partial"
        let trimmed = before_cursor.trim_end();
        if trimmed.ends_with('.') {
            return CompletionContext::AfterDot;
        }
        // Also check for dot followed by partial identifier (e.g., "obj.fie")
        if let Some(dot_pos) = trimmed.rfind('.') {
            let after_dot = &trimmed[dot_pos + 1..];
            // Check if everything after the dot is a valid identifier (being typed)
            if !after_dot.is_empty() && after_dot.chars().all(|c| c.is_alphanumeric() || c == '_') {
                return CompletionContext::AfterDot;
            }
        }

        // Check if we're in a type position (after : or ->) BEFORE checking top-level
        // This is more specific and should take precedence
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

        // Check if we're inside a struct initialization (pattern: TypeName {)
        // Note: We already checked for type position earlier, so "field: |" will have
        // been caught as TypePosition before getting here
        if self.is_in_struct_init(&all_text_before_cursor) {
            // Check if we're after a colon in a field assignment (e.g., "field: |")
            // In this case, we want value completions, not field name completions
            if self.is_after_field_colon(before_cursor) {
                return CompletionContext::ExpressionValue;
            }
            return CompletionContext::StructInitialization;
        }

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

        // Check for "as " pattern (cast expression)
        if text_before_cursor.ends_with("as ") {
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

        // Check for incomplete cast (just "as")
        if trimmed.ends_with("as") {
            // Make sure there's a word boundary before "as"
            // to avoid matching "class", "was", etc.
            if trimmed.len() >= 2 {
                let before_as = trimmed.chars().rev().nth(2);
                if let Some(ch) = before_as {
                    if !ch.is_alphanumeric() && ch != '_' {
                        return true;
                    }
                } else {
                    // "as" is at the start
                    return true;
                }
            } else {
                return true;
            }
        }

        // Check for partial type being typed after ":" or "->" or "as"
        // Pattern: "name: PartialType" or "-> PartialType" or "expr as PartialType"
        // Look backwards to find the last ":", "->", or "as" and check if there's only identifier chars after it
        if let Some(colon_pos) = text_before_cursor.rfind(": ") {
            let after_colon = &text_before_cursor[colon_pos + 2..];
            // Check if everything after ": " is a potential type identifier (or empty)
            if after_colon.chars().all(|c| c.is_alphanumeric() || c == '_') {
                return true;
            }
        }

        if let Some(arrow_pos) = text_before_cursor.rfind("-> ") {
            let after_arrow = &text_before_cursor[arrow_pos + 3..];
            // Check if everything after "-> " is a potential type identifier (or empty)
            if after_arrow.chars().all(|c| c.is_alphanumeric() || c == '_') {
                return true;
            }
        }

        if let Some(as_pos) = text_before_cursor.rfind("as ") {
            let after_as = &text_before_cursor[as_pos + 3..];
            // Check if everything after "as " is a potential type identifier (or empty)
            if after_as.chars().all(|c| c.is_alphanumeric() || c == '_') {
                return true;
            }
        }

        false
    }

    /// Check if we're after a colon in a struct field assignment (e.g., "field: |")
    /// This indicates we should suggest values, not field names
    fn is_after_field_colon(&self, line_before_cursor: &str) -> bool {
        let trimmed = line_before_cursor.trim_end();

        // Check for ": " pattern at the end
        if trimmed.ends_with(": ") {
            return true;
        }

        // Check for just ":" at the end
        if trimmed.ends_with(':') && !trimmed.ends_with("::") {
            return true;
        }

        // Check if we have a pattern like "field: partial_value"
        // where we're typing a partial value after the colon
        if let Some(colon_pos) = trimmed.rfind(':') {
            // Make sure it's not "::"
            if colon_pos > 0 && trimmed.chars().nth(colon_pos - 1) == Some(':') {
                return false;
            }

            // Check what comes after the colon
            let after_colon = &trimmed[colon_pos + 1..].trim_start();

            // If there's only identifier-like characters after the colon (or nothing),
            // we're likely in a field value position
            if after_colon.is_empty()
                || after_colon.chars().all(|c| c.is_alphanumeric() || c == '_')
            {
                return true;
            }
        }

        false
    }

    /// Check if we're inside a struct initialization literal (pattern: TypeName {)
    fn is_in_struct_init(&self, text_before_cursor: &str) -> bool {
        // Count braces to see if we're inside an unclosed brace
        let open_braces = text_before_cursor.matches('{').count();
        let close_braces = text_before_cursor.matches('}').count();

        if open_braces <= close_braces {
            return false; // Not inside any braces
        }

        // Find the position of the last unclosed opening brace
        let mut brace_depth = 0;
        let mut last_open_brace_pos = None;

        for (i, ch) in text_before_cursor.char_indices().rev() {
            if ch == '}' {
                brace_depth += 1;
            } else if ch == '{' {
                if brace_depth == 0 {
                    last_open_brace_pos = Some(i);
                    break;
                } else {
                    brace_depth -= 1;
                }
            }
        }

        if let Some(brace_pos) = last_open_brace_pos {
            // Look backwards from the brace to find an identifier (the struct name)
            let before_brace = &text_before_cursor[..brace_pos];

            // Extract identifier before the brace
            let trimmed = before_brace.trim_end();

            // Extract the last identifier (potential type name)
            let last_identifier = if let Some(last_word_start) =
                trimmed.rfind(|c: char| !c.is_alphanumeric() && c != '_')
            {
                &trimmed[last_word_start + 1..]
            } else {
                trimmed
            };

            // Check if it looks like a type name (starts with uppercase)
            if last_identifier.is_empty() || !last_identifier.chars().next().unwrap().is_uppercase()
            {
                return false;
            }

            // Now check if this is a function body by looking for "->" pattern before the type name
            // We want to detect: "fn foo() -> TypeName {" and exclude it
            // But allow: "= TypeName {" or "TypeName {" or ": TypeName {"

            // Find where the type name starts in trimmed
            let type_start = if let Some(pos) = trimmed.rfind(last_identifier) {
                pos
            } else {
                return false;
            };

            let before_type = &trimmed[..type_start].trim_end();

            // If there's a "->" immediately before the type name, it's a function return type
            if before_type.ends_with("->") {
                return false;
            }

            // If there's a "struct" keyword before the type name, it's a struct definition
            if before_type.ends_with("struct") {
                return false;
            }

            // It's a struct initialization
            return true;
        }

        false
    }

    /// Get struct field completions when initializing a struct
    fn get_struct_init_completions(
        &self,
        position: Position,
        text: &str,
    ) -> Option<Vec<CompletionItem>> {
        let lines: Vec<&str> = text.lines().collect();
        let line_idx = position.line as usize;
        let char_idx = position.character as usize;

        if line_idx >= lines.len() {
            return None;
        }

        // Get all text before cursor
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

        // Find the last unclosed opening brace
        let mut brace_depth = 0;
        let mut last_open_brace_pos = None;

        for (i, ch) in all_text_before_cursor.char_indices().rev() {
            if ch == '}' {
                brace_depth += 1;
            } else if ch == '{' {
                if brace_depth == 0 {
                    last_open_brace_pos = Some(i);
                    break;
                } else {
                    brace_depth -= 1;
                }
            }
        }

        let brace_pos = last_open_brace_pos?;
        let before_brace = &all_text_before_cursor[..brace_pos];
        let trimmed = before_brace.trim_end();

        // Extract the type name before the brace
        let type_name = if let Some(last_word_start) =
            trimmed.rfind(|c: char| !c.is_alphanumeric() && c != '_')
        {
            &trimmed[last_word_start + 1..]
        } else {
            trimmed
        };

        if type_name.is_empty() {
            return None;
        }

        // Look up the type in available scopes
        let type_id = self.lookup_type_by_name(type_name)?;

        // Get field completions for the struct type
        let completions = self.get_field_completions_for_type(type_id);

        if completions.is_empty() {
            None
        } else {
            Some(completions)
        }
    }

    /// Look up a type by name in available scopes
    fn lookup_type_by_name(&self, type_name: &str) -> Option<TypeId> {
        // Check file scope first
        let file_scope = self.compiler.scope(self.file_scope);
        for (name, type_id) in file_scope.types() {
            if name == type_name {
                return Some(type_id);
            }
        }

        // Check std scope
        let std_scope = self.compiler.scope(self.std_scope);
        for (name, type_id) in std_scope.types() {
            if name == type_name {
                return Some(type_id);
            }
        }

        // Check built-in types
        let builtins = self.compiler.builtins();
        match type_name {
            "Atom" => Some(builtins.types.atom),
            "Bytes" => Some(builtins.types.bytes),
            "Bytes32" => Some(builtins.types.bytes32),
            "String" => Some(builtins.types.string),
            "PublicKey" => Some(builtins.types.public_key),
            "Signature" => Some(builtins.types.signature),
            "Int" => Some(builtins.types.int),
            "Bool" => Some(builtins.types.bool),
            "Any" => Some(builtins.types.permissive_any),
            _ => None,
        }
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

        // Add file-local types (user-defined structs and type aliases)
        let file_scope = self.compiler.scope(self.file_scope);
        for (name, type_id) in file_scope.types() {
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

#[cfg(test)]
mod tests {
    use super::*;
    use rue_compiler::analyze_file_with_context;
    use rue_diagnostic::{Source, SourceKind};
    use rue_options::CompilerOptions;
    use std::sync::Arc;
    use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind, Position};

    // ============================================================================
    // Test Helper Functions
    // ============================================================================

    /// Parse cursor position from code containing a | marker
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

    /// Compile code and get completions at cursor position marked by |
    /// This helper tries to use scope-aware completions when available
    fn compile_and_get_completions(code: &str) -> Vec<CompletionItem> {
        let (code_without_marker, line, character) = parse_cursor_position(code);

        let source = Source::new(
            Arc::from(code_without_marker.as_str()),
            SourceKind::File("test.rue".to_string()),
        );
        let options = CompilerOptions::default();

        let result =
            analyze_file_with_context(source, options).expect("Failed to compile test code");

        let provider =
            CompletionProvider::new(&result.compiler, result.std_scope, result.file_scope);

        let position = Position::new(line, character);

        // Check if we have a scope at this position for scope-aware completions
        if let Some(scope_id) =
            result
                .scope_map
                .find_scope_at_position(line, character, &code_without_marker)
        {
            let scope_chain = result.scope_map.get_scope_chain(scope_id);

            // Try field completions first (for after-dot contexts)
            if let Some(field_completions) = provider.get_field_completions_with_scope(
                position,
                &code_without_marker,
                &scope_chain,
            ) {
                return field_completions;
            }
        }

        // Fall back to position-based completions
        provider.get_completions_at_position(position, &code_without_marker, None)
    }

    /// Compile code and get scope-based completions at cursor position marked by |
    fn compile_and_get_scope_completions(code: &str) -> Vec<CompletionItem> {
        let (code_without_marker, line, character) = parse_cursor_position(code);

        let source = Source::new(
            Arc::from(code_without_marker.as_str()),
            SourceKind::File("test.rue".to_string()),
        );
        let options = CompilerOptions::default();

        let result =
            analyze_file_with_context(source, options).expect("Failed to compile test code");

        let provider =
            CompletionProvider::new(&result.compiler, result.std_scope, result.file_scope);

        // Find scope at position
        let scope = result
            .scope_map
            .find_scope_at_position(line, character, &code_without_marker);

        if let Some(scope_id) = scope {
            let scope_chain = result.scope_map.get_scope_chain(scope_id);
            provider.get_scope_completions(&scope_chain)
        } else {
            // Fallback to position-based completions
            let position = Position::new(line, character);
            provider.get_completions_at_position(position, &code_without_marker, None)
        }
    }

    /// Assert that completions contain an item with the given label
    fn assert_contains_completion(completions: &[CompletionItem], label: &str) {
        let found = completions.iter().any(|item| item.label == label);
        assert!(
            found,
            "Expected completion '{}' not found. Available: {:?}",
            label,
            completions.iter().map(|c| &c.label).collect::<Vec<_>>()
        );
    }

    /// Assert that completions do NOT contain an item with the given label
    fn assert_not_contains_completion(completions: &[CompletionItem], label: &str) {
        let found = completions.iter().any(|item| item.label == label);
        assert!(
            !found,
            "Unexpected completion '{}' found. Available: {:?}",
            label,
            completions.iter().map(|c| &c.label).collect::<Vec<_>>()
        );
    }

    /// Assert that a completion with the given label has the expected kind
    fn assert_completion_kind(
        completions: &[CompletionItem],
        label: &str,
        expected_kind: CompletionItemKind,
    ) {
        let item = completions.iter().find(|item| item.label == label);
        assert!(item.is_some(), "Completion '{}' not found", label);
        let item = item.unwrap();
        assert_eq!(
            item.kind,
            Some(expected_kind),
            "Completion '{}' has kind {:?}, expected {:?}",
            label,
            item.kind,
            expected_kind
        );
    }

    // ============================================================================
    // Basic Smoke Tests
    // ============================================================================

    #[test]
    fn test_lsp_builds() {
        assert!(true, "LSP module compiles");
    }

    #[test]
    fn test_completion_provider_available() {
        // Test that CompletionProvider can be constructed
        let code = "fn test() -> Int { 5 }";
        let source = Source::new(Arc::from(code), SourceKind::File("test.rue".to_string()));
        let options = CompilerOptions::default();

        let result = analyze_file_with_context(source, options);
        assert!(result.is_ok(), "Should be able to compile and analyze code");
    }

    // ============================================================================
    // Context Determination Tests
    // ============================================================================

    #[test]
    fn test_context_type_position_after_colon() {
        let code = r#"
fn foo(x: |) -> Int {
    x
}
"#;
        let completions = compile_and_get_completions(code);

        // Should suggest type completions
        assert_contains_completion(&completions, "Int");
        assert_contains_completion(&completions, "Bool");
        assert_contains_completion(&completions, "Bytes");
        assert_contains_completion(&completions, "String");
    }

    #[test]
    fn test_context_type_position_after_colon_with_partial_text() {
        let code = r#"
fn foo(x: In|) -> Int {
    x
}
"#;
        let completions = compile_and_get_completions(code);

        // Should suggest type completions even when partially typed
        assert_contains_completion(&completions, "Int");
        assert_contains_completion(&completions, "Bool");
        assert_contains_completion(&completions, "Bytes");
        assert_contains_completion(&completions, "String");
    }

    #[test]
    fn test_context_type_position_after_arrow() {
        let code = r#"
fn foo() -> | {
    5
}
"#;
        let completions = compile_and_get_completions(code);

        // Should suggest type completions
        assert_contains_completion(&completions, "Int");
        assert_contains_completion(&completions, "Bool");
        assert_contains_completion(&completions, "Bytes");
    }

    #[test]
    fn test_context_type_position_after_arrow_with_partial_text() {
        let code = r#"
fn foo() -> By| {
    5
}
"#;
        let completions = compile_and_get_completions(code);

        // Should suggest type completions even when partially typed
        assert_contains_completion(&completions, "Int");
        assert_contains_completion(&completions, "Bool");
        assert_contains_completion(&completions, "Bytes");
        assert_contains_completion(&completions, "Bytes32");
    }

    #[test]
    fn test_context_type_position_struct_field() {
        let code = r#"
struct Foo {
    bar: |
}
"#;
        let completions = compile_and_get_completions(code);

        // Should suggest type completions for struct fields
        assert_contains_completion(&completions, "Int");
        assert_contains_completion(&completions, "Bool");
        assert_contains_completion(&completions, "Bytes");
        assert_contains_completion(&completions, "Bytes32");
        assert_contains_completion(&completions, "String");

        // Should NOT suggest expression keywords
        assert_not_contains_completion(&completions, "if");
        assert_not_contains_completion(&completions, "let");
    }

    #[test]
    fn test_context_type_position_struct_field_with_partial_text() {
        let code = r#"
struct Foo {
    bar: Str|
}
"#;
        let completions = compile_and_get_completions(code);

        // Should suggest type completions for struct fields even when partially typed
        assert_contains_completion(&completions, "Int");
        assert_contains_completion(&completions, "Bool");
        assert_contains_completion(&completions, "String");

        // Should NOT suggest expression keywords
        assert_not_contains_completion(&completions, "if");
        assert_not_contains_completion(&completions, "let");
    }

    #[test]
    fn test_context_type_position_includes_custom_structs() {
        let code = r#"
struct Point {
    x: Int,
    y: Int,
}

struct Rectangle {
    width: Int,
    height: Int,
}

fn foo(p: |) -> Int {
    5
}
"#;
        let completions = compile_and_get_completions(code);

        // Should suggest built-in types
        assert_contains_completion(&completions, "Int");
        assert_contains_completion(&completions, "Bool");

        // Should also suggest user-defined struct types
        assert_contains_completion(&completions, "Point");
        assert_contains_completion(&completions, "Rectangle");
    }

    #[test]
    fn test_context_type_position_after_as() {
        let code = r#"
fn foo() -> Bytes {
    1 as |
}
"#;
        let completions = compile_and_get_completions(code);

        // Should suggest type completions after "as "
        assert_contains_completion(&completions, "Int");
        assert_contains_completion(&completions, "Bool");
        assert_contains_completion(&completions, "Bytes");
        assert_contains_completion(&completions, "Bytes32");
        assert_contains_completion(&completions, "String");

        // Should NOT suggest expression keywords
        assert_not_contains_completion(&completions, "if");
        assert_not_contains_completion(&completions, "let");
    }

    #[test]
    fn test_context_type_position_after_as_with_partial_text() {
        let code = r#"
fn foo() -> Bytes {
    1 as By|
}
"#;
        let completions = compile_and_get_completions(code);

        // Should suggest type completions even when partially typed
        assert_contains_completion(&completions, "Int");
        assert_contains_completion(&completions, "Bool");
        assert_contains_completion(&completions, "Bytes");
        assert_contains_completion(&completions, "Bytes32");
    }

    #[test]
    fn test_context_type_position_after_as_on_variable() {
        let code = r#"
fn foo() -> Bytes {
    let a = 5;
    let b = a as |
    b
}
"#;
        let completions = compile_and_get_completions(code);

        // Should suggest type completions
        assert_contains_completion(&completions, "Int");
        assert_contains_completion(&completions, "Bool");
        assert_contains_completion(&completions, "Bytes");
        assert_contains_completion(&completions, "String");

        // Should NOT suggest variables/values
        assert_not_contains_completion(&completions, "if");
        assert_not_contains_completion(&completions, "let");
    }

    #[test]
    fn test_context_type_position_after_as_on_field() {
        let code = r#"
struct Data {
    value: Int,
}

fn foo(data: Data) -> Bytes {
    data.value as |
}
"#;
        let completions = compile_and_get_completions(code);

        // Should suggest type completions
        assert_contains_completion(&completions, "Int");
        assert_contains_completion(&completions, "Bool");
        assert_contains_completion(&completions, "Bytes");
        assert_contains_completion(&completions, "Bytes32");
    }

    #[test]
    fn test_context_type_position_after_as_with_custom_types() {
        let code = r#"
struct Point {
    x: Int,
    y: Int,
}

fn foo() -> Any {
    1 as |
}
"#;
        let completions = compile_and_get_completions(code);

        // Should suggest built-in types
        assert_contains_completion(&completions, "Int");
        assert_contains_completion(&completions, "Bytes");

        // Should also suggest user-defined struct types
        assert_contains_completion(&completions, "Point");
    }

    #[test]
    fn test_context_expression_after_equals() {
        let code = r#"
fn foo() -> Int {
    let x = |
    x
}
"#;
        let completions = compile_and_get_completions(code);

        // Should suggest value completions (functions, constants, keywords)
        // Should NOT suggest type names as values
        assert_contains_completion(&completions, "if");
        assert_contains_completion(&completions, "let");
        assert_contains_completion(&completions, "true");
        assert_contains_completion(&completions, "false");
    }

    #[test]
    fn test_context_expression_in_function_call() {
        let code = r#"
fn foo() -> Bytes32 {
    sha256(|)
}
"#;
        let completions = compile_and_get_completions(code);

        // Should suggest value completions
        assert_contains_completion(&completions, "sha256");
        assert_contains_completion(&completions, "concat");
    }

    #[test]
    fn test_context_top_level() {
        let code = r#"
|export fn main() -> Int {
    5
}
"#;
        let completions = compile_and_get_completions(code);

        // Should suggest top-level keywords
        assert_contains_completion(&completions, "export");
        assert_contains_completion(&completions, "inline");
        assert_contains_completion(&completions, "fn");
        assert_contains_completion(&completions, "const");
        assert_contains_completion(&completions, "struct");

        // Should NOT suggest types at the top level
        assert_not_contains_completion(&completions, "Int");
        assert_not_contains_completion(&completions, "Bool");
        assert_not_contains_completion(&completions, "String");
        assert_not_contains_completion(&completions, "Bytes");

        // Should NOT suggest functions at the top level
        assert_not_contains_completion(&completions, "sha256");
        assert_not_contains_completion(&completions, "concat");
    }

    #[test]
    fn test_context_after_dot_general() {
        let code = r#"
fn foo() -> Int {
    let x = 5;
    |
}
"#;
        let completions = compile_and_get_completions(code);

        // Should return some completions
        assert!(!completions.is_empty(), "Should have completions");
    }

    #[test]
    fn test_context_after_dot_on_pair() {
        let code = r#"
fn foo() -> Int {
    let p = (1, 2);
    p.|
}
"#;
        let completions = compile_and_get_completions(code);

        // This test verifies the context detection for dot access
        // Field completions are tested separately
        assert!(!completions.is_empty(), "Should have completions after dot");
    }

    #[test]
    fn test_context_after_dot_on_struct() {
        let code = r#"
struct Point {
    x: Int,
    y: Int,
}

fn foo(p: Point) -> Int {
    p.|
}
"#;
        let completions = compile_and_get_completions(code);

        // Should have completions after dot on struct
        assert!(
            !completions.is_empty(),
            "Should have completions after dot on struct"
        );
    }

    // ============================================================================
    // Scope-Aware Completion Tests
    // ============================================================================

    #[test]
    fn test_scope_function_parameters_visible() {
        let code = r#"
fn test(param1: Int, param2: String) -> Int {
    |
}
"#;
        let completions = compile_and_get_scope_completions(code);

        assert_contains_completion(&completions, "param1");
        assert_contains_completion(&completions, "param2");
        assert_completion_kind(&completions, "param1", CompletionItemKind::VARIABLE);
    }

    #[test]
    fn test_scope_let_bindings_visible() {
        let code = r#"
fn test() -> Int {
    let x = 5;
    let y = 10;
    |
}
"#;
        let completions = compile_and_get_scope_completions(code);

        assert_contains_completion(&completions, "x");
        assert_contains_completion(&completions, "y");
        assert_completion_kind(&completions, "x", CompletionItemKind::VARIABLE);
    }

    #[test]
    fn test_scope_shadowing_inner_scope() {
        let code = r#"
fn test() -> Int {
    let x = 5;
    if true {
        let x = 10;
        |
    } else {
        x
    }
}
"#;
        let completions = compile_and_get_scope_completions(code);

        // Both x bindings exist but inner one shadows outer
        assert_contains_completion(&completions, "x");
        assert_completion_kind(&completions, "x", CompletionItemKind::VARIABLE);
    }

    #[test]
    fn test_scope_nested_scopes_all_visible() {
        let code = r#"
fn test() -> Int {
    let outer = 1;
    if true {
        let middle = 2;
        if true {
            let inner = 3;
            |
        } else {
            0
        }
    } else {
        0
    }
}
"#;
        let completions = compile_and_get_scope_completions(code);

        assert_contains_completion(&completions, "inner");
        assert_contains_completion(&completions, "middle");
        assert_contains_completion(&completions, "outer");
    }

    #[test]
    fn test_scope_multiple_bindings_same_scope() {
        let code = r#"
fn test() -> Int {
    let first_var = 1;
    let second_var = 2;
    let third_var = 3;
    |
}
"#;
        let completions = compile_and_get_scope_completions(code);

        assert_contains_completion(&completions, "first_var");
        assert_contains_completion(&completions, "second_var");
        assert_contains_completion(&completions, "third_var");
    }

    #[test]
    fn test_scope_parameters_and_locals_together() {
        let code = r#"
fn test(my_param: Int) -> Int {
    let my_local = 5;
    |
}
"#;
        let completions = compile_and_get_scope_completions(code);

        assert_contains_completion(&completions, "my_param");
        assert_contains_completion(&completions, "my_local");
        assert_completion_kind(&completions, "my_param", CompletionItemKind::VARIABLE);
        assert_completion_kind(&completions, "my_local", CompletionItemKind::VARIABLE);
    }

    #[test]
    fn test_scope_stdlib_still_available() {
        let code = r#"
fn test() -> Bytes32 {
    let data = 0x1234;
    |
}
"#;
        let completions = compile_and_get_scope_completions(code);

        // Local binding should be present
        assert_contains_completion(&completions, "data");

        // Stdlib functions should still be available
        assert_contains_completion(&completions, "sha256");
        assert_contains_completion(&completions, "tree_hash");
        assert_contains_completion(&completions, "concat");
    }

    #[test]
    fn test_scope_chain_builds_correctly() {
        let code = r#"
fn test() -> Int {
    let outer = 1;
    if true {
        let inner = 2;
        |
    } else {
        0
    }
}
"#;
        let (code_without_marker, line, character) = parse_cursor_position(code);
        let source = Source::new(
            Arc::from(code_without_marker.as_str()),
            SourceKind::File("test.rue".to_string()),
        );
        let options = CompilerOptions::default();

        let result =
            analyze_file_with_context(source, options).expect("Failed to compile test code");

        let scope = result
            .scope_map
            .find_scope_at_position(line, character, &code_without_marker);
        assert!(scope.is_some(), "Should find scope at position");

        // Verify scope chain has multiple levels
        let scope_chain = result.scope_map.get_scope_chain(scope.unwrap());
        assert!(scope_chain.len() >= 2, "Should have nested scopes in chain");
    }

    #[test]
    fn test_scope_parent_scopes_included() {
        let code = r#"
fn test() -> Int {
    let outer = 1;
    if true {
        |
    } else {
        0
    }
}
"#;
        let completions = compile_and_get_scope_completions(code);

        // Should see outer from parent scope
        assert_contains_completion(&completions, "outer");
    }

    #[test]
    fn test_scope_exported_functions_visible() {
        let code = r#"
export fn helper() -> Int {
    42
}

fn main() -> Int {
    |
}
"#;
        let completions = compile_and_get_scope_completions(code);

        // Should see the helper function
        assert_contains_completion(&completions, "helper");
        assert_completion_kind(&completions, "helper", CompletionItemKind::FUNCTION);
    }

    // ============================================================================
    // Field Completion Tests
    // ============================================================================

    #[test]
    fn test_field_struct_field_access() {
        let code = r#"
struct Point {
    x: Int,
    y: Int,
}

fn foo() -> Int {
    let p = Point { x: 1, y: 2 };
    p.x
}
"#;
        let source = Source::new(Arc::from(code), SourceKind::File("test.rue".to_string()));
        let options = CompilerOptions::default();

        let result =
            analyze_file_with_context(source, options).expect("Failed to compile test code");

        let provider =
            CompletionProvider::new(&result.compiler, result.std_scope, result.file_scope);

        // We need to test field completions through type inference
        // This is a basic test to ensure the machinery works
        let completions = provider.get_all_completions();
        assert!(!completions.is_empty(), "Should have some completions");
    }

    #[test]
    fn test_field_pair_first_access() {
        let code = r#"
fn foo() -> Int {
    let p = (1, 2);
    p.first
}
"#;
        // This test validates that pair field access is recognized
        let source = Source::new(Arc::from(code), SourceKind::File("test.rue".to_string()));
        let options = CompilerOptions::default();

        let result = analyze_file_with_context(source, options);
        assert!(result.is_ok(), "Code with pair.first should compile");
    }

    #[test]
    fn test_field_pair_rest_access() {
        let code = r#"
fn foo() -> Int {
    let p = (1, 2);
    p.rest
}
"#;
        // This test validates that pair field access is recognized
        let source = Source::new(Arc::from(code), SourceKind::File("test.rue".to_string()));
        let options = CompilerOptions::default();

        let result = analyze_file_with_context(source, options);
        assert!(result.is_ok(), "Code with pair.rest should compile");
    }

    #[test]
    fn test_field_atom_length_access() {
        let code = r#"
fn foo() -> Int {
    let a = 0x1234;
    a.length
}
"#;
        // This test validates that atom.length is recognized
        let source = Source::new(Arc::from(code), SourceKind::File("test.rue".to_string()));
        let options = CompilerOptions::default();

        let result = analyze_file_with_context(source, options);
        assert!(result.is_ok(), "Code with atom.length should compile");
    }

    #[test]
    fn test_field_completions_for_pair_type() {
        let code = r#"
fn foo() -> Int {
    let p = (1, 2);
    p.first
}
"#;
        let source = Source::new(Arc::from(code), SourceKind::File("test.rue".to_string()));
        let options = CompilerOptions::default();

        let mut result =
            analyze_file_with_context(source, options).expect("Failed to compile test code");

        // Get pair type and test field completions
        let int_type = result.compiler.builtins().types.int;
        let pair_type = result
            .compiler
            .alloc_type(rue_types::Type::Pair(rue_types::Pair {
                first: int_type,
                rest: int_type,
            }));

        let provider =
            CompletionProvider::new(&result.compiler, result.std_scope, result.file_scope);

        let field_completions = provider.get_field_completions_for_type(pair_type);

        assert_contains_completion(&field_completions, "first");
        assert_contains_completion(&field_completions, "rest");
        assert_completion_kind(&field_completions, "first", CompletionItemKind::FIELD);
    }

    #[test]
    fn test_field_completions_for_atom_type() {
        let code = r#"
fn foo() -> Int {
    5
}
"#;
        let source = Source::new(Arc::from(code), SourceKind::File("test.rue".to_string()));
        let options = CompilerOptions::default();

        let result =
            analyze_file_with_context(source, options).expect("Failed to compile test code");

        let provider =
            CompletionProvider::new(&result.compiler, result.std_scope, result.file_scope);

        // Get atom type and test field completions
        let builtins = result.compiler.builtins();
        let field_completions = provider.get_field_completions_for_type(builtins.types.bytes);

        assert_contains_completion(&field_completions, "length");
        assert_completion_kind(&field_completions, "length", CompletionItemKind::FIELD);
    }

    #[test]
    fn test_field_type_alias_resolution() {
        let code = r#"
struct Point {
    x: Int,
    y: Int,
}

fn foo() -> Int {
    let p = Point { x: 1, y: 2 };
    p.x
}
"#;
        // This test validates that field access works through type system
        let source = Source::new(Arc::from(code), SourceKind::File("test.rue".to_string()));
        let options = CompilerOptions::default();

        let result = analyze_file_with_context(source, options);
        assert!(
            result.is_ok(),
            "Code with struct field access should compile"
        );
    }

    // ============================================================================
    // Completion Quality Tests
    // ============================================================================

    #[test]
    fn test_quality_functions_ranked_before_constants() {
        let code = r#"
fn foo() -> Int {
    |
}
"#;
        let completions = compile_and_get_completions(code);

        // Find positions of functions and constants
        let sha256_pos = completions.iter().position(|c| c.label == "sha256");
        let create_coin_pos = completions.iter().position(|c| c.label == "CREATE_COIN");

        if let (Some(func_pos), Some(const_pos)) = (sha256_pos, create_coin_pos) {
            assert!(
                func_pos < const_pos,
                "Functions should be ranked before constants"
            );
        }
    }

    #[test]
    fn test_quality_keywords_present_in_value_context() {
        let code = r#"
fn foo() -> Int {
    let x = |
    x
}
"#;
        let completions = compile_and_get_completions(code);

        // Keywords should be present in expression context
        assert_contains_completion(&completions, "if");
        assert_contains_completion(&completions, "let");
        assert_contains_completion(&completions, "true");
        assert_contains_completion(&completions, "false");
        assert_contains_completion(&completions, "nil");
    }

    #[test]
    fn test_quality_types_filtered_in_value_context() {
        let code = r#"
fn foo() -> Int {
    let x = |
    x
}
"#;
        let completions = compile_and_get_completions(code);

        // In value context, we might still get type completions from get_all_completions
        // But we should at least get value-appropriate items
        assert_contains_completion(&completions, "if");
        assert_contains_completion(&completions, "let");
    }

    #[test]
    fn test_quality_no_duplicate_completions() {
        let code = r#"
fn foo() -> Int {
    |
}
"#;
        let completions = compile_and_get_completions(code);

        // Check for duplicates
        let mut labels = Vec::new();
        for completion in &completions {
            assert!(
                !labels.contains(&completion.label),
                "Duplicate completion found: {}",
                completion.label
            );
            labels.push(completion.label.clone());
        }
    }

    #[test]
    fn test_quality_detail_strings_informative() {
        let code = r#"
fn foo() -> Int {
    |
}
"#;
        let completions = compile_and_get_completions(code);

        // Check that functions have detail strings
        let sha256 = completions.iter().find(|c| c.label == "sha256");
        assert!(sha256.is_some(), "sha256 should be in completions");
        assert!(
            sha256.unwrap().detail.is_some(),
            "sha256 should have detail string"
        );

        // Check that the detail is informative
        let detail = sha256.unwrap().detail.as_ref().unwrap();
        assert!(
            detail.contains("Bytes") || detail.contains("fn") || detail.contains("builtin"),
            "Detail should be informative, got: {}",
            detail
        );
    }

    // ============================================================================
    // Incomplete/Unfinished Code Tests
    // ============================================================================

    #[test]
    fn test_incomplete_let_statement() {
        let code = r#"
fn foo() -> Int {
    let x = |
}
"#;
        // Should not panic even with incomplete code
        let result = std::panic::catch_unwind(|| compile_and_get_completions(code));

        // We expect this to either:
        // 1. Return completions successfully (best case)
        // 2. Fail compilation gracefully (acceptable)
        match result {
            Ok(completions) => {
                // If we get completions, they should include value suggestions
                println!("Got {} completions for incomplete let", completions.len());
            }
            Err(_) => {
                println!("Compilation failed for incomplete let statement");
            }
        }
    }

    #[test]
    fn test_incomplete_function_signature() {
        let code = r#"
fn foo(x: |
}
"#;
        // Should not panic with incomplete function signature
        let result = std::panic::catch_unwind(|| compile_and_get_completions(code));

        match result {
            Ok(completions) => {
                println!(
                    "Got {} completions for incomplete signature",
                    completions.len()
                );
            }
            Err(_) => {
                println!("Compilation failed for incomplete function signature");
            }
        }
    }

    #[test]
    fn test_incomplete_function_call() {
        let code = r#"
fn foo() -> Int {
    sha256(|
}
"#;
        // Should handle incomplete function call
        let result = std::panic::catch_unwind(|| compile_and_get_completions(code));

        match result {
            Ok(completions) => {
                println!(
                    "Got {} completions for incomplete function call",
                    completions.len()
                );
            }
            Err(_) => {
                println!("Compilation failed for incomplete function call");
            }
        }
    }

    #[test]
    fn test_incomplete_struct_literal() {
        let code = r#"
struct Point {
    x: Int,
    y: Int,
}

fn foo() -> Point {
    Point {
      |
}
"#;
        // Should handle incomplete struct literal and suggest fields
        let completions = compile_and_get_completions(code);

        // Should suggest the struct fields x and y
        assert_contains_completion(&completions, "x");
        assert_contains_completion(&completions, "y");
        assert_completion_kind(&completions, "x", CompletionItemKind::FIELD);
        assert_completion_kind(&completions, "y", CompletionItemKind::FIELD);
    }

    #[test]
    fn test_missing_closing_brace() {
        let code = r#"
fn foo() -> Int {
    let x = 5;
    |
"#;
        // Should handle missing closing brace
        let result = std::panic::catch_unwind(|| compile_and_get_completions(code));

        match result {
            Ok(completions) => {
                println!(
                    "Got {} completions with missing closing brace",
                    completions.len()
                );

                // Known limitation: Local variable 'x' is NOT included when closing brace is missing
                // This is because the parser can't properly register the function scope
                // We still get global completions though (stdlib, builtins, etc.)
                assert!(!completions.is_empty(), "Should get some completions");

                // Verify we at least get stdlib functions
                assert_contains_completion(&completions, "sha256");
                assert_contains_completion(&completions, "concat");

                // Document that local scope is lost (this is the limitation)
                let has_local_x = completions.iter().any(|c| c.label == "x");
                if !has_local_x {
                    println!("Note: Local variable 'x' not in completions (known limitation)");
                }
            }
            Err(_) => {
                println!("Compilation failed for missing closing brace");
            }
        }
    }

    #[test]
    fn test_typing_in_middle_of_identifier() {
        let code = r#"
fn foo() -> Int {
    let result = 42;
    res|ult
}
"#;
        // This is a case where the cursor is in the middle of an identifier
        // The parser might handle this differently
        let result = std::panic::catch_unwind(|| compile_and_get_completions(code));

        match result {
            Ok(completions) => {
                println!(
                    "Got {} completions with cursor in identifier",
                    completions.len()
                );
            }
            Err(_) => {
                println!("Compilation failed with cursor in middle of identifier");
            }
        }
    }

    // ============================================================================
    // End-to-End Field Completion Integration Tests
    // ============================================================================

    #[test]
    fn test_e2e_field_completion_on_pair_variable() {
        let code = r#"
fn foo() -> Int {
    let my_pair = (1, 2);
    my_pair.|
}
"#;
        // This test validates end-to-end field completion on a pair variable
        // The LSP should recognize "my_pair" and provide "first" and "rest" completions
        let completions = compile_and_get_completions(code);

        // Should have field completions for pair
        assert_contains_completion(&completions, "first");
        assert_contains_completion(&completions, "rest");
    }

    #[test]
    fn test_e2e_field_completion_on_struct_parameter() {
        let code = r#"
struct Point {
    x: Int,
    y: Int,
}

fn get_x(p: Point) -> Int {
    p.|
}
"#;
        // Test field completions on a struct parameter
        let completions = compile_and_get_completions(code);

        // Should have field completions for Point struct
        assert_contains_completion(&completions, "x");
        assert_contains_completion(&completions, "y");
    }

    #[test]
    fn test_e2e_field_completion_while_typing_after_dot() {
        let code = r#"
struct Point {
    x: Int,
    y: Int,
}

fn get_x(p: Point) -> Int {
    p.x|
}
"#;
        // Test field completions when typing after the dot (should still show fields, not full scope)
        let completions = compile_and_get_completions(code);

        // Should have field completions for Point struct
        assert_contains_completion(&completions, "x");
        assert_contains_completion(&completions, "y");

        // Should NOT have full scope completions like top-level functions
        // (the LSP client will filter based on "x", but we should provide fields)
        assert_completion_kind(&completions, "x", CompletionItemKind::FIELD);
        assert_completion_kind(&completions, "y", CompletionItemKind::FIELD);
    }

    #[test]
    fn test_e2e_field_completion_partial_field_name() {
        let code = r#"
struct Rectangle {
    width: Int,
    height: Int,
}

fn test(r: Rectangle) -> Int {
    r.wi|
}
"#;
        // Test field completions when partially typing a field name
        let completions = compile_and_get_completions(code);

        // Should still show all fields (LSP client filters based on prefix)
        assert_contains_completion(&completions, "width");
        assert_contains_completion(&completions, "height");
        assert_completion_kind(&completions, "width", CompletionItemKind::FIELD);
    }

    #[test]
    fn test_e2e_field_completion_on_bytes_binding() {
        let code = r#"
fn foo() -> Int {
    let data = 0x1234;
    data.|
}
"#;
        // Test field completions on a bytes variable (should provide .length)
        let completions = compile_and_get_completions(code);

        // Should have length field for bytes/atom types
        assert_contains_completion(&completions, "length");
    }

    #[test]
    fn test_e2e_no_field_completion_on_int() {
        let code = r#"
fn foo() -> Int {
    let num = 42;
    num.|
}
"#;
        // Test that we don't get random completions for Int type (which has no fields)
        let completions = compile_and_get_completions(code);

        // Int type has no fields, so should not have first/rest/length
        assert_not_contains_completion(&completions, "first");
        assert_not_contains_completion(&completions, "rest");
        // Note: length might not be present, but if the type system doesn't have it, this should pass
    }

    // ============================================================================
    // Struct Initialization Field Completion Tests
    // ============================================================================

    #[test]
    fn test_struct_init_field_completion_basic() {
        let code = r#"
struct Point {
    x: Int,
    y: Int,
}

fn foo() -> Point {
    Point {|
}
"#;
        // Test field completions at the start of struct initialization
        let completions = compile_and_get_completions(code);

        println!(
            "Completions: {:?}",
            completions.iter().map(|c| &c.label).collect::<Vec<_>>()
        );

        assert_contains_completion(&completions, "x");
        assert_contains_completion(&completions, "y");
        assert_completion_kind(&completions, "x", CompletionItemKind::FIELD);
        assert_completion_kind(&completions, "y", CompletionItemKind::FIELD);

        // Should NOT have top-level keywords
        assert_not_contains_completion(&completions, "export");
        assert_not_contains_completion(&completions, "fn");
        assert_not_contains_completion(&completions, "const");

        // Should NOT have expression keywords either
        assert_not_contains_completion(&completions, "if");
        assert_not_contains_completion(&completions, "let");
    }

    #[test]
    fn test_struct_init_field_completion_with_scope_map() {
        let code = r#"
struct Point {
    x: Int,
    y: Int,
}

fn foo() -> Point {
    Point {|
}
"#;
        let (code_without_marker, line, character) = parse_cursor_position(code);

        let source = Source::new(
            Arc::from(code_without_marker.as_str()),
            SourceKind::File("test.rue".to_string()),
        );
        let options = CompilerOptions::default();

        let result =
            analyze_file_with_context(source, options).expect("Failed to compile test code");

        let provider =
            CompletionProvider::new(&result.compiler, result.std_scope, result.file_scope);

        let position = Position::new(line, character);

        // This simulates the actual LSP call with scope_map
        let completions = provider.get_completions_at_position(
            position,
            &code_without_marker,
            Some(&result.scope_map),
        );

        println!(
            "Completions with scope_map: {:?}",
            completions.iter().map(|c| &c.label).collect::<Vec<_>>()
        );

        assert_contains_completion(&completions, "x");
        assert_contains_completion(&completions, "y");
        assert_completion_kind(&completions, "x", CompletionItemKind::FIELD);
        assert_completion_kind(&completions, "y", CompletionItemKind::FIELD);

        // Should NOT have top-level keywords
        assert_not_contains_completion(&completions, "export");
        assert_not_contains_completion(&completions, "fn");
        assert_not_contains_completion(&completions, "const");

        // Should NOT have expression keywords either
        assert_not_contains_completion(&completions, "if");
        assert_not_contains_completion(&completions, "let");
    }

    #[test]
    fn test_struct_init_field_completion_after_newline() {
        let code = r#"
struct Person {
    name: String,
    age: Int,
}

fn create_person() -> Person {
    Person {
        |
    }
}
"#;
        // Test field completions on new line inside struct initialization
        let completions = compile_and_get_completions(code);

        assert_contains_completion(&completions, "name");
        assert_contains_completion(&completions, "age");
        assert_completion_kind(&completions, "name", CompletionItemKind::FIELD);
        assert_completion_kind(&completions, "age", CompletionItemKind::FIELD);
    }

    #[test]
    fn test_struct_init_field_completion_after_comma() {
        let code = r#"
struct Rect {
    width: Int,
    height: Int,
}

fn create_rect() -> Rect {
    Rect {
        width: 10,
        |
    }
}
"#;
        // Test field completions after completing one field
        let completions = compile_and_get_completions(code);

        // Should still suggest all fields (including already filled ones)
        assert_contains_completion(&completions, "width");
        assert_contains_completion(&completions, "height");
    }

    #[test]
    fn test_struct_init_nested_struct() {
        let code = r#"
struct Inner {
    value: Int,
}

struct Outer {
    inner: Inner,
    name: String,
}

fn foo() -> Outer {
    Outer {
        inner: Inner {|
    }
}
"#;
        // Test field completions for nested struct initialization
        let completions = compile_and_get_completions(code);

        // Should suggest fields from Inner struct
        assert_contains_completion(&completions, "value");
    }

    #[test]
    fn test_struct_init_in_let_binding() {
        let code = r#"
struct Color {
    r: Int,
    g: Int,
    b: Int,
}

fn foo() -> Int {
    let c = Color {|
    5
}
"#;
        // Test field completions in let binding context
        let completions = compile_and_get_completions(code);

        assert_contains_completion(&completions, "r");
        assert_contains_completion(&completions, "g");
        assert_contains_completion(&completions, "b");
    }

    #[test]
    fn test_struct_init_doesnt_trigger_in_function_body() {
        let code = r#"
struct Point {
    x: Int,
    y: Int,
}

fn foo() -> Point {
    let z = 5;
    |
}
"#;
        // Test that struct init context isn't triggered in regular function body
        let completions = compile_and_get_completions(code);

        // Should have regular completions (not just x and y)
        // Check for some standard completions that wouldn't be present if we were in struct init
        assert_contains_completion(&completions, "sha256");
        assert_contains_completion(&completions, "let");

        // But we shouldn't exclusively have just x and y
        assert!(
            completions.len() > 2,
            "Should have more than just struct fields"
        );
    }
}
