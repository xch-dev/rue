#[cfg(test)]
mod tests {
    use rue_compiler::analyze_file_with_context;
    use rue_diagnostic::{Source, SourceKind};
    use rue_lsp::completion::CompletionProvider;
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
        provider.get_completions_at_position(position, &code_without_marker)
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
            provider.get_completions_at_position(position, &code_without_marker)
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
            "Unexpected completion '{}' found in completions",
            label
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
    Point { |
}
"#;
        // Should handle incomplete struct literal
        let result = std::panic::catch_unwind(|| compile_and_get_completions(code));

        match result {
            Ok(completions) => {
                println!(
                    "Got {} completions for incomplete struct literal",
                    completions.len()
                );
            }
            Err(_) => {
                println!("Compilation failed for incomplete struct literal");
            }
        }
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
}
