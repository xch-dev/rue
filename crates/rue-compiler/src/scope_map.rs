use std::collections::HashMap;
use rowan::TextRange;
use rue_hir::{ScopeId, SymbolId};

/// Maps source code positions to scopes and tracks scope hierarchy
#[derive(Debug, Default, Clone)]
pub struct ScopeMap {
    /// Maps source byte offset ranges to ScopeId
    /// Sorted by range start for binary search
    scope_ranges: Vec<(TextRange, ScopeId)>,
    
    /// Maps ScopeId to parent ScopeId for hierarchical lookups
    scope_parents: HashMap<ScopeId, ScopeId>,
    
    /// Maps symbols to their definition ranges (for local bindings/parameters)
    symbol_ranges: HashMap<SymbolId, TextRange>,
}

impl ScopeMap {
    pub fn new() -> Self {
        Self::default()
    }
    
    /// Record a scope active at a particular text range
    pub fn add_scope_range(&mut self, range: TextRange, scope: ScopeId) {
        self.scope_ranges.push((range, scope));
    }
    
    /// Record that a scope has a parent scope
    pub fn set_scope_parent(&mut self, scope: ScopeId, parent: ScopeId) {
        self.scope_parents.insert(scope, parent);
    }
    
    /// Record where a symbol is defined
    pub fn add_symbol_range(&mut self, symbol: SymbolId, range: TextRange) {
        self.symbol_ranges.insert(symbol, range);
    }
    
    /// Find the most specific scope at a given position (line, character)
    pub fn find_scope_at_position(&self, line: u32, character: u32, source_text: &str) -> Option<ScopeId> {
        let offset = position_to_offset(line, character, source_text)?;
        
        // Find all scopes that contain this offset
        let mut matching_scopes: Vec<(TextRange, ScopeId)> = self.scope_ranges
            .iter()
            .filter(|(range, _)| {
                let start: usize = range.start().into();
                let end: usize = range.end().into();
                start <= offset && offset <= end
            })
            .copied()
            .collect();
        
        // If we found matching scopes, return the most specific one
        if !matching_scopes.is_empty() {
            // Sort by range size (smallest first = most specific)
            matching_scopes.sort_by_key(|(range, _)| range.len());
            return matching_scopes.first().map(|(_, scope)| *scope);
        }
        
        // Fallback: Find scopes that are "close" to containing this position
        // This handles cases where cursor is at trailing whitespace not included in scope range
        // We want to find the innermost (most specific) scope that's near the cursor
        let tolerance = 200; // Allow cursor to be slightly outside scope end
        
        let mut candidate_scopes: Vec<(TextRange, ScopeId)> = self.scope_ranges
            .iter()
            .filter(|(range, _)| {
                let start: usize = range.start().into();
                let end: usize = range.end().into();
                // Scope starts at or before cursor, and cursor is at or shortly after end
                start <= offset && offset <= end + tolerance
            })
            .copied()
            .collect();
        
        if candidate_scopes.is_empty() {
            return None;
        }
        
        // Sort by range size (smallest first = most specific/innermost)
        candidate_scopes.sort_by_key(|(range, _)| range.len());
        
        // Return the most specific (smallest) scope
        candidate_scopes.first().map(|(_, scope)| *scope)
    }
    
    /// Get the chain of scopes from innermost to outermost
    pub fn get_scope_chain(&self, scope: ScopeId) -> Vec<ScopeId> {
        let mut chain = vec![scope];
        let mut current = scope;
        
        while let Some(&parent) = self.scope_parents.get(&current) {
            chain.push(parent);
            current = parent;
        }
        
        chain
    }
    
    /// Get the range where a symbol is defined
    pub fn get_symbol_range(&self, symbol: SymbolId) -> Option<TextRange> {
        self.symbol_ranges.get(&symbol).copied()
    }
    
    /// Finalize the scope map by sorting scope ranges for efficient lookup
    pub fn finalize(&mut self) {
        self.scope_ranges.sort_by_key(|(range, _)| range.start());
    }
}

/// Convert line/character position to byte offset in source text
fn position_to_offset(line: u32, character: u32, source_text: &str) -> Option<usize> {
    let mut offset = 0;
    let mut current_line = 0;
    
    for text_line in source_text.lines() {
        if current_line == line as usize {
            let char_offset = character as usize;
            // Handle case where character is beyond line length
            let line_char_count = text_line.chars().count();
            if char_offset <= line_char_count {
                // Count bytes up to the character position
                let byte_offset = text_line.chars().take(char_offset).map(|c| c.len_utf8()).sum::<usize>();
                return Some(offset + byte_offset);
            } else {
                return Some(offset + text_line.len());
            }
        }
        offset += text_line.len() + 1; // +1 for newline
        current_line += 1;
    }
    
    // If we're past the end of the file, return the file length
    if current_line == line as usize {
        Some(offset)
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_position_to_offset() {
        let text = "fn main() {\n    let x = 5;\n}";
        
        // Start of file
        assert_eq!(position_to_offset(0, 0, text), Some(0));
        
        // "fn" - character 2
        assert_eq!(position_to_offset(0, 2, text), Some(2));
        
        // Start of second line
        assert_eq!(position_to_offset(1, 0, text), Some(12));
        
        // "let" on second line - character 8
        assert_eq!(position_to_offset(1, 8, text), Some(20));
    }
}

