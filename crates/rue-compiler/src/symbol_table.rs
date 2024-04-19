use indexmap::{IndexMap, IndexSet};
use rue_parser::SyntaxToken;

use crate::{SymbolId, TypeId};

#[derive(Debug, Default)]
pub struct GlobalSymbolTable {
    symbol_tokens: IndexMap<SymbolId, SyntaxToken>,
    type_tokens: IndexMap<TypeId, SyntaxToken>,
    symbol_type_references: IndexMap<SymbolId, IndexSet<TypeId>>,
    type_type_references: IndexMap<TypeId, IndexSet<TypeId>>,
}

impl GlobalSymbolTable {
    pub fn insert_symbol(&mut self, symbol_id: SymbolId, token: SyntaxToken) {
        self.symbol_tokens.insert(symbol_id, token);
    }

    pub fn insert_type(&mut self, type_id: TypeId, token: SyntaxToken) {
        self.type_tokens.insert(type_id, token);
    }

    pub fn insert_symbol_type_reference(&mut self, symbol_id: SymbolId, type_id: TypeId) {
        self.symbol_type_references
            .entry(symbol_id)
            .or_default()
            .insert(type_id);
    }

    pub fn insert_type_type_reference(&mut self, type_id: TypeId, referenced_type_id: TypeId) {
        self.type_type_references
            .entry(type_id)
            .or_default()
            .insert(referenced_type_id);
    }

    pub fn type_referenced_by_symbol(&self, type_id: TypeId, symbol_id: SymbolId) -> bool {
        self.symbol_type_references
            .get(&symbol_id)
            .map(|type_ids| type_ids.contains(&type_id))
            .unwrap_or(false)
    }

    pub fn type_referenced_by_type(&self, type_id: TypeId, referenced_type_id: TypeId) -> bool {
        self.type_type_references
            .get(&type_id)
            .map(|type_ids| type_ids.contains(&referenced_type_id))
            .unwrap_or(false)
    }

    pub fn referenced_types_for_type(&self, type_id: TypeId) -> Vec<TypeId> {
        self.type_type_references
            .get(&type_id)
            .map(|type_ids| type_ids.iter().copied().collect())
            .unwrap_or_default()
    }

    pub fn symbol_token(&self, symbol_id: SymbolId) -> Option<&SyntaxToken> {
        self.symbol_tokens.get(&symbol_id)
    }

    pub fn named_types(&self) -> Vec<TypeId> {
        self.type_tokens.keys().copied().collect()
    }

    pub fn type_token(&self, type_id: TypeId) -> Option<&SyntaxToken> {
        self.type_tokens.get(&type_id)
    }
}
