use std::collections::HashSet;

use indexmap::{IndexMap, IndexSet};
use rue_parser::SyntaxToken;

use crate::{dependency_graph::DependencyGraph, Database, ScopeId, SymbolId, TypeId};

use super::unused::Unused;

#[derive(Debug, Default)]
pub struct SymbolTable {
    symbol_tokens: IndexMap<SymbolId, SyntaxToken>,
    type_tokens: IndexMap<TypeId, SyntaxToken>,
    symbol_type_references: IndexMap<SymbolId, IndexSet<TypeId>>,
    type_type_references: IndexMap<TypeId, IndexSet<TypeId>>,
}

impl SymbolTable {
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

    pub fn calculate_unused(
        &self,
        db: &Database,
        dependency_graph: &DependencyGraph,
        root_scope_id: ScopeId,
        main_symbol_id: SymbolId,
    ) -> Unused {
        let mut unused = Unused::default();
        let mut used_symbols = HashSet::new();

        for symbol_id in dependency_graph
            .visited_scopes()
            .into_iter()
            .chain([root_scope_id])
            .flat_map(|scope_id| db.scope(scope_id).local_symbols())
            .collect::<Vec<SymbolId>>()
        {
            if dependency_graph.symbol_usages(symbol_id) > 0 || symbol_id == main_symbol_id {
                used_symbols.insert(symbol_id);
                continue;
            }

            unused.symbol_ids.insert(symbol_id);

            let token = self.symbol_token(symbol_id).unwrap();

            if token.text().starts_with('_') {
                unused.exempt_symbols.insert(symbol_id);
            }
        }

        let directly_used_types = self.named_types().into_iter().filter(|type_id| {
            used_symbols
                .iter()
                .any(|symbol_id| self.type_referenced_by_symbol(*type_id, *symbol_id))
        });

        let mut used_types = HashSet::new();
        for type_id in directly_used_types {
            self.calculate_used_types(type_id, &mut used_types);
        }

        for type_id in self.named_types() {
            if used_types.contains(&type_id) {
                continue;
            }

            unused.type_ids.insert(type_id);

            let token = self.type_token(type_id).unwrap();

            if token.text().starts_with('_') {
                unused.exempt_types.insert(type_id);
            }
        }

        unused
    }

    fn calculate_used_types(&self, type_id: TypeId, used_types: &mut HashSet<TypeId>) {
        if !used_types.insert(type_id) {
            return;
        }

        for referenced_type_id in self.referenced_types_for_type(type_id) {
            self.calculate_used_types(referenced_type_id, used_types);
        }
    }
}
