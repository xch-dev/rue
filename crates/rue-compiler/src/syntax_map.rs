use std::collections::HashSet;

use rowan::TextRange;
use rue_hir::{ScopeId, SymbolId};
use rue_types::TypeId;

#[derive(Debug, Default, Clone)]
pub struct SyntaxMap {
    items: Vec<SyntaxItem>,
}

impl SyntaxMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_item(&mut self, item: SyntaxItem) {
        self.items.push(item);
    }

    pub fn items(&self) -> impl Iterator<Item = &SyntaxItem> {
        self.items.iter()
    }
}

#[derive(Debug, Clone)]
pub struct SyntaxItem {
    pub kind: SyntaxItemKind,
    pub span: TextRange,
}

impl SyntaxItem {
    pub fn new(kind: SyntaxItemKind, span: TextRange) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, Clone)]
pub enum SyntaxItemKind {
    SymbolDeclaration(SymbolId),
    SymbolReference(SymbolId),
    TypeDeclaration(TypeId),
    TypeReference(TypeId),
    FieldDeclaration(SyntaxField),
    FieldReference(SyntaxField),
    FieldInitializer(SyntaxField),
    Scope(ScopeId),
    CompletionContext(CompletionContext),
}

#[derive(Debug, Clone)]
pub struct SyntaxField {
    pub name: String,
    pub container: TypeId,
    pub ty: TypeId,
}

#[derive(Debug, Clone)]
pub enum CompletionContext {
    Document,
    Item,
    Statement,
    Type,
    Expression,
    StructFields {
        ty: TypeId,
        specified_fields: Option<HashSet<String>>,
    },
}
