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
}

#[derive(Debug, Clone, Copy)]
pub struct SyntaxItem {
    pub kind: SyntaxItemKind,
    pub span: TextRange,
}

impl SyntaxItem {
    pub fn new(kind: SyntaxItemKind, span: TextRange) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum SyntaxItemKind {
    Symbol(SymbolId),
    Type(TypeId),
    Scope(ScopeId),
}
