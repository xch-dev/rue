use std::collections::HashSet;

use rowan::TextRange;
use rue_diagnostic::SourceKind;
use rue_hir::{Declaration, ScopeId, SymbolId};
use rue_types::{FunctionType, TypeId};

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

    /// The number of items added so far, which [`Self::truncate`] goes back to.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Whether there are no items.
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    /// Removes the items added after there were `len` of them.
    pub fn truncate(&mut self, len: usize) {
        self.items.truncate(len);
    }
}

#[derive(Debug, Clone)]
pub struct SyntaxItem {
    pub kind: SyntaxItemKind,
    pub span: TextRange,
    pub source_kind: SourceKind,
}

impl SyntaxItem {
    pub fn new(kind: SyntaxItemKind, span: TextRange, source_kind: SourceKind) -> Self {
        Self {
            kind,
            span,
            source_kind,
        }
    }
}

#[derive(Debug, Clone)]
pub enum SyntaxItemKind {
    FileModule(SymbolId),
    SymbolDeclaration(SymbolId),
    SymbolReference(SymbolId),
    TypeDeclaration(TypeId),
    TypeReference(TypeId),
    FieldDeclaration(SyntaxField),
    FieldReference(SyntaxField),
    FieldInitializer(SyntaxField),
    Call(SyntaxCall),
    Scope(ScopeId),
    CompletionContext(CompletionContext),
}

impl SyntaxItemKind {
    /// A reference to a declaration.
    pub fn reference(declaration: Declaration) -> Self {
        match declaration {
            Declaration::Symbol(symbol) => Self::SymbolReference(symbol),
            Declaration::Type(ty) => Self::TypeReference(ty),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SyntaxField {
    pub name: String,
    pub container: TypeId,
    pub ty: TypeId,
}

/// A call to a function, whose span is the whole call.
#[derive(Debug, Clone)]
pub struct SyntaxCall {
    /// The type of the function that's called, as it's declared.
    pub function: FunctionType,
    /// The function's generics that were inferred from the call, and what they were inferred as.
    pub generics: Vec<(TypeId, TypeId)>,
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
    ModuleExports {
        module: SymbolId,
        /// Whether `super` can come next, after a path of `super`s.
        allow_super: bool,
        /// Whether the module was named by `super` or `project`, so it contains the code and its
        /// private items (and a file's sibling modules) can be used, rather than only its exports.
        members: bool,
    },
}
