use std::ops::Range;

use id_arena::{Arena, Id};
use rowan::TextRange;

use crate::{Error, ErrorKind, Scope, Symbol, SyntaxNode, SyntaxToken, Type};

pub type ScopeId = Id<Scope>;
pub type SymbolId = Id<Symbol>;
pub type TypeId = Id<Type>;

#[derive(Debug, Clone)]
pub struct Context {
    errors: Vec<Error>,
    scopes: Arena<Scope>,
    symbols: Arena<Symbol>,
    types: Arena<Type>,
    scope_stack: Vec<ScopeId>,
}

impl Default for Context {
    fn default() -> Self {
        let mut scopes = Arena::new();
        let scope = scopes.alloc(Scope::new());

        Self {
            errors: Vec::new(),
            scopes,
            symbols: Arena::new(),
            types: Arena::new(),
            scope_stack: vec![scope],
        }
    }
}

impl Context {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn error(&mut self, node: &impl GetTextRange, kind: ErrorKind) {
        let range = node.text_range();
        let span: Range<usize> = range.start().into()..range.end().into();
        self.errors.push(Error::new(span, kind));
    }

    pub fn push_scope(&mut self, scope: ScopeId) {
        self.scope_stack.push(scope);
    }

    pub fn pop_scope(&mut self) {
        self.scope_stack.pop().unwrap();
    }

    pub fn last_scope(&self) -> &Scope {
        self.scope(*self.scope_stack.last().unwrap())
    }

    pub fn last_scope_mut(&mut self) -> &mut Scope {
        self.scope_mut(*self.scope_stack.last().unwrap())
    }

    pub fn alloc_scope(&mut self, scope: Scope) -> ScopeId {
        self.scopes.alloc(scope)
    }

    pub fn alloc_symbol(&mut self, symbol: Symbol) -> SymbolId {
        self.symbols.alloc(symbol)
    }

    pub fn alloc_type(&mut self, ty: Type) -> TypeId {
        self.types.alloc(ty)
    }

    pub fn scope(&self, id: ScopeId) -> &Scope {
        self.scopes.get(id).unwrap()
    }

    pub fn scope_mut(&mut self, id: ScopeId) -> &mut Scope {
        self.scopes.get_mut(id).unwrap()
    }

    pub fn symbol(&self, id: SymbolId) -> &Symbol {
        self.symbols.get(id).unwrap()
    }

    pub fn ty(&self, id: TypeId) -> &Type {
        self.types.get(id).unwrap()
    }
}

pub trait GetTextRange {
    fn text_range(&self) -> TextRange;
}

impl GetTextRange for SyntaxNode {
    fn text_range(&self) -> TextRange {
        self.text_range()
    }
}

impl GetTextRange for SyntaxToken {
    fn text_range(&self) -> TextRange {
        self.text_range()
    }
}
