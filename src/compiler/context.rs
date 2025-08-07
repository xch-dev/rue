use std::ops::Range;

use id_arena::{Arena, Id};
use rowan::TextRange;

use crate::{Error, ErrorKind, Scope, Symbol, SyntaxNode, SyntaxToken, Type};

pub type SymbolId = Id<Symbol>;
pub type TypeId = Id<Type>;

#[derive(Debug, Clone)]
pub struct Context {
    errors: Vec<Error>,
    scope_stack: Vec<Scope>,
    symbols: Arena<Symbol>,
    types: Arena<Type>,
}

impl Default for Context {
    fn default() -> Self {
        Self {
            errors: Vec::new(),
            scope_stack: vec![Scope::new()],
            symbols: Arena::new(),
            types: Arena::new(),
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

    pub fn push_scope(&mut self) {
        self.scope_stack.push(Scope::new());
    }

    pub fn pop_scope(&mut self) {
        self.scope_stack.pop().unwrap();
    }

    pub fn scope(&self) -> &Scope {
        self.scope_stack.last().unwrap()
    }

    pub fn scope_mut(&mut self) -> &mut Scope {
        self.scope_stack.last_mut().unwrap()
    }

    pub fn alloc_symbol(&mut self, symbol: Symbol) -> SymbolId {
        self.symbols.alloc(symbol)
    }

    pub fn alloc_type(&mut self, ty: Type) -> TypeId {
        self.types.alloc(ty)
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
