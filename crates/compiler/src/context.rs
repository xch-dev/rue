use std::ops::Range;

use id_arena::{Arena, Id};
use rowan::TextRange;
use rue_diagnostic::{Diagnostic, DiagnosticKind};
use rue_parser::{SyntaxNode, SyntaxToken};

use crate::{Hir, Mir, Scope, Symbol, Type, Value};

pub type ScopeId = Id<Scope>;
pub type SymbolId = Id<Symbol>;
pub type TypeId = Id<Type>;
pub type HirId = Id<Hir>;
pub type MirId = Id<Mir>;

#[derive(Debug, Clone)]
pub struct Builtins {
    pub unresolved: Value,
    pub unresolved_mir: MirId,
    pub bytes: TypeId,
}

#[derive(Debug, Clone)]
pub struct Context {
    errors: Vec<Diagnostic>,
    scopes: Arena<Scope>,
    symbols: Arena<Symbol>,
    types: Arena<Type>,
    hir: Arena<Hir>,
    mir: Arena<Mir>,
    scope_stack: Vec<ScopeId>,
    builtins: Builtins,
}

impl Default for Context {
    fn default() -> Self {
        let mut types = Arena::new();
        let unresolved_type = types.alloc(Type::Unresolved);

        let mut hir = Arena::new();
        let unresolved_hir = hir.alloc(Hir::Unresolved);

        let mut mir = Arena::new();
        let unresolved_mir = mir.alloc(Mir::Unresolved);

        let builtins = Builtins {
            unresolved: Value::new(unresolved_hir, unresolved_type),
            unresolved_mir,
            bytes: unresolved_type,
        };

        let mut scopes = Arena::new();
        let mut scope = Scope::new();

        scope.insert_type("Bytes".to_string(), builtins.bytes);

        let scope = scopes.alloc(scope);

        Self {
            errors: Vec::new(),
            scopes,
            symbols: Arena::new(),
            types,
            hir,
            mir,
            scope_stack: vec![scope],
            builtins,
        }
    }
}

impl Context {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn errors(&self) -> &[Diagnostic] {
        &self.errors
    }

    pub fn diagnostic(&mut self, node: &impl GetTextRange, kind: DiagnosticKind) {
        let range = node.text_range();
        let span: Range<usize> = range.start().into()..range.end().into();
        self.errors.push(Diagnostic::new(span, kind));
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

    pub fn resolve_symbol(&self, name: &str) -> Option<SymbolId> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(symbol) = self.scope(*scope).symbol(name) {
                return Some(symbol);
            }
        }
        None
    }

    pub fn resolve_type(&self, name: &str) -> Option<TypeId> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(ty) = self.scope(*scope).ty(name) {
                return Some(ty);
            }
        }
        None
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

    pub fn alloc_hir(&mut self, hir: Hir) -> HirId {
        self.hir.alloc(hir)
    }

    pub fn alloc_mir(&mut self, mir: Mir) -> MirId {
        self.mir.alloc(mir)
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

    pub fn symbol_mut(&mut self, id: SymbolId) -> &mut Symbol {
        self.symbols.get_mut(id).unwrap()
    }

    pub fn ty(&self, id: TypeId) -> &Type {
        self.types.get(id).unwrap()
    }

    pub fn ty_mut(&mut self, id: TypeId) -> &mut Type {
        self.types.get_mut(id).unwrap()
    }

    pub fn hir(&self, id: HirId) -> &Hir {
        self.hir.get(id).unwrap()
    }

    pub fn hir_mut(&mut self, id: HirId) -> &mut Hir {
        self.hir.get_mut(id).unwrap()
    }

    pub fn mir(&self, id: MirId) -> &Mir {
        self.mir.get(id).unwrap()
    }

    pub fn builtins(&self) -> &Builtins {
        &self.builtins
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
