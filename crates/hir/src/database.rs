use id_arena::Arena;

use crate::{Hir, HirId, Scope, ScopeId, Symbol, SymbolId, Type, TypeId};

#[derive(Debug, Default, Clone)]
pub struct Database {
    hir: Arena<Hir>,
    scopes: Arena<Scope>,
    symbols: Arena<Symbol>,
    types: Arena<Type>,
}

impl Database {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn alloc_hir(&mut self, hir: Hir) -> HirId {
        self.hir.alloc(hir)
    }

    pub fn hir(&self, id: HirId) -> &Hir {
        &self.hir[id]
    }

    pub fn hir_mut(&mut self, id: HirId) -> &mut Hir {
        &mut self.hir[id]
    }

    pub fn alloc_scope(&mut self, scope: Scope) -> ScopeId {
        self.scopes.alloc(scope)
    }

    pub fn scope(&self, id: ScopeId) -> &Scope {
        &self.scopes[id]
    }

    pub fn scope_mut(&mut self, id: ScopeId) -> &mut Scope {
        &mut self.scopes[id]
    }

    pub fn alloc_symbol(&mut self, symbol: Symbol) -> SymbolId {
        self.symbols.alloc(symbol)
    }

    pub fn symbol(&self, id: SymbolId) -> &Symbol {
        &self.symbols[id]
    }

    pub fn symbol_mut(&mut self, id: SymbolId) -> &mut Symbol {
        &mut self.symbols[id]
    }

    pub fn alloc_type(&mut self, ty: Type) -> TypeId {
        self.types.alloc(ty)
    }

    pub fn ty(&self, id: TypeId) -> &Type {
        &self.types[id]
    }

    pub fn ty_mut(&mut self, id: TypeId) -> &mut Type {
        &mut self.types[id]
    }
}
