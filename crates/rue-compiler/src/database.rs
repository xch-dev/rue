use id_arena::{Arena, Id};

use crate::{scope::Scope, symbol::Symbol};

#[derive(Default)]
pub struct Database {
    scopes: Arena<Scope>,
    symbols: Arena<Symbol>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(Id<Symbol>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(Id<Scope>);

impl Database {
    pub fn alloc_scope(&mut self, scope: Scope) -> ScopeId {
        ScopeId(self.scopes.alloc(scope))
    }

    pub fn alloc_symbol(&mut self, symbol: Symbol) -> SymbolId {
        SymbolId(self.symbols.alloc(symbol))
    }

    pub fn scope(&self, id: ScopeId) -> &Scope {
        &self.scopes[id.0]
    }

    pub fn symbol(&self, id: SymbolId) -> &Symbol {
        &self.symbols[id.0]
    }

    pub fn scope_mut(&mut self, id: ScopeId) -> &mut Scope {
        &mut self.scopes[id.0]
    }

    pub fn symbol_mut(&mut self, id: SymbolId) -> &mut Symbol {
        &mut self.symbols[id.0]
    }
}
