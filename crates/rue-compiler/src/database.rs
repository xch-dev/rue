use id_arena::{Arena, Id};

use crate::{hir::Hir, lir::Lir, scope::Scope, symbol::Symbol, ty::Type};

#[derive(Default)]
pub struct Database {
    scopes: Arena<Scope>,
    symbols: Arena<Symbol>,
    types: Arena<Type>,
    hir: Arena<Hir>,
    lir: Arena<Lir>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(Id<Symbol>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(Id<Scope>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(Id<Type>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct HirId(Id<Hir>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LirId(Id<Lir>);

impl Database {
    pub(crate) fn alloc_scope(&mut self, scope: Scope) -> ScopeId {
        ScopeId(self.scopes.alloc(scope))
    }

    pub(crate) fn alloc_symbol(&mut self, symbol: Symbol) -> SymbolId {
        SymbolId(self.symbols.alloc(symbol))
    }

    pub(crate) fn alloc_type(&mut self, ty: Type) -> TypeId {
        TypeId(self.types.alloc(ty))
    }

    pub(crate) fn alloc_hir(&mut self, hir: Hir) -> HirId {
        HirId(self.hir.alloc(hir))
    }

    pub(crate) fn alloc_lir(&mut self, lir: Lir) -> LirId {
        LirId(self.lir.alloc(lir))
    }

    pub fn scope(&self, id: ScopeId) -> &Scope {
        &self.scopes[id.0]
    }

    pub fn symbol(&self, id: SymbolId) -> &Symbol {
        &self.symbols[id.0]
    }

    pub fn ty(&self, id: TypeId) -> &Type {
        &self.types[id.0]
    }

    pub(crate) fn ty_mut(&mut self, id: TypeId) -> &mut Type {
        &mut self.types[id.0]
    }

    pub fn hir(&self, id: HirId) -> &Hir {
        &self.hir[id.0]
    }

    pub fn lir(&self, id: LirId) -> &Lir {
        &self.lir[id.0]
    }

    pub(crate) fn scope_mut(&mut self, id: ScopeId) -> &mut Scope {
        &mut self.scopes[id.0]
    }

    pub(crate) fn symbol_mut(&mut self, id: SymbolId) -> &mut Symbol {
        &mut self.symbols[id.0]
    }
}
