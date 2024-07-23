use indexmap::IndexSet;
use rue_typing::{Callable, TypeId};

use crate::{
    database::{HirId, ScopeId},
    value::Value,
    SymbolId,
};

#[derive(Debug, Clone)]
pub enum Symbol {
    Unknown,
    Function(Function),
    InlineFunction(Function),
    Parameter(TypeId),
    Let(Value),
    Const(Value),
    InlineConst(Value),
    Module(Module),
}

impl Symbol {
    pub fn is_capturable(&self) -> bool {
        matches!(
            self,
            Symbol::Function(..) | Symbol::Parameter(..) | Symbol::Let(..) | Symbol::Const(..)
        )
    }

    pub fn is_definable(&self) -> bool {
        matches!(
            self,
            Symbol::Function(..) | Symbol::Let(..) | Symbol::Const(..)
        )
    }

    pub fn is_constant(&self) -> bool {
        matches!(
            self,
            Symbol::Const(..) | Symbol::InlineConst(..) | Symbol::InlineFunction(..)
        )
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub scope_id: ScopeId,
    pub hir_id: HirId,
    pub ty: Callable,
}

#[derive(Debug, Clone)]
pub struct Module {
    pub scope_id: ScopeId,
    pub exported_types: IndexSet<TypeId>,
    pub exported_symbols: IndexSet<SymbolId>,
}
