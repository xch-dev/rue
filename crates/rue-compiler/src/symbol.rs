use crate::{
    database::{HirId, ScopeId, TypeId},
    ty::FunctionType,
};

#[derive(Debug, Clone)]
pub enum Symbol {
    Unknown,
    Function(Function),
    InlineFunction(Function),
    Parameter(TypeId),
    Let(Let),
    Const(Const),
    InlineConst(Const),
}

impl Symbol {
    pub fn is_parameter(&self) -> bool {
        matches!(self, Symbol::Parameter { .. })
    }

    pub fn is_capturable(&self) -> bool {
        matches!(
            self,
            Symbol::Function(..) | Symbol::Parameter(..) | Symbol::Let(..) | Symbol::Const(..)
        )
    }

    pub fn is_definition(&self) -> bool {
        matches!(
            self,
            Symbol::Function(..) | Symbol::Let(..) | Symbol::Const(..)
        )
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub scope_id: ScopeId,
    pub hir_id: HirId,
    pub ty: FunctionType,
}

#[derive(Debug, Clone, Copy)]
pub struct Let {
    pub type_id: TypeId,
    pub hir_id: HirId,
}

#[derive(Debug, Clone, Copy)]
pub struct Const {
    pub type_id: TypeId,
    pub hir_id: HirId,
}
