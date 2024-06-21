use crate::{
    database::{HirId, ScopeId, TypeId},
    ty::FunctionType,
};

#[derive(Debug, Clone)]
pub enum Symbol {
    Unknown,
    Function(Function),
    Parameter(TypeId),
    Let(Let),
    Const(Const),
}

impl Symbol {
    pub fn is_parameter(&self) -> bool {
        matches!(self, Symbol::Parameter { .. })
    }

    pub fn is_capturable(&self) -> bool {
        matches!(
            self,
            Symbol::Function(Function { inline: false, .. })
                | Symbol::Parameter { .. }
                | Symbol::Let { .. }
                | Symbol::Const(Const { inline: false, .. })
        )
    }

    pub fn is_definition(&self) -> bool {
        matches!(
            self,
            Symbol::Function(Function { inline: false, .. })
                | Symbol::Let { .. }
                | Symbol::Const(Const { inline: false, .. })
        )
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub scope_id: ScopeId,
    pub hir_id: HirId,
    pub ty: FunctionType,
    pub inline: bool,
}

#[derive(Debug, Clone)]
pub struct Let {
    pub type_id: TypeId,
    pub hir_id: HirId,
}

#[derive(Debug, Clone)]
pub struct Const {
    pub type_id: TypeId,
    pub hir_id: HirId,
    pub inline: bool,
}
