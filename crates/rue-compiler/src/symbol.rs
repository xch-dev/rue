use crate::{
    database::{HirId, ScopeId, TypeId},
    ty::FunctionType,
};

#[derive(Debug, Clone)]
pub enum Symbol {
    Unknown,
    Function {
        scope_id: ScopeId,
        hir_id: HirId,
        ty: FunctionType,
        inline: bool,
    },
    Parameter {
        type_id: TypeId,
    },
    LetBinding {
        type_id: TypeId,
        hir_id: HirId,
    },
    ConstBinding {
        type_id: TypeId,
        hir_id: HirId,
        inline: bool,
    },
}

impl Symbol {
    pub fn is_parameter(&self) -> bool {
        matches!(self, Symbol::Parameter { .. })
    }

    pub fn is_capturable(&self) -> bool {
        matches!(
            self,
            Symbol::Function { inline: false, .. }
                | Symbol::Parameter { .. }
                | Symbol::LetBinding { .. }
                | Symbol::ConstBinding { inline: false, .. }
        )
    }

    pub fn is_definition(&self) -> bool {
        matches!(
            self,
            Symbol::Function { inline: false, .. }
                | Symbol::LetBinding { .. }
                | Symbol::ConstBinding { inline: false, .. }
        )
    }

    pub fn is_inline_function(&self) -> bool {
        matches!(self, Symbol::Function { inline: true, .. })
    }
}
