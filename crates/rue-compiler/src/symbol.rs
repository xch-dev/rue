use crate::database::{HirId, ScopeId, TypeId};

#[derive(Debug, Clone)]
pub enum Symbol {
    Function {
        scope_id: ScopeId,
        hir_id: HirId,
        parameter_types: Vec<TypeId>,
        return_type: TypeId,
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
    },
}

impl Symbol {
    pub fn is_parameter(&self) -> bool {
        matches!(self, Symbol::Parameter { .. })
    }

    pub fn is_capturable(&self) -> bool {
        !matches!(self, Symbol::ConstBinding { .. })
    }

    pub fn is_definition(&self) -> bool {
        matches!(self, Symbol::Function { .. } | Symbol::LetBinding { .. })
    }
}
