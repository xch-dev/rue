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
