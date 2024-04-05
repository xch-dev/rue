use crate::{
    database::{ScopeId, TypeId},
    value::Value,
};

#[derive(Debug, Clone)]
pub enum Symbol {
    Function {
        scope_id: ScopeId,
        value: Value,
        param_types: Vec<TypeId>,
        ret_type: TypeId,
    },
    Parameter {
        ty: TypeId,
    },
    Binding {
        ty: TypeId,
        value: Value,
    },
}
