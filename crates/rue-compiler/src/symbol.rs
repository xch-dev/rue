use crate::{database::ScopeId, value::Value};

#[derive(Debug, Clone)]
pub enum Symbol {
    Function { scope_id: ScopeId, value: Value },
    Parameter,
}
