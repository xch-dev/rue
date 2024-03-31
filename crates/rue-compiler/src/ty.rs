use crate::{database::TypeId, value::Value};

#[derive(Debug, Clone)]
pub enum Type {
    Unknown,
    Int,
    Bool,
    Function { params: Vec<TypeId>, ret: TypeId },
}

pub struct Typed {
    pub value: Value,
    pub ty: TypeId,
}
