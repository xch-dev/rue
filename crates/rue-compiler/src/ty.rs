use crate::{database::TypeId, value::Value};

#[derive(Debug, Clone)]
pub enum Type {
    Unknown,
    Nil,
    Int,
    Bool,
    Bytes,
    List(TypeId),
    Function { params: Vec<TypeId>, ret: TypeId },
    Alias(TypeId),
}

pub struct Typed {
    pub value: Value,
    pub ty: TypeId,
}
