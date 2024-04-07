use indexmap::IndexMap;

use crate::{database::TypeId, value::Value};

#[derive(Debug, Clone)]
pub enum Type {
    Unknown,
    Nil,
    Int,
    Bool,
    Bytes,
    List(TypeId),
    Struct {
        named_fields: IndexMap<String, TypeId>,
        fields: Vec<TypeId>,
    },
    Function {
        params: Vec<TypeId>,
        ret: TypeId,
    },
    Alias(TypeId),
}

pub struct Typed {
    pub value: Value,
    pub ty: TypeId,
}
