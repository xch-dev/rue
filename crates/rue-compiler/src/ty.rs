use std::collections::HashMap;

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
        named_fields: HashMap<String, TypeId>,
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
