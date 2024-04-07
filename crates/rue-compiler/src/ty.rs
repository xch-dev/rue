use indexmap::IndexMap;

use crate::database::{HirId, TypeId};

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
        parameter_types: Vec<TypeId>,
        return_type: TypeId,
    },
    Alias(TypeId),
}

#[derive(Debug, Clone, Copy)]
pub struct Typed {
    pub value: HirId,
    pub ty: TypeId,
}
