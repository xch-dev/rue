use indexmap::IndexMap;

use crate::database::{HirId, TypeId};

#[derive(Debug, Clone)]
pub enum Type {
    Unknown,
    Int,
    Bool,
    Bytes,
    Union(Vec<TypeId>),
    Tuple(Vec<TypeId>),
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

#[derive(Debug, Clone)]
pub struct Value {
    hir: HirId,
    ty: TypeId,
}

impl Value {
    pub fn typed(hir: HirId, ty: TypeId) -> Self {
        Self { hir, ty }
    }

    pub fn hir(&self) -> HirId {
        self.hir
    }

    pub fn ty(&self) -> TypeId {
        self.ty
    }
}
