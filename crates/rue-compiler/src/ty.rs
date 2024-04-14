use indexmap::IndexMap;

use crate::database::{HirId, TypeId};

#[derive(Debug, Clone)]
pub enum Type {
    Unknown,
    Nil,
    Any,
    Int,
    Bool,
    Bytes,
    Pair(TypeId, TypeId),
    Union(Vec<TypeId>),
    List(TypeId),
    Struct(StructType),
    Enum(EnumType),
    EnumVariant(EnumVariant),
    Function(FunctionType),
    Alias(TypeId),
}

#[derive(Debug, Clone)]
pub struct StructType {
    fields: IndexMap<String, TypeId>,
}

impl StructType {
    pub fn new(fields: IndexMap<String, TypeId>) -> Self {
        Self { fields }
    }

    pub fn fields(&self) -> &IndexMap<String, TypeId> {
        &self.fields
    }
}

#[derive(Debug, Clone)]
pub struct EnumType {
    variants: IndexMap<String, TypeId>,
}

impl EnumType {
    pub fn new(variants: IndexMap<String, TypeId>) -> Self {
        Self { variants }
    }

    pub fn variants(&self) -> &IndexMap<String, TypeId> {
        &self.variants
    }
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    name: String,
    enum_type: TypeId,
    fields: IndexMap<String, TypeId>,
    discriminant: HirId,
}

impl EnumVariant {
    pub fn new(
        name: String,
        enum_type: TypeId,
        fields: IndexMap<String, TypeId>,
        discriminant: HirId,
    ) -> Self {
        Self {
            name,
            enum_type,
            fields,
            discriminant,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn enum_type(&self) -> TypeId {
        self.enum_type
    }

    pub fn fields(&self) -> &IndexMap<String, TypeId> {
        &self.fields
    }

    pub fn discriminant(&self) -> HirId {
        self.discriminant
    }
}

#[derive(Debug, Clone)]
pub struct FunctionType {
    parameter_types: Vec<TypeId>,
    return_type: TypeId,
}

impl FunctionType {
    pub fn new(parameter_types: Vec<TypeId>, return_type: TypeId) -> Self {
        Self {
            parameter_types,
            return_type,
        }
    }

    pub fn parameter_types(&self) -> &[TypeId] {
        &self.parameter_types
    }

    pub fn return_type(&self) -> TypeId {
        self.return_type
    }
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
