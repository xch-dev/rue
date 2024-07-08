use std::collections::HashMap;

use indexmap::IndexMap;

use crate::{
    database::{HirId, TypeId},
    ScopeId,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Unknown,
    Generic,
    Nil,
    Any,
    Int,
    Bool,
    Bytes,
    Bytes32,
    PublicKey,
    Pair(PairType),
    Union(Vec<TypeId>),
    List(TypeId),
    Struct(StructType),
    Enum(EnumType),
    EnumVariant(EnumVariantType),
    Function(FunctionType),
    Alias(AliasType),
    Ref(TypeId),
    Substitute(SubstitutionType),
    Nullable(TypeId),
    Optional(TypeId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AliasType {
    pub type_id: TypeId,
    pub generic_types: Vec<TypeId>,
    pub scope_id: ScopeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SubstitutionType {
    pub type_id: TypeId,
    pub substitutions: HashMap<TypeId, TypeId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PairType {
    pub first: TypeId,
    pub rest: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructType {
    pub original_type_id: TypeId,
    pub fields: IndexMap<String, TypeId>,
    pub rest: Rest,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumType {
    pub has_fields: bool,
    pub variants: IndexMap<String, TypeId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumVariantType {
    pub enum_type: TypeId,
    pub original_type_id: TypeId,
    pub fields: Option<IndexMap<String, TypeId>>,
    pub rest: Rest,
    pub discriminant: HirId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub generic_types: Vec<TypeId>,
    pub param_types: Vec<TypeId>,
    pub rest: Rest,
    pub return_type: TypeId,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum Rest {
    #[default]
    Nil,
    Spread,
    Optional,
}
