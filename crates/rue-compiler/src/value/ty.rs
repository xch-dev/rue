use std::collections::HashMap;

use indexmap::IndexMap;
use num_bigint::BigInt;

use crate::{database::TypeId, ScopeId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Unknown,
    Generic,
    Any,
    Nil,
    Int,
    Bool,
    Bytes,
    Bytes32,
    PublicKey,
    Pair(PairType),
    Union(Vec<TypeId>),
    Struct(StructType),
    Enum(EnumType),
    EnumVariant(EnumVariantType),
    Function(FunctionType),
    Alias(AliasType),
    Ref(TypeId),
    Lazy(LazyType),
    Optional(TypeId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AliasType {
    pub type_id: TypeId,
    pub generic_types: Vec<TypeId>,
    pub scope_id: ScopeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LazyType {
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
    pub discriminant: BigInt,
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
