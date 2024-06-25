use std::{collections::HashMap, ops::Not};

use indexmap::IndexMap;

use crate::{
    database::{HirId, TypeId},
    SymbolId,
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
    List(TypeId),
    Struct(StructType),
    Enum(EnumType),
    EnumVariant(EnumVariantType),
    Function(FunctionType),
    Alias(TypeId),
    Optional(TypeId),
    PossiblyUndefined(TypeId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PairType {
    pub first: TypeId,
    pub rest: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructType {
    pub fields: IndexMap<String, TypeId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumType {
    pub has_fields: bool,
    pub variants: IndexMap<String, TypeId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumVariantType {
    pub enum_type: TypeId,
    pub fields: IndexMap<String, TypeId>,
    pub discriminant: HirId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub generic_types: Vec<TypeId>,
    pub param_types: Vec<TypeId>,
    pub rest: Rest,
    pub return_type: TypeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Rest {
    Nil,
    Spread,
    Optional,
}

#[derive(Debug, Clone)]
pub struct Value {
    pub hir_id: HirId,
    pub type_id: TypeId,
    pub guards: Guards,
}

impl Value {
    pub fn new(hir_id: HirId, type_id: TypeId) -> Self {
        Self {
            hir_id,
            type_id,
            guards: HashMap::new(),
        }
    }

    pub fn then_guards(&self) -> HashMap<SymbolId, TypeId> {
        self.guards.iter().map(|(k, v)| (*k, v.then_type)).collect()
    }

    pub fn else_guards(&self) -> HashMap<SymbolId, TypeId> {
        self.guards.iter().map(|(k, v)| (*k, v.else_type)).collect()
    }
}

pub type Guards = HashMap<SymbolId, Guard>;

#[derive(Debug, Clone)]
pub struct Guard {
    pub then_type: TypeId,
    pub else_type: TypeId,
}

impl Guard {
    pub fn new(then_type: TypeId, else_type: TypeId) -> Self {
        Self {
            then_type,
            else_type,
        }
    }
}

impl Not for Guard {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self {
            then_type: self.else_type,
            else_type: self.then_type,
        }
    }
}
