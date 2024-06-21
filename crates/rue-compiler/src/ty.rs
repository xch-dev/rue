use std::{collections::HashMap, ops::Not};

use indexmap::IndexMap;

use crate::{
    database::{HirId, TypeId},
    SymbolId,
};

#[derive(Debug, Clone)]
pub enum Type {
    Unknown,
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
    EnumVariant(EnumVariant),
    Function(FunctionType),
    Alias(TypeId),
    Optional(TypeId),
}

#[derive(Debug, Clone)]
pub struct PairType {
    pub first: TypeId,
    pub rest: TypeId,
}

#[derive(Debug, Clone)]
pub struct StructType {
    pub fields: IndexMap<String, TypeId>,
}

#[derive(Debug, Clone)]
pub struct EnumType {
    pub variants: IndexMap<String, TypeId>,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: String,
    pub enum_type: TypeId,
    pub fields: IndexMap<String, TypeId>,
    pub discriminant: HirId,
}

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub param_types: Vec<TypeId>,
    pub rest: Rest,
    pub return_type: TypeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Rest {
    Nil,
    Parameter,
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
