use std::collections::HashMap;

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
    Pair(TypeId, TypeId),
    List(TypeId),
    Struct(StructType),
    Enum(EnumType),
    EnumVariant(EnumVariant),
    Function(FunctionType),
    Alias(TypeId),
    Optional(TypeId),
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
    pub parameter_types: Vec<TypeId>,
    pub return_type: TypeId,
    pub varargs: bool,
}

#[derive(Debug, Clone)]
pub struct Value {
    hir: HirId,
    ty: TypeId,
    guards: HashMap<SymbolId, Guard>,
}

impl Value {
    pub fn typed(hir: HirId, ty: TypeId) -> Self {
        Self {
            hir,
            ty,
            guards: HashMap::new(),
        }
    }

    pub fn hir(&self) -> HirId {
        self.hir
    }

    pub fn ty(&self) -> TypeId {
        self.ty
    }

    pub fn guard(&mut self, symbol: SymbolId, guard: Guard) {
        self.guards.insert(symbol, guard);
    }

    pub fn guards(&self) -> &HashMap<SymbolId, Guard> {
        &self.guards
    }

    pub fn then_guards(&self) -> HashMap<SymbolId, TypeId> {
        self.guards
            .iter()
            .map(|(k, v)| (*k, v.then_type()))
            .collect()
    }

    pub fn else_guards(&self) -> HashMap<SymbolId, TypeId> {
        self.guards
            .iter()
            .map(|(k, v)| (*k, v.else_type()))
            .collect()
    }
}

#[derive(Debug, Clone)]
pub struct Guard {
    then_type: TypeId,
    else_type: TypeId,
}

impl Guard {
    pub fn new(then_type: TypeId, else_type: TypeId) -> Self {
        Self {
            then_type,
            else_type,
        }
    }

    pub fn then_type(&self) -> TypeId {
        self.then_type
    }

    pub fn else_type(&self) -> TypeId {
        self.else_type
    }

    pub fn to_reversed(&self) -> Self {
        Self {
            then_type: self.else_type,
            else_type: self.then_type,
        }
    }
}
