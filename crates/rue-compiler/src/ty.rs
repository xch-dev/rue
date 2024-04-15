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
    Pair(TypeId, TypeId),
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
    varargs: bool,
}

impl FunctionType {
    pub fn new(parameter_types: Vec<TypeId>, return_type: TypeId, varargs: bool) -> Self {
        Self {
            parameter_types,
            return_type,
            varargs,
        }
    }

    pub fn parameter_types(&self) -> &[TypeId] {
        &self.parameter_types
    }

    pub fn return_type(&self) -> TypeId {
        self.return_type
    }

    pub fn varargs(&self) -> bool {
        self.varargs
    }
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
}
