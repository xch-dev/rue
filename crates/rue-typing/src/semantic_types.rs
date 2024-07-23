use std::collections::HashMap;

use indexmap::IndexSet;

use crate::TypeId;

/// Allows you to map generic types lazily without having to resolve them immediately.
/// This prevents stack overflows when resolving generic type definitions that reference themselves.
/// When reducing a type to its structural form, lazy types should be removed.
#[derive(Debug, Clone)]
pub struct Lazy {
    pub type_id: TypeId,
    pub substitutions: HashMap<TypeId, TypeId>,
}

/// Represents an alias to a type with a set of generic parameters that must be mapped prior to use.
#[derive(Debug, Clone)]
pub struct Alias {
    pub original_type_id: Option<TypeId>,
    pub type_id: TypeId,
    pub generic_types: Vec<TypeId>,
}

/// Struct types are just wrappers around a structural type that provide field information.
#[derive(Debug, Clone)]
pub struct Struct {
    pub original_type_id: Option<TypeId>,
    pub type_id: TypeId,
    pub field_names: IndexSet<String>,
    pub nil_terminated: bool,
    pub generic_types: Vec<TypeId>,
}
