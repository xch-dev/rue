use std::collections::HashMap;

use indexmap::IndexSet;
use num_bigint::BigInt;

use crate::TypeId;

/// The kind of ending that a list has.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Rest {
    /// This means that there is no special terminator for the list.
    /// The last element is the rest value.
    Spread,
    /// This means that the list is nil-terminated.
    Nil,
    /// This means that the list is nil-terminated, but may contain an additional optional value.
    Optional,
}

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
    pub field_names: IndexSet<String>,
    pub type_id: TypeId,
    pub rest: Rest,
    pub generic_types: Vec<TypeId>,
}

/// Represents something which can be called with arguments and returns a given type.
#[derive(Debug, Clone)]
pub struct Callable {
    pub original_type_id: Option<TypeId>,
    pub parameter_names: IndexSet<String>,
    pub parameters: TypeId,
    pub return_type: TypeId,
    pub rest: Rest,
    pub generic_types: Vec<TypeId>,
}

/// Represents an enum type which can have multiple variants.
#[derive(Debug, Clone)]
pub struct Enum {
    pub original_variants: Vec<TypeId>,
    pub has_fields: bool,
}

/// Represents a variant type which can optionally have fields.
#[derive(Debug, Clone)]
pub struct Variant {
    pub original_type_id: Option<TypeId>,
    pub enum_type: TypeId,
    pub field_names: IndexSet<String>,
    pub fields: Option<VariantFields>,
    pub generic_types: Vec<TypeId>,
    pub discriminant: BigInt,
}

/// Field information for a variant type.
#[derive(Debug, Clone)]
pub struct VariantFields {
    pub fields: TypeId,
    pub rest: Rest,
}
