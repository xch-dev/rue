use std::collections::HashMap;

use indexmap::{IndexMap, IndexSet};
use num_bigint::BigInt;

use crate::{Comparison, Type, TypeId, TypeSystem};

/// The kind of ending that a list has.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Rest {
    /// This means that the list is nil-terminated.
    #[default]
    Nil,
    /// This means that there is no special terminator for the list.
    /// The last element is the rest value.
    Spread,
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
    /// A pointer to the enum from which this was derived, if any.
    pub original_type_id: Option<TypeId>,
    /// The structural type of the enum.
    pub type_id: TypeId,
    /// Whether the enum semantically has fields.
    pub has_fields: bool,
    /// This is a map of the original variant names to their type ids.
    pub variants: IndexMap<String, TypeId>,
}

/// Represents a variant type which can optionally have fields.
#[derive(Debug, Clone)]
pub struct Variant {
    /// A pointer to the variant from which this was derived, if any.
    pub original_type_id: Option<TypeId>,
    /// The enum type to which this variant belongs.
    pub enum_type: TypeId,
    /// The field names of the variant.
    pub field_names: Option<IndexSet<String>>,
    /// The structural type of the enum variant.
    pub type_id: TypeId,
    /// The rest kind of the variant.
    pub rest: Rest,
    /// The generic types of the variant.
    pub generic_types: Vec<TypeId>,
    /// The discriminant value.
    pub discriminant: BigInt,
}

/// Constructs a structural type consisting of the items in a list.
pub fn construct_items(
    db: &mut TypeSystem,
    items: impl DoubleEndedIterator<Item = TypeId>,
    rest: Rest,
) -> TypeId {
    let mut result = db.std().nil;
    for (i, item) in items.rev().enumerate() {
        if i == 0 {
            match rest {
                Rest::Spread => {
                    result = item;
                    continue;
                }
                Rest::Optional => {
                    result = db.alloc(Type::Pair(item, result));
                    result = db.alloc(Type::Union(vec![result, db.std().nil]));
                    continue;
                }
                Rest::Nil => {}
            }
        }
        result = db.alloc(Type::Pair(item, result));
    }
    result
}

/// Deconstructs a structural type into a list of items and a rest value.
pub fn deconstruct_items(
    db: &mut TypeSystem,
    type_id: TypeId,
    length: usize,
    rest: Rest,
) -> Option<Vec<TypeId>> {
    let mut items = Vec::with_capacity(length);
    let mut current = type_id;

    for i in (0..length).rev() {
        if i == 0 {
            match rest {
                Rest::Spread => {
                    items.push(current);
                    break;
                }
                Rest::Optional => {
                    if db.compare(db.std().nil, current) > Comparison::Assignable {
                        return None;
                    }

                    let non_nil = db.difference(current, db.std().nil);
                    let (first, rest) = db.get_pair(non_nil)?;

                    if db.compare(rest, db.std().nil) > Comparison::Equal {
                        return None;
                    }

                    items.push(first);
                    break;
                }
                Rest::Nil => {
                    let (first, rest) = db.get_pair(current)?;
                    items.push(first);
                    if db.compare(rest, db.std().nil) > Comparison::Equal {
                        return None;
                    }
                    break;
                }
            }
        }
        let (first, rest) = db.get_pair(current)?;
        items.push(first);
        current = rest;
    }

    Some(items)
}

/// Unwraps a list type into its inner type.
pub fn unwrap_list(db: &mut TypeSystem, type_id: TypeId) -> Option<TypeId> {
    if db.compare(db.std().nil, type_id) > Comparison::Assignable {
        return None;
    }

    let non_nil = db.difference(type_id, db.std().nil);
    let (first, rest) = db.get_pair(non_nil)?;

    if db.compare(rest, type_id) > Comparison::Assignable {
        return None;
    }

    Some(first)
}

#[cfg(test)]
mod tests {
    use crate::alloc_list;

    use super::*;

    #[test]
    fn test_construct_int_nil() {
        let mut db = TypeSystem::new();
        let std = db.std();
        let type_id = construct_items(&mut db, [std.int].into_iter(), Rest::Nil);
        let items = deconstruct_items(&mut db, type_id, 1, Rest::Nil);
        assert_eq!(items, Some(vec![std.int]));
    }

    #[test]
    fn test_construct_int() {
        let mut db = TypeSystem::new();
        let std = db.std();
        let type_id = construct_items(&mut db, [std.int].into_iter(), Rest::Spread);
        let items = deconstruct_items(&mut db, type_id, 1, Rest::Spread);
        assert_eq!(items, Some(vec![std.int]));
    }

    #[test]
    fn test_construct_int_optional() {
        let mut db = TypeSystem::new();
        let std = db.std();
        let type_id = construct_items(&mut db, [std.int].into_iter(), Rest::Optional);
        let items = deconstruct_items(&mut db, type_id, 1, Rest::Optional);
        assert_eq!(items, Some(vec![std.int]));
    }

    #[test]
    fn test_construct_empty_nil() {
        let mut db = TypeSystem::new();
        let type_id = construct_items(&mut db, [].into_iter(), Rest::Nil);
        let items = deconstruct_items(&mut db, type_id, 0, Rest::Nil);
        assert_eq!(items, Some(vec![]));
    }

    #[test]
    fn test_construct_empty() {
        let mut db = TypeSystem::new();
        let type_id = construct_items(&mut db, [].into_iter(), Rest::Spread);
        let items = deconstruct_items(&mut db, type_id, 0, Rest::Spread);
        assert_eq!(items, Some(vec![]));
    }

    #[test]
    fn test_construct_empty_optional() {
        let mut db = TypeSystem::new();
        let type_id = construct_items(&mut db, [].into_iter(), Rest::Optional);
        let items = deconstruct_items(&mut db, type_id, 0, Rest::Optional);
        assert_eq!(items, Some(vec![]));
    }

    #[test]
    fn test_construct_int_int_nil() {
        let mut db = TypeSystem::new();
        let std = db.std();
        let type_id = construct_items(&mut db, [std.int, std.int].into_iter(), Rest::Nil);
        let items = deconstruct_items(&mut db, type_id, 2, Rest::Nil);
        assert_eq!(items, Some(vec![std.int, std.int]));
    }

    #[test]
    fn test_construct_int_int() {
        let mut db = TypeSystem::new();
        let std = db.std();
        let type_id = construct_items(&mut db, [std.int, std.int].into_iter(), Rest::Spread);
        let items = deconstruct_items(&mut db, type_id, 2, Rest::Spread);
        assert_eq!(items, Some(vec![std.int, std.int]));
    }

    #[test]
    fn test_construct_int_int_optional() {
        let mut db = TypeSystem::new();
        let std = db.std();
        let type_id = construct_items(&mut db, [std.int, std.int].into_iter(), Rest::Optional);
        let items = deconstruct_items(&mut db, type_id, 2, Rest::Optional);
        assert_eq!(items, Some(vec![std.int, std.int]));
    }

    #[test]
    fn test_construct_bytes32_pair_nil() {
        let mut db = TypeSystem::new();
        let std = db.std();
        let pair = db.alloc(Type::Pair(std.bytes32, std.nil));
        let type_id = construct_items(&mut db, [std.bytes32, pair].into_iter(), Rest::Nil);
        let items = deconstruct_items(&mut db, type_id, 2, Rest::Nil);
        assert_eq!(items, Some(vec![std.bytes32, pair]));
    }

    #[test]
    fn test_construct_bytes32_pair() {
        let mut db = TypeSystem::new();
        let std = db.std();
        let pair = db.alloc(Type::Pair(std.bytes32, std.nil));
        let type_id = construct_items(&mut db, [std.bytes32, pair].into_iter(), Rest::Spread);
        let items = deconstruct_items(&mut db, type_id, 2, Rest::Spread);
        assert_eq!(items, Some(vec![std.bytes32, pair]));
    }

    #[test]
    fn test_construct_bytes32_pair_optional() {
        let mut db = TypeSystem::new();
        let std = db.std();
        let pair = db.alloc(Type::Pair(std.bytes32, std.nil));
        let type_id = construct_items(&mut db, [std.bytes32, pair].into_iter(), Rest::Optional);
        let items = deconstruct_items(&mut db, type_id, 2, Rest::Optional);
        assert_eq!(items, Some(vec![std.bytes32, pair]));
    }

    #[test]
    fn test_unwrap_list() {
        let mut db = TypeSystem::new();
        let std = db.std();
        let list = alloc_list(&mut db, std.public_key);
        assert_eq!(unwrap_list(&mut db, list), Some(std.public_key));
    }
}
