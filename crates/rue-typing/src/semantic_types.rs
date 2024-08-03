use indexmap::{IndexMap, IndexSet};
use num_bigint::BigInt;

use crate::{Comparison, Type, TypeId, TypeSystem};

/// Allows you to map generic types lazily without having to resolve them immediately.
/// This prevents stack overflows when resolving generic type definitions that reference themselves.
/// When reducing a type to its structural form, lazy types should be removed.
#[derive(Debug, Clone)]
pub struct Lazy {
    pub type_id: TypeId,
    pub substitutions: IndexMap<TypeId, TypeId>,
}

/// Represents an alias to a type with a set of generic parameters that must be mapped prior to use.
#[derive(Debug, Clone)]
pub struct Alias {
    /// A pointer to the alias from which this was derived.
    pub original_type_id: TypeId,
    pub type_id: TypeId,
    pub generic_types: Vec<TypeId>,
}

/// Struct types are just wrappers around a structural type that provide field information.
#[derive(Debug, Clone)]
pub struct Struct {
    /// A pointer to the struct from which this was derived.
    pub original_type_id: TypeId,
    pub field_names: IndexSet<String>,
    pub type_id: TypeId,
    pub nil_terminated: bool,
    pub generic_types: Vec<TypeId>,
}

/// Represents something which can be called with arguments and returns a given type.
#[derive(Debug, Clone)]
pub struct Callable {
    /// A pointer to the callable from which this was derived.
    pub original_type_id: TypeId,
    pub parameter_names: IndexSet<String>,
    pub parameters: TypeId,
    pub return_type: TypeId,
    pub nil_terminated: bool,
    pub generic_types: Vec<TypeId>,
}

/// Represents an enum type which can have multiple variants.
#[derive(Debug, Clone)]
pub struct Enum {
    /// A pointer to the enum from which this was derived.
    pub original_type_id: TypeId,
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
    /// A pointer to the variant from which this was derived.
    pub original_type_id: TypeId,
    /// The original enum type to which this variant belongs.
    pub original_enum_type_id: TypeId,
    /// The field names of the variant.
    pub field_names: Option<IndexSet<String>>,
    /// The structural type of the enum variant.
    pub type_id: TypeId,
    /// Whether the chain of cons pairs is nil terminated.
    pub nil_terminated: bool,
    /// The generic types of the variant.
    pub generic_types: Vec<TypeId>,
    /// The discriminant value.
    pub discriminant: BigInt,
}

/// Constructs a structural type consisting of the items in a list.
pub fn construct_items(
    db: &mut TypeSystem,
    items: impl DoubleEndedIterator<Item = TypeId>,
    nil_terminated: bool,
) -> TypeId {
    let mut result = db.std().nil;
    for (i, item) in items.rev().enumerate() {
        if i == 0 && !nil_terminated {
            result = item;
            continue;
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
    nil_terminated: bool,
) -> Option<Vec<TypeId>> {
    let mut items = Vec::with_capacity(length);
    let mut current = type_id;

    for i in (0..length).rev() {
        if i == 0 {
            if !nil_terminated {
                items.push(current);
                break;
            }

            let (first, rest) = db.get_pair(current)?;
            items.push(first);
            if db.compare(rest, db.std().nil) > Comparison::Equal {
                return None;
            }
            break;
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
        let type_id = construct_items(&mut db, [std.int].into_iter(), true);
        let items = deconstruct_items(&mut db, type_id, 1, true);
        assert_eq!(items, Some(vec![std.int]));
    }

    #[test]
    fn test_construct_int() {
        let mut db = TypeSystem::new();
        let std = db.std();
        let type_id = construct_items(&mut db, [std.int].into_iter(), false);
        let items = deconstruct_items(&mut db, type_id, 1, false);
        assert_eq!(items, Some(vec![std.int]));
    }

    #[test]
    fn test_construct_empty_nil() {
        let mut db = TypeSystem::new();
        let type_id = construct_items(&mut db, [].into_iter(), true);
        let items = deconstruct_items(&mut db, type_id, 0, true);
        assert_eq!(items, Some(vec![]));
    }

    #[test]
    fn test_construct_empty() {
        let mut db = TypeSystem::new();
        let type_id = construct_items(&mut db, [].into_iter(), false);
        let items = deconstruct_items(&mut db, type_id, 0, false);
        assert_eq!(items, Some(vec![]));
    }

    #[test]
    fn test_construct_int_int_nil() {
        let mut db = TypeSystem::new();
        let std = db.std();
        let type_id = construct_items(&mut db, [std.int, std.int].into_iter(), true);
        let items = deconstruct_items(&mut db, type_id, 2, true);
        assert_eq!(items, Some(vec![std.int, std.int]));
    }

    #[test]
    fn test_construct_int_int() {
        let mut db = TypeSystem::new();
        let std = db.std();
        let type_id = construct_items(&mut db, [std.int, std.int].into_iter(), false);
        let items = deconstruct_items(&mut db, type_id, 2, false);
        assert_eq!(items, Some(vec![std.int, std.int]));
    }

    #[test]
    fn test_construct_bytes32_pair_nil() {
        let mut db = TypeSystem::new();
        let std = db.std();
        let pair = db.alloc(Type::Pair(std.bytes32, std.nil));
        let type_id = construct_items(&mut db, [std.bytes32, pair].into_iter(), true);
        let items = deconstruct_items(&mut db, type_id, 2, true);
        assert_eq!(items, Some(vec![std.bytes32, pair]));
    }

    #[test]
    fn test_construct_bytes32_pair() {
        let mut db = TypeSystem::new();
        let std = db.std();
        let pair = db.alloc(Type::Pair(std.bytes32, std.nil));
        let type_id = construct_items(&mut db, [std.bytes32, pair].into_iter(), false);
        let items = deconstruct_items(&mut db, type_id, 2, false);
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
