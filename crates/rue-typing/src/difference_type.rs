use crate::HashSet;

use num_bigint::BigInt;
use num_traits::One;

use crate::{bigint_to_bytes, Enum, Struct, Type, TypeId, TypeSystem, Variant};

pub(crate) fn difference_type(
    types: &mut TypeSystem,
    lhs: TypeId,
    rhs: TypeId,
    visited: &mut HashSet<(TypeId, TypeId)>,
) -> TypeId {
    let std = types.std();

    if !visited.insert((lhs, rhs)) {
        return lhs;
    }

    let result = match (types.get(lhs), types.get(rhs)) {
        (Type::Ref(..) | Type::Lazy(..), _) | (_, Type::Ref(..) | Type::Lazy(..)) => unreachable!(),

        // If you subtract a supertype or equal type, there are no other possible types.
        (Type::Never, _)
        | (_, Type::Any | Type::Unknown | Type::Generic | Type::Callable(..))
        | (Type::Bytes | Type::Int | Type::Value(..), Type::Bytes | Type::Int)
        | (Type::Bytes32, Type::Bytes32 | Type::Bytes | Type::Int)
        | (Type::PublicKey, Type::PublicKey | Type::Bytes | Type::Int)
        | (Type::Nil | Type::False, Type::Nil | Type::Bytes | Type::Int | Type::False)
        | (Type::True, Type::True | Type::Bytes | Type::Int) => std.never,

        // If you subtract something which is a subtype, the result is the same as the original type.
        (_, Type::Never)
        | (
            Type::Any
            | Type::Int
            | Type::Bytes
            | Type::Unknown
            | Type::Generic
            | Type::Callable(..),
            Type::Bytes32
            | Type::PublicKey
            | Type::Nil
            | Type::True
            | Type::False
            | Type::Value(..),
        )
        | (
            Type::Unknown | Type::Generic | Type::Callable(..),
            Type::Int | Type::Bytes | Type::Pair(..),
        )
        | (
            Type::Bytes32,
            Type::PublicKey | Type::Nil | Type::True | Type::False | Type::Value(..),
        )
        | (
            Type::PublicKey,
            Type::Bytes32 | Type::Nil | Type::True | Type::False | Type::Value(..),
        )
        | (Type::Nil | Type::False, Type::Bytes32 | Type::PublicKey | Type::True)
        | (Type::True, Type::Bytes32 | Type::PublicKey | Type::Nil | Type::False)
        | (
            Type::Pair(..),
            Type::Bytes
            | Type::Bytes32
            | Type::PublicKey
            | Type::Int
            | Type::Nil
            | Type::True
            | Type::False
            | Type::Value(..),
        )
        | (
            Type::Bytes
            | Type::Bytes32
            | Type::PublicKey
            | Type::Int
            | Type::Nil
            | Type::True
            | Type::False
            | Type::Value(..),
            Type::Pair(..),
        ) => lhs,

        // Any is defined as either Bytes or (Any, Any).
        (Type::Any, Type::Pair(..)) => std.bytes,
        (Type::Any, Type::Bytes | Type::Int) => types.alloc(Type::Pair(std.any, std.any)),

        (Type::Nil | Type::False, Type::Value(value))
        | (Type::Value(value), Type::Nil | Type::False) => {
            if value == &BigInt::ZERO {
                std.never
            } else {
                lhs
            }
        }

        (Type::True, Type::Value(value)) | (Type::Value(value), Type::True) => {
            if value == &BigInt::one() {
                std.never
            } else {
                lhs
            }
        }

        (Type::Value(value), Type::Bytes32) => {
            if bigint_to_bytes(value.clone()).len() == 32 {
                std.never
            } else {
                lhs
            }
        }
        (Type::Value(value), Type::PublicKey) => {
            if bigint_to_bytes(value.clone()).len() == 48 {
                std.never
            } else {
                lhs
            }
        }

        (Type::Value(lhs_value), Type::Value(rhs_value)) => {
            if lhs_value == rhs_value {
                std.never
            } else {
                lhs
            }
        }

        (Type::Pair(lhs_first, lhs_rest), Type::Pair(rhs_first, rhs_rest)) => {
            let (lhs_first, lhs_rest) = (*lhs_first, *lhs_rest);
            let (rhs_first, rhs_rest) = (*rhs_first, *rhs_rest);

            let first = difference_type(types, lhs_first, rhs_first, visited);
            let rest = difference_type(types, lhs_rest, rhs_rest, visited);

            if matches!(types.get(first), Type::Never) || matches!(types.get(first), Type::Never) {
                std.never
            } else if first == lhs_first && rest == lhs_rest {
                lhs
            } else {
                types.alloc(Type::Pair(first, rest))
            }
        }

        (Type::Alias(alias), _) => difference_type(types, alias.type_id, rhs, visited),
        (_, Type::Alias(alias)) => difference_type(types, lhs, alias.type_id, visited),

        (Type::Struct(ty), _) => {
            let ty = ty.clone();
            let type_id = difference_type(types, ty.type_id, rhs, visited);

            types.alloc(Type::Struct(Struct {
                original_type_id: ty.original_type_id,
                type_id,
                field_names: ty.field_names,
                nil_terminated: ty.nil_terminated,
                generic_types: ty.generic_types,
            }))
        }
        (_, Type::Struct(ty)) => difference_type(types, lhs, ty.type_id, visited),

        (Type::Enum(ty), _) => {
            let ty = ty.clone();
            let type_id = difference_type(types, ty.type_id, rhs, visited);

            types.alloc(Type::Enum(Enum {
                original_type_id: ty.original_type_id,
                type_id,
                has_fields: ty.has_fields,
                variants: ty.variants,
            }))
        }
        (_, Type::Enum(ty)) => difference_type(types, lhs, ty.type_id, visited),

        (Type::Variant(variant), _) => {
            let variant = variant.clone();
            let type_id = difference_type(types, variant.type_id, rhs, visited);

            types.alloc(Type::Variant(Variant {
                original_type_id: variant.original_type_id,
                original_enum_type_id: variant.original_enum_type_id,
                field_names: variant.field_names,
                type_id,
                nil_terminated: variant.nil_terminated,
                generic_types: variant.generic_types,
                discriminant: variant.discriminant,
            }))
        }
        (_, Type::Variant(variant)) => difference_type(types, lhs, variant.type_id, visited),

        (Type::Union(items), _) => {
            let items = items.clone();

            let mut result = Vec::new();

            for item in &items {
                let item = difference_type(types, *item, rhs, visited);
                if matches!(types.get_recursive(item), Type::Never) {
                    continue;
                }
                result.push(item);
            }

            if result.is_empty() {
                std.never
            } else if result.len() == 1 {
                result[0]
            } else if result == items {
                lhs
            } else {
                types.alloc(Type::Union(result))
            }
        }

        (_, Type::Union(items)) => {
            let items = items.clone();
            let mut lhs = lhs;
            for item in items {
                lhs = difference_type(types, lhs, item, visited);
            }
            lhs
        }
    };

    visited.remove(&(lhs, rhs));

    result
}

#[cfg(test)]
mod tests {
    use crate::{alloc_list, Comparison};

    use super::*;

    #[test]
    fn test_difference_list_nil() {
        let mut db = TypeSystem::new();
        let types = db.std();

        let generic = db.alloc(Type::Generic);
        let list = alloc_list(&mut db, generic);
        let non_nil = db.difference(list, types.nil);

        assert_eq!(db.compare(non_nil, list), Comparison::Assignable);
        assert_eq!(db.compare(types.nil, non_nil), Comparison::NotEqual);
    }
}
