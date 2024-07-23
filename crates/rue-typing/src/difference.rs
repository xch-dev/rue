use std::collections::HashSet;

use num_bigint::BigInt;
use num_traits::One;

use crate::{bigint_to_bytes, Enum, StandardTypes, Struct, Type, TypeId, TypeSystem, Variant};

pub(crate) fn difference_type(
    types: &mut TypeSystem,
    std: &StandardTypes,
    lhs: TypeId,
    rhs: TypeId,
    visited: &mut HashSet<(TypeId, TypeId)>,
) -> TypeId {
    if !visited.insert((lhs, rhs)) {
        return lhs;
    }

    let result = match (types.get(lhs), types.get(rhs)) {
        (Type::Ref(..), _) | (_, Type::Ref(..)) => unreachable!(),
        (Type::Lazy(..), _) | (_, Type::Lazy(..)) => unreachable!(),

        (Type::Generic, _) | (_, Type::Generic) => lhs,
        (Type::Unknown, _) | (_, Type::Unknown) => lhs,

        (Type::Never, _) => std.never,
        (_, Type::Never) => lhs,

        (Type::Atom, Type::Atom) => std.never,
        (Type::Bytes, Type::Bytes) => std.never,
        (Type::Bytes32, Type::Bytes32) => std.never,
        (Type::PublicKey, Type::PublicKey) => std.never,
        (Type::Int, Type::Int) => std.never,
        (Type::Nil, Type::Nil) => std.never,
        (Type::True, Type::True) => std.never,
        (Type::False, Type::False) => std.never,

        (Type::Int, Type::Bytes32) => lhs,
        (Type::Int, Type::PublicKey) => lhs,
        (Type::Int, Type::Bytes) => std.never,
        (Type::Int, Type::Atom) => std.never,
        (Type::Int, Type::Nil) => lhs,
        (Type::Int, Type::True) => lhs,
        (Type::Int, Type::False) => lhs,
        (Type::Int, Type::Value(..)) => lhs,

        (Type::Bytes, Type::Bytes32) => lhs,
        (Type::Bytes, Type::PublicKey) => lhs,
        (Type::Bytes, Type::Int) => std.never,
        (Type::Bytes, Type::Atom) => std.never,
        (Type::Bytes, Type::Nil) => lhs,
        (Type::Bytes, Type::True) => lhs,
        (Type::Bytes, Type::False) => lhs,
        (Type::Bytes, Type::Value(..)) => lhs,

        (Type::Atom, Type::Bytes32) => lhs,
        (Type::Atom, Type::PublicKey) => lhs,
        (Type::Atom, Type::Int) => std.never,
        (Type::Atom, Type::Bytes) => std.never,
        (Type::Atom, Type::Nil) => lhs,
        (Type::Atom, Type::True) => lhs,
        (Type::Atom, Type::False) => lhs,
        (Type::Atom, Type::Value(..)) => lhs,

        (Type::Bytes32, Type::PublicKey) => lhs,
        (Type::Bytes32, Type::Bytes) => std.never,
        (Type::Bytes32, Type::Int) => std.never,
        (Type::Bytes32, Type::Atom) => std.never,
        (Type::Bytes32, Type::Nil) => lhs,
        (Type::Bytes32, Type::True) => lhs,
        (Type::Bytes32, Type::False) => lhs,
        (Type::Bytes32, Type::Value(..)) => lhs,

        (Type::PublicKey, Type::Bytes32) => lhs,
        (Type::PublicKey, Type::Bytes) => std.never,
        (Type::PublicKey, Type::Int) => std.never,
        (Type::PublicKey, Type::Atom) => std.never,
        (Type::PublicKey, Type::Nil) => lhs,
        (Type::PublicKey, Type::True) => lhs,
        (Type::PublicKey, Type::False) => lhs,
        (Type::PublicKey, Type::Value(..)) => lhs,

        (Type::Nil, Type::Bytes32) => lhs,
        (Type::Nil, Type::PublicKey) => lhs,
        (Type::Nil, Type::Bytes) => std.never,
        (Type::Nil, Type::Atom) => std.never,
        (Type::Nil, Type::Int) => std.never,
        (Type::Nil, Type::True) => lhs,
        (Type::Nil, Type::False) => std.never,

        (Type::True, Type::Atom) => std.never,
        (Type::True, Type::Bytes) => std.never,
        (Type::True, Type::Bytes32) => lhs,
        (Type::True, Type::PublicKey) => lhs,
        (Type::True, Type::Int) => std.never,
        (Type::True, Type::Nil) => lhs,
        (Type::True, Type::False) => lhs,

        (Type::False, Type::Atom) => std.never,
        (Type::False, Type::Bytes) => std.never,
        (Type::False, Type::Bytes32) => lhs,
        (Type::False, Type::PublicKey) => lhs,
        (Type::False, Type::Int) => std.never,
        (Type::False, Type::Nil) => lhs,
        (Type::False, Type::True) => lhs,

        (Type::Nil, Type::Value(value)) => {
            if value == &BigInt::ZERO {
                std.never
            } else {
                lhs
            }
        }
        (Type::False, Type::Value(value)) => {
            if value == &BigInt::ZERO {
                std.never
            } else {
                lhs
            }
        }
        (Type::True, Type::Value(value)) => {
            if value == &BigInt::one() {
                std.never
            } else {
                lhs
            }
        }

        (Type::Value(..), Type::Atom) => std.never,
        (Type::Value(..), Type::Bytes) => std.never,
        (Type::Value(..), Type::Int) => std.never,

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
        (Type::Value(value), Type::True) => {
            if value == &BigInt::one() {
                std.never
            } else {
                lhs
            }
        }
        (Type::Value(value), Type::False) => {
            if value == &BigInt::ZERO {
                std.never
            } else {
                lhs
            }
        }
        (Type::Value(value), Type::Nil) => {
            if value == &BigInt::ZERO {
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

        (Type::Atom, Type::Pair(..)) => lhs,
        (Type::Bytes, Type::Pair(..)) => lhs,
        (Type::Bytes32, Type::Pair(..)) => lhs,
        (Type::PublicKey, Type::Pair(..)) => lhs,
        (Type::Int, Type::Pair(..)) => lhs,
        (Type::Nil, Type::Pair(..)) => lhs,
        (Type::True, Type::Pair(..)) => lhs,
        (Type::False, Type::Pair(..)) => lhs,
        (Type::Value(..), Type::Pair(..)) => lhs,

        (Type::Pair(..), Type::Atom) => lhs,
        (Type::Pair(..), Type::Bytes) => lhs,
        (Type::Pair(..), Type::Bytes32) => lhs,
        (Type::Pair(..), Type::PublicKey) => lhs,
        (Type::Pair(..), Type::Int) => lhs,
        (Type::Pair(..), Type::Nil) => lhs,
        (Type::Pair(..), Type::True) => lhs,
        (Type::Pair(..), Type::False) => lhs,
        (Type::Pair(..), Type::Value(..)) => lhs,

        (Type::Pair(lhs_first, lhs_rest), Type::Pair(rhs_first, rhs_rest)) => {
            let (lhs_first, lhs_rest) = (*lhs_first, *lhs_rest);
            let (rhs_first, rhs_rest) = (*rhs_first, *rhs_rest);

            let first = difference_type(types, std, lhs_first, rhs_first, visited);
            let rest = difference_type(types, std, lhs_rest, rhs_rest, visited);

            if matches!(types.get(first), Type::Never) || matches!(types.get(first), Type::Never) {
                std.never
            } else if first == lhs_first && rest == lhs_rest {
                lhs
            } else {
                types.alloc(Type::Pair(first, rest))
            }
        }

        (Type::Union(items), _) => {
            let items = items.clone();

            let mut result = Vec::new();

            for item in &items {
                let item = difference_type(types, std, *item, rhs, visited);
                if matches!(types.get(item), Type::Never) {
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
                lhs = difference_type(types, std, lhs, item, visited);
            }
            lhs
        }

        (Type::Alias(alias), _) => difference_type(types, std, alias.type_id, rhs, visited),
        (_, Type::Alias(alias)) => difference_type(types, std, lhs, alias.type_id, visited),

        (Type::Struct(ty), _) => {
            let ty = ty.clone();
            let type_id = difference_type(types, std, ty.type_id, rhs, visited);

            types.alloc(Type::Struct(Struct {
                original_type_id: Some(ty.original_type_id.unwrap_or(lhs)),
                type_id,
                field_names: ty.field_names,
                rest: ty.rest,
                generic_types: ty.generic_types,
            }))
        }
        (_, Type::Struct(ty)) => {
            let ty = ty.clone();
            let type_id = difference_type(types, std, lhs, ty.type_id, visited);

            types.alloc(Type::Struct(Struct {
                original_type_id: Some(ty.original_type_id.unwrap_or(rhs)),
                type_id,
                field_names: ty.field_names,
                rest: ty.rest,
                generic_types: ty.generic_types,
            }))
        }

        (Type::Enum(ty), _) => {
            let ty = ty.clone();
            let type_id = difference_type(types, std, ty.type_id, rhs, visited);

            types.alloc(Type::Enum(Enum {
                original_type_id: Some(ty.original_type_id.unwrap_or(lhs)),
                type_id,
                has_fields: ty.has_fields,
                variants: ty.variants,
            }))
        }
        (_, Type::Enum(ty)) => {
            let ty = ty.clone();
            let type_id = difference_type(types, std, lhs, ty.type_id, visited);

            types.alloc(Type::Enum(Enum {
                original_type_id: Some(ty.original_type_id.unwrap_or(rhs)),
                type_id,
                has_fields: ty.has_fields,
                variants: ty.variants,
            }))
        }

        (Type::Variant(variant), _) => {
            let variant = variant.clone();
            let type_id = difference_type(types, std, variant.type_id, rhs, visited);

            types.alloc(Type::Variant(Variant {
                original_type_id: Some(variant.original_type_id.unwrap_or(lhs)),
                enum_type: variant.enum_type,
                field_names: variant.field_names,
                type_id,
                rest: variant.rest,
                generic_types: variant.generic_types,
            }))
        }
        (_, Type::Variant(variant)) => {
            let variant = variant.clone();
            let type_id = difference_type(types, std, lhs, variant.type_id, visited);

            types.alloc(Type::Variant(Variant {
                original_type_id: Some(variant.original_type_id.unwrap_or(rhs)),
                enum_type: variant.enum_type,
                field_names: variant.field_names,
                type_id,
                rest: variant.rest,
                generic_types: variant.generic_types,
            }))
        }

        (Type::Callable(..), _) => lhs,
        (_, Type::Callable(..)) => lhs,
    };

    visited.remove(&(lhs, rhs));

    result
}
