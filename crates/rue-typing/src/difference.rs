use std::collections::HashSet;

use crate::{StandardTypes, Struct, Type, TypeId, TypeSystem};

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
        (Type::Bool, Type::Bool) => std.never,
        (Type::Nil, Type::Nil) => std.never,

        (Type::Int, Type::Bytes32) => lhs,
        (Type::Int, Type::PublicKey) => lhs,
        (Type::Int, Type::Bytes) => std.never,
        (Type::Int, Type::Atom) => std.never,
        (Type::Int, Type::Bool) => lhs,
        (Type::Int, Type::Nil) => lhs,

        (Type::Bytes, Type::Bytes32) => lhs,
        (Type::Bytes, Type::PublicKey) => lhs,
        (Type::Bytes, Type::Int) => std.never,
        (Type::Bytes, Type::Atom) => std.never,
        (Type::Bytes, Type::Bool) => lhs,
        (Type::Bytes, Type::Nil) => lhs,

        (Type::Atom, Type::Bytes32) => lhs,
        (Type::Atom, Type::PublicKey) => lhs,
        (Type::Atom, Type::Int) => std.never,
        (Type::Atom, Type::Bytes) => std.never,
        (Type::Atom, Type::Bool) => lhs,
        (Type::Atom, Type::Nil) => lhs,

        (Type::Bytes32, Type::PublicKey) => lhs,
        (Type::Bytes32, Type::Bytes) => std.never,
        (Type::Bytes32, Type::Int) => std.never,
        (Type::Bytes32, Type::Atom) => std.never,
        (Type::Bytes32, Type::Bool) => lhs,
        (Type::Bytes32, Type::Nil) => lhs,

        (Type::PublicKey, Type::Bytes32) => lhs,
        (Type::PublicKey, Type::Bytes) => std.never,
        (Type::PublicKey, Type::Int) => std.never,
        (Type::PublicKey, Type::Atom) => std.never,
        (Type::PublicKey, Type::Bool) => lhs,
        (Type::PublicKey, Type::Nil) => lhs,

        (Type::Bool, Type::Bytes32) => lhs,
        (Type::Bool, Type::PublicKey) => lhs,
        (Type::Bool, Type::Bytes) => std.never,
        (Type::Bool, Type::Atom) => std.never,
        (Type::Bool, Type::Int) => std.never,
        (Type::Bool, Type::Nil) => lhs,

        (Type::Nil, Type::Bytes32) => lhs,
        (Type::Nil, Type::PublicKey) => lhs,
        (Type::Nil, Type::Bytes) => std.never,
        (Type::Nil, Type::Atom) => std.never,
        (Type::Nil, Type::Int) => std.never,
        (Type::Nil, Type::Bool) => std.never,

        (Type::Atom, Type::Pair(..)) => lhs,
        (Type::Bytes, Type::Pair(..)) => lhs,
        (Type::Bytes32, Type::Pair(..)) => lhs,
        (Type::PublicKey, Type::Pair(..)) => lhs,
        (Type::Int, Type::Pair(..)) => lhs,
        (Type::Bool, Type::Pair(..)) => lhs,
        (Type::Nil, Type::Pair(..)) => lhs,

        (Type::Pair(..), Type::Atom) => lhs,
        (Type::Pair(..), Type::Bytes) => lhs,
        (Type::Pair(..), Type::Bytes32) => lhs,
        (Type::Pair(..), Type::PublicKey) => lhs,
        (Type::Pair(..), Type::Int) => lhs,
        (Type::Pair(..), Type::Bool) => lhs,
        (Type::Pair(..), Type::Nil) => lhs,

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
        (Type::Callable(..), _) => lhs,
        (_, Type::Callable(..)) => lhs,
    };

    visited.remove(&(lhs, rhs));

    result
}
