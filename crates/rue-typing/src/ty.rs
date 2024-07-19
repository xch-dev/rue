use std::{
    cmp::{max, min},
    collections::HashSet,
    hash::BuildHasher,
};

use crate::{Comparison, TypeId, TypeSystem};

#[derive(Debug, Clone)]
pub enum Type {
    Unknown,
    Bytes,
    Bytes32,
    PublicKey,
    Int,
    Bool,
    Nil,
    Pair(TypeId, TypeId),
    Union(Vec<TypeId>),
    Ref(TypeId),
}

pub(crate) fn compare_type<S>(
    types: &TypeSystem,
    lhs: TypeId,
    rhs: TypeId,
    visited: &mut HashSet<(TypeId, TypeId), S>,
) -> Comparison
where
    S: BuildHasher,
{
    if !visited.insert((lhs, rhs)) {
        return Comparison::Assignable;
    }

    let comparison = match (types.get(lhs), types.get(rhs)) {
        (Type::Ref(..), _) | (_, Type::Ref(..)) => unreachable!(),

        (Type::Unknown, _) | (_, Type::Unknown) => Comparison::Assignable,

        (Type::Union(lhs), _) => {
            let mut result = Comparison::Assignable;
            for lhs in lhs {
                let comparison = compare_type(types, *lhs, rhs, visited);
                result = max(result, comparison);
            }
            result
        }

        (_, Type::Union(rhs)) => {
            let mut result = Comparison::Assignable;
            for rhs in rhs {
                let comparison = compare_type(types, lhs, *rhs, visited);
                result = min(result, comparison);
            }
            result
        }

        (Type::Pair(lhs_first, lhs_rest), Type::Pair(rhs_first, rhs_rest)) => {
            let first = compare_type(types, *lhs_first, *rhs_first, visited);
            let rest = compare_type(types, *lhs_rest, *rhs_rest, visited);
            max(first, rest)
        }
        (Type::Pair(..), _) | (_, Type::Pair(..)) => Comparison::Incompatible,

        (Type::Bytes, Type::Bytes) => Comparison::Equal,
        (Type::Bytes32, Type::Bytes32) => Comparison::Equal,
        (Type::PublicKey, Type::PublicKey) => Comparison::Equal,
        (Type::Int, Type::Int) => Comparison::Equal,
        (Type::Bool, Type::Bool) => Comparison::Equal,
        (Type::Nil, Type::Nil) => Comparison::Equal,

        (Type::Bytes, Type::Bytes32) => Comparison::Superset,
        (Type::Bytes, Type::PublicKey) => Comparison::Superset,
        (Type::Bytes, Type::Bool) => Comparison::Superset,
        (Type::Bytes, Type::Nil) => Comparison::Superset,
        (Type::Int, Type::Bytes32) => Comparison::Superset,
        (Type::Int, Type::PublicKey) => Comparison::Superset,
        (Type::Int, Type::Bool) => Comparison::Superset,
        (Type::Int, Type::Nil) => Comparison::Superset,

        (Type::Bytes32, Type::Bytes) => Comparison::Assignable,
        (Type::Nil, Type::Bytes) => Comparison::Assignable,

        (Type::Bytes, Type::Int) => Comparison::Castable,
        (Type::Bytes32, Type::Int) => Comparison::Castable,
        (Type::PublicKey, Type::Bytes) => Comparison::Castable,
        (Type::PublicKey, Type::Int) => Comparison::Castable,
        (Type::Int, Type::Bytes) => Comparison::Castable,
        (Type::Nil, Type::Bool) => Comparison::Castable,
        (Type::Nil, Type::Int) => Comparison::Castable,
        (Type::Bool, Type::Bytes) => Comparison::Castable,
        (Type::Bool, Type::Int) => Comparison::Castable,

        (Type::Bytes32, Type::PublicKey) => Comparison::Incompatible,
        (Type::Bytes32, Type::Bool) => Comparison::Incompatible,
        (Type::Bytes32, Type::Nil) => Comparison::Incompatible,
        (Type::PublicKey, Type::Bytes32) => Comparison::Incompatible,
        (Type::PublicKey, Type::Bool) => Comparison::Incompatible,
        (Type::PublicKey, Type::Nil) => Comparison::Incompatible,
        (Type::Bool, Type::Bytes32) => Comparison::Incompatible,
        (Type::Bool, Type::PublicKey) => Comparison::Incompatible,
        (Type::Bool, Type::Nil) => Comparison::Incompatible,
        (Type::Nil, Type::Bytes32) => Comparison::Incompatible,
        (Type::Nil, Type::PublicKey) => Comparison::Incompatible,
    };

    visited.remove(&(lhs, rhs));

    comparison
}
