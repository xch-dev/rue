use std::{
    cmp::{max, min},
    collections::HashSet,
    hash::BuildHasher,
};

use crate::{Comparison, StandardTypes, TypeId, TypeSystem};

#[derive(Debug, Clone)]
pub enum Type {
    Unknown,
    Never,
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

        // TODO: Is this correct?
        (Type::Never, _) => Comparison::Assignable,
        (_, Type::Never) => Comparison::Superset,

        (Type::Union(items), _) => {
            let items = items.clone();
            let mut result = Comparison::Assignable;
            let mut incompatible_count = 0;

            let length = items.len();

            for item in items {
                let cmp = compare_type(types, item, rhs, visited);
                result = max(result, cmp);
                if cmp == Comparison::Incompatible {
                    incompatible_count += 1;
                }
            }

            if incompatible_count == length {
                Comparison::Incompatible
            } else {
                min(result, Comparison::Superset)
            }
        }

        (_, Type::Union(items)) => {
            let items = items.clone();
            let mut result = Comparison::Incompatible;

            for item in items {
                let cmp = compare_type(types, lhs, item, visited);
                result = min(result, cmp);
            }

            max(result, Comparison::Assignable)
        }

        // (Type::Union(lhs), _) => {
        //     let mut results = Vec::new();
        //     for lhs in lhs {
        //         let comparison = compare_type(types, *lhs, rhs, visited);
        //         results.push(comparison);
        //     }
        //     if results.iter().all(|cmp| cmp == &Comparison::Incompatible) {
        //         Comparison::Incompatible
        //     } else {
        //         max(
        //             min(*results.iter().max().unwrap(), Comparison::Superset),
        //             Comparison::Assignable,
        //         )
        //     }
        // }

        // (_, Type::Union(rhs)) => {
        //     let mut result = Comparison::Assignable;
        //     for rhs in rhs {
        //         let comparison = compare_type(types, lhs, *rhs, visited);
        //         result = min(result, comparison);
        //     }
        //     max(result, Comparison::Assignable)
        // }
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

pub fn difference_type<S>(
    types: &mut TypeSystem,
    std: &StandardTypes,
    lhs: TypeId,
    rhs: TypeId,
    visited: &mut HashSet<(TypeId, TypeId), S>,
) -> TypeId
where
    S: BuildHasher,
{
    if !visited.insert((lhs, rhs)) {
        return lhs;
    }

    let result = match (types.get(lhs), types.get(rhs)) {
        (Type::Ref(..), _) | (_, Type::Ref(..)) => unreachable!(),

        (Type::Unknown, _) | (_, Type::Unknown) => lhs,

        (Type::Never, _) => std.never,
        (_, Type::Never) => lhs,

        (Type::Bytes, Type::Bytes) => std.never,
        (Type::Bytes32, Type::Bytes32) => std.never,
        (Type::PublicKey, Type::PublicKey) => std.never,
        (Type::Int, Type::Int) => std.never,
        (Type::Bool, Type::Bool) => std.never,
        (Type::Nil, Type::Nil) => std.never,

        (Type::Int, Type::Bytes32) => lhs,
        (Type::Int, Type::PublicKey) => lhs,
        (Type::Int, Type::Bytes) => std.never,
        (Type::Int, Type::Bool) => lhs,
        (Type::Int, Type::Nil) => lhs,

        (Type::Bytes, Type::Bytes32) => lhs,
        (Type::Bytes, Type::PublicKey) => lhs,
        (Type::Bytes, Type::Int) => std.never,
        (Type::Bytes, Type::Bool) => lhs,
        (Type::Bytes, Type::Nil) => lhs,

        (Type::Bytes32, Type::PublicKey) => lhs,
        (Type::Bytes32, Type::Bytes) => std.never,
        (Type::Bytes32, Type::Int) => std.never,
        (Type::Bytes32, Type::Bool) => lhs,
        (Type::Bytes32, Type::Nil) => lhs,

        (Type::PublicKey, Type::Bytes32) => lhs,
        (Type::PublicKey, Type::Bytes) => std.never,
        (Type::PublicKey, Type::Int) => std.never,
        (Type::PublicKey, Type::Bool) => lhs,
        (Type::PublicKey, Type::Nil) => lhs,

        (Type::Bool, Type::Bytes32) => lhs,
        (Type::Bool, Type::PublicKey) => lhs,
        (Type::Bool, Type::Bytes) => std.never,
        (Type::Bool, Type::Int) => std.never,
        (Type::Bool, Type::Nil) => lhs,

        (Type::Nil, Type::Bytes32) => lhs,
        (Type::Nil, Type::PublicKey) => lhs,
        (Type::Nil, Type::Bytes) => std.never,
        (Type::Nil, Type::Int) => std.never,
        (Type::Nil, Type::Bool) => std.never,

        (Type::Bytes, Type::Pair(..)) => lhs,
        (Type::Bytes32, Type::Pair(..)) => lhs,
        (Type::PublicKey, Type::Pair(..)) => lhs,
        (Type::Int, Type::Pair(..)) => lhs,
        (Type::Bool, Type::Pair(..)) => lhs,
        (Type::Nil, Type::Pair(..)) => lhs,

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

            if matches!(
                (types.get(first), types.get(first)),
                (Type::Never, Type::Never)
            ) {
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
    };

    visited.remove(&(lhs, rhs));

    result
}
