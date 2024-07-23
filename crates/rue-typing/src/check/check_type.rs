use std::{
    collections::{HashMap, HashSet, VecDeque},
    hash::BuildHasher,
};

use num_bigint::BigInt;
use num_traits::One;

use crate::{bigint_to_bytes, Comparison, Type, TypeId, TypeSystem};

use super::{Check, CheckError};

/// Returns [`None`] for recursive checks.
pub(crate) fn check_type<S>(
    types: &mut TypeSystem,
    lhs: TypeId,
    rhs: TypeId,
    visited: &mut HashSet<(TypeId, TypeId), S>,
) -> Result<Check, CheckError>
where
    S: BuildHasher,
{
    if !visited.insert((lhs, rhs)) {
        if types.compare(lhs, rhs) <= Comparison::Castable {
            return Ok(Check::None);
        }
        return Err(CheckError::Recursive(lhs, rhs));
    }

    let check = match (types.get(lhs), types.get(rhs)) {
        (Type::Ref(..), _) | (_, Type::Ref(..)) => unreachable!(),
        (Type::Lazy(..), _) | (_, Type::Lazy(..)) => unreachable!(),
        (Type::Alias(..), _) | (_, Type::Alias(..)) => unreachable!(),
        (Type::Struct(..), _) | (_, Type::Struct(..)) => unreachable!(),
        (Type::Callable(..), _) | (_, Type::Callable(..)) => unreachable!(),

        // TODO: Implement generic type checking?
        (Type::Generic, _) => Check::None,
        (_, Type::Generic) => Check::None,

        (Type::Unknown, _) => Check::None,
        (_, Type::Unknown) => Check::None,

        (Type::Never, _) => Check::None,
        (_, Type::Never) => return Err(CheckError::Impossible(lhs, rhs)),

        (Type::Atom, Type::Atom) => Check::None,
        (Type::Bytes, Type::Bytes) => Check::None,
        (Type::Bytes32, Type::Bytes32) => Check::None,
        (Type::PublicKey, Type::PublicKey) => Check::None,
        (Type::Int, Type::Int) => Check::None,
        (Type::Nil, Type::Nil) => Check::None,
        (Type::True, Type::True) => Check::None,
        (Type::False, Type::False) => Check::None,

        (Type::Bytes32, Type::Atom) => Check::None,
        (Type::PublicKey, Type::Atom) => Check::None,
        (Type::Int, Type::Atom) => Check::None,
        (Type::Bytes, Type::Atom) => Check::None,
        (Type::Nil, Type::Atom) => Check::None,

        (Type::Atom, Type::Bytes) => Check::None,
        (Type::Bytes32, Type::Bytes) => Check::None,
        (Type::PublicKey, Type::Bytes) => Check::None,
        (Type::Int, Type::Bytes) => Check::None,
        (Type::Nil, Type::Bytes) => Check::None,

        (Type::Atom, Type::Int) => Check::None,
        (Type::Bytes32, Type::Int) => Check::None,
        (Type::PublicKey, Type::Int) => Check::None,
        (Type::Bytes, Type::Int) => Check::None,
        (Type::Nil, Type::Int) => Check::None,

        (Type::Atom, Type::Nil) => Check::Value(BigInt::ZERO),
        (Type::Atom, Type::PublicKey) => Check::Length(48),
        (Type::Atom, Type::Bytes32) => Check::Length(32),

        (Type::Bytes, Type::Nil) => Check::Value(BigInt::ZERO),
        (Type::Bytes, Type::PublicKey) => Check::Length(48),
        (Type::Bytes, Type::Bytes32) => Check::Length(32),

        (Type::Int, Type::Nil) => Check::Value(BigInt::ZERO),
        (Type::Int, Type::PublicKey) => Check::Length(48),
        (Type::Int, Type::Bytes32) => Check::Length(32),

        (Type::PublicKey, Type::Bytes32) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Bytes32, Type::PublicKey) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Nil, Type::PublicKey) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Nil, Type::Bytes32) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::PublicKey, Type::Nil) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Bytes32, Type::Nil) => return Err(CheckError::Impossible(lhs, rhs)),

        (Type::True, Type::Atom) => Check::None,
        (Type::False, Type::Atom) => Check::None,
        (Type::True, Type::Nil) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::False, Type::Nil) => Check::None,
        (Type::True, Type::False) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::False, Type::True) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::True, Type::Bytes) => Check::None,
        (Type::False, Type::Bytes) => Check::None,
        (Type::True, Type::Bytes32) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::False, Type::Bytes32) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::True, Type::PublicKey) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::False, Type::PublicKey) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::True, Type::Int) => Check::None,
        (Type::False, Type::Int) => Check::None,

        (Type::Atom, Type::True) => Check::Value(BigInt::one()),
        (Type::Atom, Type::False) => Check::Value(BigInt::ZERO),
        (Type::Bytes, Type::True) => Check::Value(BigInt::one()),
        (Type::Bytes, Type::False) => Check::Value(BigInt::ZERO),
        (Type::Int, Type::True) => Check::Value(BigInt::one()),
        (Type::Int, Type::False) => Check::Value(BigInt::ZERO),
        (Type::Bytes32, Type::True) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Bytes32, Type::False) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::PublicKey, Type::True) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::PublicKey, Type::False) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Nil, Type::True) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Nil, Type::False) => Check::None,

        (Type::Value(..), Type::Atom) => Check::None,
        (Type::Value(..), Type::Bytes) => Check::None,
        (Type::Value(..), Type::Int) => Check::None,
        (Type::Value(value), Type::Bytes32) => {
            if bigint_to_bytes(value.clone()).len() == 32 {
                Check::None
            } else {
                return Err(CheckError::Impossible(lhs, rhs));
            }
        }
        (Type::Value(value), Type::PublicKey) => {
            if bigint_to_bytes(value.clone()).len() == 48 {
                Check::None
            } else {
                return Err(CheckError::Impossible(lhs, rhs));
            }
        }
        (Type::Value(value), Type::Nil) => {
            if value == &BigInt::ZERO {
                Check::None
            } else {
                return Err(CheckError::Impossible(lhs, rhs));
            }
        }
        (Type::Value(value), Type::True) => {
            if value == &BigInt::one() {
                Check::None
            } else {
                return Err(CheckError::Impossible(lhs, rhs));
            }
        }
        (Type::Value(value), Type::False) => {
            if value == &BigInt::ZERO {
                Check::None
            } else {
                return Err(CheckError::Impossible(lhs, rhs));
            }
        }

        (Type::Atom, Type::Value(value)) => Check::Value(value.clone()),
        (Type::Bytes, Type::Value(value)) => Check::Value(value.clone()),
        (Type::Int, Type::Value(value)) => Check::Value(value.clone()),
        (Type::Bytes32, Type::Value(value)) => {
            if bigint_to_bytes(value.clone()).len() == 32 {
                Check::Value(value.clone())
            } else {
                return Err(CheckError::Impossible(lhs, rhs));
            }
        }
        (Type::PublicKey, Type::Value(value)) => {
            if bigint_to_bytes(value.clone()).len() == 48 {
                Check::Value(value.clone())
            } else {
                return Err(CheckError::Impossible(lhs, rhs));
            }
        }
        (Type::Nil, Type::Value(value)) => {
            if value == &BigInt::ZERO {
                Check::None
            } else {
                return Err(CheckError::Impossible(lhs, rhs));
            }
        }
        (Type::True, Type::Value(value)) => {
            if value == &BigInt::one() {
                Check::None
            } else {
                return Err(CheckError::Impossible(lhs, rhs));
            }
        }
        (Type::False, Type::Value(value)) => {
            if value == &BigInt::ZERO {
                Check::None
            } else {
                return Err(CheckError::Impossible(lhs, rhs));
            }
        }

        (Type::Value(lhs_value), Type::Value(rhs_value)) => {
            if lhs_value == rhs_value {
                Check::None
            } else {
                return Err(CheckError::Impossible(lhs, rhs));
            }
        }

        (Type::Atom, Type::Pair(..)) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Bytes, Type::Pair(..)) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Bytes32, Type::Pair(..)) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::PublicKey, Type::Pair(..)) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Int, Type::Pair(..)) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Nil, Type::Pair(..)) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::True, Type::Pair(..)) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::False, Type::Pair(..)) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Value(..), Type::Pair(..)) => return Err(CheckError::Impossible(lhs, rhs)),

        (Type::Pair(..), Type::Atom) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Pair(..), Type::Bytes) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Pair(..), Type::Bytes32) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Pair(..), Type::PublicKey) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Pair(..), Type::Int) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Pair(..), Type::Nil) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Pair(..), Type::True) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Pair(..), Type::False) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Pair(..), Type::Value(..)) => return Err(CheckError::Impossible(lhs, rhs)),

        (Type::Pair(lhs_first, lhs_rest), Type::Pair(rhs_first, rhs_rest)) => {
            let (lhs_first, lhs_rest) = (*lhs_first, *lhs_rest);
            let (rhs_first, rhs_rest) = (*rhs_first, *rhs_rest);
            let first = check_type(types, lhs_first, rhs_first, visited)?;
            let rest = check_type(types, lhs_rest, rhs_rest, visited)?;
            Check::And(vec![
                Check::First(Box::new(first)),
                Check::Rest(Box::new(rest)),
            ])
        }

        (_, Type::Union(items)) => {
            let mut result = Vec::new();
            for item in items.clone() {
                result.push(check_type(types, lhs, item, visited)?);
            }
            Check::Or(result)
        }

        (Type::Union(items), _) => {
            let items = items.clone();
            check_union_against_rhs(types, lhs, &items, rhs, visited)?
        }
    };

    visited.remove(&(lhs, rhs));

    Ok(check)
}

fn check_union_against_rhs<S>(
    types: &mut TypeSystem,
    original_type_id: TypeId,
    items: &[TypeId],
    rhs: TypeId,
    visited: &mut HashSet<(TypeId, TypeId), S>,
) -> Result<Check, CheckError>
where
    S: BuildHasher,
{
    let union = types.alloc(Type::Union(items.to_vec()));

    if types.compare(union, rhs) <= Comparison::Castable {
        return Ok(Check::None);
    }

    if let Type::Union(union) = types.get(rhs) {
        let rhs_items = union.clone();
        let mut result = Vec::new();
        for rhs_item in rhs_items {
            if !visited.insert((original_type_id, rhs_item)) {
                return Err(CheckError::Recursive(original_type_id, rhs_item));
            }
            result.push(check_union_against_rhs(
                types,
                original_type_id,
                items,
                rhs_item,
                visited,
            )?);
        }
        return Ok(Check::Or(result));
    }

    let mut atom_count = 0;
    let mut bytes32_count = 0;
    let mut public_key_count = 0;
    let mut pairs = Vec::new();
    let mut values = HashMap::new();

    let mut items: VecDeque<_> = items.iter().copied().collect::<VecDeque<_>>();
    let mut length = 0;

    while !items.is_empty() {
        let item = items.remove(0).unwrap();
        length += 1;

        if !visited.insert((item, rhs)) {
            return Err(CheckError::Recursive(item, rhs));
        }

        match types.get(item) {
            Type::Ref(..) => unreachable!(),
            Type::Lazy(..) => unreachable!(),
            Type::Alias(..) => unreachable!(),
            Type::Struct(..) => unreachable!(),
            Type::Callable(..) => unreachable!(),
            Type::Generic => return Err(CheckError::Impossible(item, rhs)),
            Type::Unknown => {}
            Type::Never => {
                length -= 1;
            }
            Type::Atom | Type::Bytes | Type::Int => {
                atom_count += 1;
            }
            Type::Bytes32 => {
                atom_count += 1;
                bytes32_count += 1;
            }
            Type::PublicKey => {
                atom_count += 1;
                public_key_count += 1;
            }
            Type::Nil => {
                atom_count += 1;
                *values.entry(BigInt::ZERO).or_insert(0) += 1;
            }
            Type::True => {
                atom_count += 1;
                *values.entry(BigInt::one()).or_insert(0) += 1;
            }
            Type::False => {
                atom_count += 1;
                *values.entry(BigInt::ZERO).or_insert(0) += 1;
            }
            Type::Value(value) => {
                atom_count += 1;
                *values.entry(value.clone()).or_insert(0) += 1;
            }
            Type::Pair(first, rest) => {
                pairs.push((*first, *rest));
            }
            Type::Union(child_items) => {
                items.extend(child_items);
            }
        }

        visited.remove(&(item, rhs));
    }

    Ok(match types.get(rhs) {
        Type::Ref(..) => unreachable!(),
        Type::Lazy(..) => unreachable!(),
        Type::Alias(..) => unreachable!(),
        Type::Union(..) => unreachable!(),
        Type::Struct(..) => unreachable!(),
        Type::Callable(..) => unreachable!(),
        Type::Unknown => Check::None,
        Type::Generic => return Err(CheckError::Impossible(original_type_id, rhs)),
        Type::Never => return Err(CheckError::Impossible(original_type_id, rhs)),
        Type::Atom if atom_count == length => Check::None,
        Type::Atom => Check::IsAtom,
        Type::Bytes if atom_count == length => Check::None,
        Type::Bytes => Check::IsAtom,
        Type::Int if atom_count == length => Check::None,
        Type::Int => Check::IsAtom,
        Type::Nil if values.get(&BigInt::ZERO).copied().unwrap_or(0) == length => Check::None,
        Type::Nil if values.get(&BigInt::ZERO).copied().unwrap_or(0) == atom_count => Check::IsAtom,
        Type::Nil if atom_count == length => Check::Value(BigInt::ZERO),
        Type::Nil => Check::And(vec![Check::IsAtom, Check::Value(BigInt::ZERO)]),
        Type::False if values.get(&BigInt::ZERO).copied().unwrap_or(0) == length => Check::None,
        Type::False if values.get(&BigInt::ZERO).copied().unwrap_or(0) == atom_count => {
            Check::IsAtom
        }
        Type::False if atom_count == length => Check::Value(BigInt::ZERO),
        Type::False => Check::And(vec![Check::IsAtom, Check::Value(BigInt::ZERO)]),
        Type::True if values.get(&BigInt::one()).copied().unwrap_or(0) == length => Check::None,
        Type::True if values.get(&BigInt::one()).copied().unwrap_or(0) == atom_count => {
            Check::IsAtom
        }
        Type::True if atom_count == length => Check::Value(BigInt::one()),
        Type::True => Check::And(vec![Check::IsAtom, Check::Value(BigInt::one())]),
        Type::Value(value) if values.get(value).copied().unwrap_or(0) == length => Check::None,
        Type::Value(value) if values.get(value).copied().unwrap_or(0) == atom_count => {
            Check::IsAtom
        }
        Type::Value(value) if atom_count == length => Check::Value(value.clone()),
        Type::Value(value) => Check::And(vec![Check::IsAtom, Check::Value(value.clone())]),
        Type::Bytes32 if bytes32_count == length => Check::None,
        Type::Bytes32 if atom_count == length => Check::Length(32),
        Type::Bytes32 if bytes32_count == atom_count => Check::IsAtom,
        Type::Bytes32 => Check::And(vec![Check::IsAtom, Check::Length(32)]),
        Type::PublicKey if public_key_count == length => Check::None,
        Type::PublicKey if atom_count == length => Check::Length(48),
        Type::PublicKey if public_key_count == atom_count => Check::IsAtom,
        Type::PublicKey => Check::And(vec![Check::IsAtom, Check::Length(48)]),
        Type::Pair(..) if atom_count == length => {
            return Err(CheckError::Impossible(original_type_id, rhs))
        }
        Type::Pair(first, rest) => {
            let (first, rest) = (*first, *rest);

            let first_items: Vec<_> = pairs.iter().map(|(first, _)| *first).collect();
            let rest_items: Vec<_> = pairs.iter().map(|(_, rest)| *rest).collect();

            let first =
                check_union_against_rhs(types, original_type_id, &first_items, first, visited)?;
            let rest =
                check_union_against_rhs(types, original_type_id, &rest_items, rest, visited)?;

            if pairs.len() == length {
                Check::And(vec![
                    Check::First(Box::new(first)),
                    Check::Rest(Box::new(rest)),
                ])
            } else {
                Check::And(vec![
                    Check::IsPair,
                    Check::First(Box::new(first)),
                    Check::Rest(Box::new(rest)),
                ])
            }
        }
    })
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use indexmap::indexmap;

    use crate::{alloc_list, alloc_struct, Rest, Semantics};

    use super::*;

    fn check_str(db: &mut TypeSystem, lhs: TypeId, rhs: TypeId, expected: &str) {
        assert_eq!(format!("{}", db.check(lhs, rhs).unwrap()), expected);
    }

    fn check_recursive(db: &mut TypeSystem, lhs: TypeId, rhs: TypeId) {
        assert!(matches!(db.check(lhs, rhs), Err(CheckError::Recursive(..))));
    }

    fn check_impossible(db: &mut TypeSystem, lhs: TypeId, rhs: TypeId) {
        assert!(matches!(
            db.check(lhs, rhs),
            Err(CheckError::Impossible(..))
        ));
    }

    #[test]
    fn test_check_any_bytes() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        check_str(&mut db, types.any, types.bytes, "(not (l val))");
    }

    #[test]
    fn test_check_any_bytes32() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        check_str(
            &mut db,
            types.any,
            types.bytes32,
            "(and (not (l val)) (= (strlen val) 32))",
        );
    }

    #[test]
    fn test_check_any_public_key() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        check_str(
            &mut db,
            types.any,
            types.public_key,
            "(and (not (l val)) (= (strlen val) 48))",
        );
    }

    #[test]
    fn test_check_any_int() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        check_str(&mut db, types.any, types.int, "(not (l val))");
    }

    #[test]
    fn test_check_any_bool() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        check_str(
            &mut db,
            types.any,
            types.bool,
            "(and (not (l val)) (or (= val 0) (= val 1)))",
        );
    }

    #[test]
    fn test_check_any_nil() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        check_str(
            &mut db,
            types.any,
            types.nil,
            "(and (not (l val)) (= val 0))",
        );
    }

    #[test]
    fn test_check_bytes_bytes() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        check_str(&mut db, types.bytes, types.bytes, "1");
    }

    #[test]
    fn test_check_bytes32_bytes32() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        check_str(&mut db, types.bytes32, types.bytes32, "1");
    }

    #[test]
    fn test_check_public_key_public_key() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        check_str(&mut db, types.public_key, types.public_key, "1");
    }

    #[test]
    fn test_check_int_int() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        check_str(&mut db, types.int, types.int, "1");
    }

    #[test]
    fn test_check_bool_bool() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        check_str(&mut db, types.bool, types.bool, "1");
    }

    #[test]
    fn test_check_nil_nil() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        check_str(&mut db, types.nil, types.nil, "1");
    }

    #[test]
    fn test_check_bytes_bytes32() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        check_str(&mut db, types.bytes, types.bytes32, "(= (strlen val) 32)");
    }

    #[test]
    fn test_check_bytes32_bytes() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        check_str(&mut db, types.bytes32, types.bytes, "1");
    }

    #[test]
    fn test_check_bytes_public_key() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        check_str(
            &mut db,
            types.bytes,
            types.public_key,
            "(= (strlen val) 48)",
        );
    }

    #[test]
    fn test_check_public_key_bytes() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        check_str(&mut db, types.public_key, types.bytes, "1");
    }

    #[test]
    fn test_check_bytes_nil() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        check_str(&mut db, types.bytes, types.nil, "(= val 0)");
    }

    #[test]
    fn test_check_nil_bytes() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        check_str(&mut db, types.nil, types.bytes, "1");
    }

    #[test]
    fn test_check_bytes_bool() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        check_str(&mut db, types.bytes, types.bool, "(or (= val 0) (= val 1))");
    }

    #[test]
    fn test_check_bool_bytes() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        check_str(&mut db, types.bool, types.bytes, "1");
    }

    #[test]
    fn test_check_bytes32_public_key() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        check_impossible(&mut db, types.bytes32, types.public_key);
    }

    #[test]
    fn test_check_bytes_int() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        check_str(&mut db, types.bytes, types.int, "1");
    }

    #[test]
    fn test_check_int_bytes() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        check_str(&mut db, types.int, types.bytes, "1");
    }

    #[test]
    fn test_check_bool_nil() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        check_str(&mut db, types.bool, types.nil, "(= val 0)");
    }

    #[test]
    fn test_check_nil_bool() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        check_str(&mut db, types.nil, types.bool, "1");
    }

    #[test]
    fn test_check_any_any() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        check_str(&mut db, types.any, types.any, "1");
    }

    #[test]
    fn test_check_bytes_any() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        check_str(&mut db, types.any, types.any, "1");
    }

    #[test]
    fn test_check_list_nil() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        let list = alloc_list(&mut db, types.bytes);
        check_str(&mut db, list, types.nil, "(not (l val))");
    }

    #[test]
    fn test_check_list_pair() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        let list = alloc_list(&mut db, types.bytes);
        let pair = db.alloc(Type::Pair(types.bytes, list));
        check_str(&mut db, list, pair, "(l val)");
    }

    #[test]
    fn test_check_any_list() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        let list = alloc_list(&mut db, types.bytes);
        check_recursive(&mut db, types.any, list);
    }

    #[test]
    fn test_check_any_point() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        let point_end = db.alloc(Type::Pair(types.int, types.nil));
        let point = db.alloc(Type::Pair(types.int, point_end));
        check_str(&mut db, types.any, point, "(and (l val) (not (l (f val))) (l (r val)) (not (l (f (r val)))) (not (l (r (r val)))) (= (r (r val)) 0))");
    }

    #[test]
    fn test_check_any_point_struct() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        let point_struct = alloc_struct(
            &mut db,
            &indexmap! {
                "x".to_string() => types.int,
                "y".to_string() => types.int,
            },
            Rest::Nil,
        );
        let point = db.substitute(
            point_struct,
            HashMap::new(),
            Semantics::StructuralOnly {
                callable: types.never,
            },
        );
        check_str(&mut db, types.any, point, "(and (l val) (not (l (f val))) (l (r val)) (not (l (f (r val)))) (not (l (r (r val)))) (= (r (r val)) 0))");
    }
}
