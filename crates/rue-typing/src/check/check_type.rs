use std::collections::HashSet;

use num_bigint::BigInt;
use num_traits::One;

use crate::{bigint_to_bytes, Comparison, Type, TypeId, TypeSystem};

use super::{union_attributes, Check, CheckError};

/// Returns [`None`] for recursive checks.
pub(crate) fn check_type(
    types: &mut TypeSystem,
    lhs: TypeId,
    rhs: TypeId,
    visited: &mut HashSet<(TypeId, TypeId)>,
) -> Result<Check, CheckError> {
    if !visited.insert((lhs, rhs)) {
        if types.compare(lhs, rhs) <= Comparison::Castable {
            return Ok(Check::True);
        }
        return Err(CheckError::Recursive(lhs, rhs));
    }

    let check = match (types.get_recursive(lhs), types.get_recursive(rhs)) {
        (
            Type::Ref(..)
            | Type::Lazy(..)
            | Type::Alias(..)
            | Type::Struct(..)
            | Type::Enum(..)
            | Type::Variant(..),
            _,
        )
        | (
            _,
            Type::Ref(..)
            | Type::Lazy(..)
            | Type::Alias(..)
            | Type::Struct(..)
            | Type::Enum(..)
            | Type::Variant(..),
        ) => {
            unreachable!()
        }

        (_, Type::Any | Type::Unknown)
        | (Type::Never | Type::Unknown, _)
        | (Type::Bytes | Type::Int | Type::Value(..), Type::Bytes | Type::Int)
        | (Type::Bytes32, Type::Bytes | Type::Int | Type::Bytes32)
        | (Type::PublicKey, Type::Bytes | Type::Int | Type::PublicKey)
        | (Type::Nil | Type::False, Type::Bytes | Type::Int | Type::Nil | Type::False)
        | (Type::True, Type::Bytes | Type::Int | Type::True) => Check::True,

        (Type::Any | Type::Generic | Type::Callable(..), Type::Bytes | Type::Int) => Check::IsAtom,
        (Type::Any | Type::Generic | Type::Callable(..), Type::Bytes32) => {
            Check::And(vec![Check::IsAtom, Check::Length(32)])
        }
        (Type::Any | Type::Generic | Type::Callable(..), Type::PublicKey) => {
            Check::And(vec![Check::IsAtom, Check::Length(48)])
        }
        (Type::Any | Type::Generic | Type::Callable(..), Type::False | Type::Nil) => {
            Check::And(vec![Check::IsAtom, Check::Value(BigInt::ZERO)])
        }
        (Type::Any | Type::Generic | Type::Callable(..), Type::True) => {
            Check::And(vec![Check::IsAtom, Check::Value(BigInt::one())])
        }
        (Type::Any | Type::Generic | Type::Callable(..), Type::Value(value)) => {
            Check::And(vec![Check::IsAtom, Check::Value(value.clone())])
        }
        (Type::Any | Type::Generic | Type::Callable(..), Type::Pair(first, rest)) => {
            let (first, rest) = (*first, *rest);
            let first = check_type(types, types.std().any, first, visited)?;
            let rest = check_type(types, types.std().any, rest, visited)?;
            Check::And(vec![
                Check::IsPair,
                Check::First(Box::new(first)),
                Check::Rest(Box::new(rest)),
            ])
        }

        (Type::Bytes | Type::Int, Type::Nil | Type::False) => Check::Value(BigInt::ZERO),
        (Type::Bytes | Type::Int, Type::True) => Check::Value(BigInt::one()),
        (Type::Bytes | Type::Int, Type::PublicKey) => Check::Length(48),
        (Type::Bytes | Type::Int, Type::Bytes32) => Check::Length(32),

        (_, Type::Never | Type::Generic | Type::Callable(..))
        | (Type::PublicKey, Type::Bytes32 | Type::Nil | Type::True | Type::False)
        | (Type::Bytes32, Type::PublicKey | Type::Nil | Type::True | Type::False)
        | (Type::Nil | Type::False, Type::PublicKey | Type::Bytes32 | Type::True)
        | (Type::True, Type::PublicKey | Type::Bytes32 | Type::Nil | Type::False)
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
        )
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
        ) => Check::False,

        (Type::Value(value), Type::Bytes32) => {
            if bigint_to_bytes(value.clone()).len() == 32 {
                Check::True
            } else {
                Check::False
            }
        }

        (Type::Value(value), Type::PublicKey) => {
            if bigint_to_bytes(value.clone()).len() == 48 {
                Check::True
            } else {
                Check::False
            }
        }

        (Type::Value(value), Type::Nil | Type::False)
        | (Type::Nil | Type::False, Type::Value(value)) => {
            if value == &BigInt::ZERO {
                Check::True
            } else {
                Check::False
            }
        }

        (Type::Value(value), Type::True) | (Type::True, Type::Value(value)) => {
            if value == &BigInt::one() {
                Check::True
            } else {
                Check::False
            }
        }

        (Type::Bytes | Type::Int, Type::Value(value)) => Check::Value(value.clone()),

        (Type::Bytes32, Type::Value(value)) => {
            if bigint_to_bytes(value.clone()).len() == 32 {
                Check::Value(value.clone())
            } else {
                Check::False
            }
        }
        (Type::PublicKey, Type::Value(value)) => {
            if bigint_to_bytes(value.clone()).len() == 48 {
                Check::Value(value.clone())
            } else {
                Check::False
            }
        }

        (Type::Value(lhs_value), Type::Value(rhs_value)) => {
            if lhs_value == rhs_value {
                Check::True
            } else {
                Check::False
            }
        }

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

        (Type::Union(items), _) => {
            let items = items.clone();
            check_union_against_rhs(types, lhs, &items, rhs, visited)?
        }

        (_, Type::Union(items)) => {
            let mut result = Vec::new();
            for item in items.clone() {
                result.push(check_type(types, lhs, item, visited)?);
            }
            Check::Or(result)
        }
    };

    visited.remove(&(lhs, rhs));

    Ok(check)
}

fn check_union_against_rhs(
    types: &mut TypeSystem,
    original_type_id: TypeId,
    items: &[TypeId],
    rhs: TypeId,
    visited: &mut HashSet<(TypeId, TypeId)>,
) -> Result<Check, CheckError> {
    let union = types.alloc(Type::Union(items.to_vec()));

    if types.compare(union, rhs) <= Comparison::Castable {
        return Ok(Check::True);
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

    let attrs = union_attributes(types, items, true, rhs, visited)?;

    Ok(match types.get_recursive(rhs) {
        Type::Ref(..)
        | Type::Lazy(..)
        | Type::Union(..)
        | Type::Alias(..)
        | Type::Struct(..)
        | Type::Enum(..)
        | Type::Variant(..) => unreachable!(),
        Type::Unknown | Type::Any => Check::True,
        Type::Never | Type::Generic | Type::Callable(..) => Check::False,
        Type::Bytes | Type::Int if attrs.all_atoms() => Check::True,
        Type::Bytes | Type::Int => Check::IsAtom,
        Type::Nil | Type::False if attrs.all_value(&BigInt::ZERO) => Check::True,
        Type::Nil | Type::False if attrs.atoms_are_value(&BigInt::ZERO) => Check::IsAtom,
        Type::Nil | Type::False if attrs.all_atoms() => Check::Value(BigInt::ZERO),
        Type::Nil | Type::False => Check::And(vec![Check::IsAtom, Check::Value(BigInt::ZERO)]),
        Type::True if attrs.all_value(&BigInt::one()) => Check::True,
        Type::True if attrs.atoms_are_value(&BigInt::ZERO) => Check::IsAtom,
        Type::True if attrs.all_atoms() => Check::Value(BigInt::one()),
        Type::True => Check::And(vec![Check::IsAtom, Check::Value(BigInt::one())]),
        Type::Value(value) if attrs.all_value(value) => Check::True,
        Type::Value(value) if attrs.atoms_are_value(value) => Check::IsAtom,
        Type::Value(value) if attrs.all_atoms() => Check::Value(value.clone()),
        Type::Value(value) => Check::And(vec![Check::IsAtom, Check::Value(value.clone())]),
        Type::Bytes32 if attrs.all_bytes32() => Check::True,
        Type::Bytes32 if attrs.all_atoms() => Check::Length(32),
        Type::Bytes32 if attrs.atoms_are_bytes32() => Check::IsAtom,
        Type::Bytes32 => Check::And(vec![Check::IsAtom, Check::Length(32)]),
        Type::PublicKey if attrs.all_public_key() => Check::True,
        Type::PublicKey if attrs.all_atoms() => Check::Length(48),
        Type::PublicKey if attrs.atoms_are_public_key() => Check::IsAtom,
        Type::PublicKey => Check::And(vec![Check::IsAtom, Check::Length(48)]),
        Type::Pair(..) if attrs.all_atoms() => Check::False,
        Type::Pair(first, rest) => {
            let (first, rest) = (*first, *rest);

            let first_items: Vec<_> = attrs.pairs.iter().map(|(first, _)| *first).collect();
            let rest_items: Vec<_> = attrs.pairs.iter().map(|(_, rest)| *rest).collect();

            let first =
                check_union_against_rhs(types, original_type_id, &first_items, first, visited)?;
            let rest =
                check_union_against_rhs(types, original_type_id, &rest_items, rest, visited)?;

            if attrs.all_pairs() {
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
    use indexmap::indexmap;

    use crate::{alloc_list, alloc_struct, Rest};

    use super::*;

    fn check_str(db: &mut TypeSystem, lhs: TypeId, rhs: TypeId, expected: &str) {
        assert_eq!(format!("{}", db.check(lhs, rhs).unwrap()), expected);
    }

    fn check_recursive(db: &mut TypeSystem, lhs: TypeId, rhs: TypeId) {
        assert!(matches!(db.check(lhs, rhs), Err(CheckError::Recursive(..))));
    }

    #[test]
    fn test_check_any_bytes() {
        let mut db = TypeSystem::new();
        let types = db.std();
        check_str(&mut db, types.any, types.bytes, "(not (l val))");
    }

    #[test]
    fn test_check_any_bytes32() {
        let mut db = TypeSystem::new();
        let types = db.std();
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
        let types = db.std();
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
        let types = db.std();
        check_str(&mut db, types.any, types.int, "(not (l val))");
    }

    #[test]
    fn test_check_any_bool() {
        let mut db = TypeSystem::new();
        let types = db.std();
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
        let types = db.std();
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
        let types = db.std();
        check_str(&mut db, types.bytes, types.bytes, "1");
    }

    #[test]
    fn test_check_bytes32_bytes32() {
        let mut db = TypeSystem::new();
        let types = db.std();
        check_str(&mut db, types.bytes32, types.bytes32, "1");
    }

    #[test]
    fn test_check_public_key_public_key() {
        let mut db = TypeSystem::new();
        let types = db.std();
        check_str(&mut db, types.public_key, types.public_key, "1");
    }

    #[test]
    fn test_check_int_int() {
        let mut db = TypeSystem::new();
        let types = db.std();
        check_str(&mut db, types.int, types.int, "1");
    }

    #[test]
    fn test_check_bool_bool() {
        let mut db = TypeSystem::new();
        let types = db.std();
        check_str(&mut db, types.bool, types.bool, "1");
    }

    #[test]
    fn test_check_nil_nil() {
        let mut db = TypeSystem::new();
        let types = db.std();
        check_str(&mut db, types.nil, types.nil, "1");
    }

    #[test]
    fn test_check_bytes_bytes32() {
        let mut db = TypeSystem::new();
        let types = db.std();
        check_str(&mut db, types.bytes, types.bytes32, "(= (strlen val) 32)");
    }

    #[test]
    fn test_check_bytes32_bytes() {
        let mut db = TypeSystem::new();
        let types = db.std();
        check_str(&mut db, types.bytes32, types.bytes, "1");
    }

    #[test]
    fn test_check_bytes_public_key() {
        let mut db = TypeSystem::new();
        let types = db.std();
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
        let types = db.std();
        check_str(&mut db, types.public_key, types.bytes, "1");
    }

    #[test]
    fn test_check_bytes_nil() {
        let mut db = TypeSystem::new();
        let types = db.std();
        check_str(&mut db, types.bytes, types.nil, "(= val 0)");
    }

    #[test]
    fn test_check_nil_bytes() {
        let mut db = TypeSystem::new();
        let types = db.std();
        check_str(&mut db, types.nil, types.bytes, "1");
    }

    #[test]
    fn test_check_bytes_bool() {
        let mut db = TypeSystem::new();
        let types = db.std();
        check_str(&mut db, types.bytes, types.bool, "(or (= val 0) (= val 1))");
    }

    #[test]
    fn test_check_bool_bytes() {
        let mut db = TypeSystem::new();
        let types = db.std();
        check_str(&mut db, types.bool, types.bytes, "1");
    }

    #[test]
    fn test_check_bytes32_public_key() {
        let mut db = TypeSystem::new();
        let types = db.std();
        check_str(&mut db, types.bytes32, types.public_key, "0");
    }

    #[test]
    fn test_check_bytes_int() {
        let mut db = TypeSystem::new();
        let types = db.std();
        check_str(&mut db, types.bytes, types.int, "1");
    }

    #[test]
    fn test_check_int_bytes() {
        let mut db = TypeSystem::new();
        let types = db.std();
        check_str(&mut db, types.int, types.bytes, "1");
    }

    #[test]
    fn test_check_bool_nil() {
        let mut db = TypeSystem::new();
        let types = db.std();
        check_str(&mut db, types.bool, types.nil, "(= val 0)");
    }

    #[test]
    fn test_check_nil_bool() {
        let mut db = TypeSystem::new();
        let types = db.std();
        check_str(&mut db, types.nil, types.bool, "1");
    }

    #[test]
    fn test_check_any_any() {
        let mut db = TypeSystem::new();
        let types = db.std();
        check_str(&mut db, types.any, types.any, "1");
    }

    #[test]
    fn test_check_bytes_any() {
        let mut db = TypeSystem::new();
        let types = db.std();
        check_str(&mut db, types.any, types.any, "1");
    }

    #[test]
    fn test_check_list_nil() {
        let mut db = TypeSystem::new();
        let types = db.std();
        let list = alloc_list(&mut db, types.bytes);
        check_str(&mut db, list, types.nil, "(not (l val))");
    }

    #[test]
    fn test_check_list_pair() {
        let mut db = TypeSystem::new();
        let types = db.std();
        let list = alloc_list(&mut db, types.bytes);
        let pair = db.alloc(Type::Pair(types.bytes, list));
        check_str(&mut db, list, pair, "(l val)");
    }

    #[test]
    fn test_check_any_list() {
        let mut db = TypeSystem::new();
        let types = db.std();
        let list = alloc_list(&mut db, types.bytes);
        check_recursive(&mut db, types.any, list);
    }

    #[test]
    fn test_check_any_point() {
        let mut db = TypeSystem::new();
        let types = db.std();
        let point_end = db.alloc(Type::Pair(types.int, types.nil));
        let point = db.alloc(Type::Pair(types.int, point_end));
        check_str(&mut db, types.any, point, "(and (l val) (not (l (f val))) (l (r val)) (not (l (f (r val)))) (not (l (r (r val)))) (= (r (r val)) 0))");
    }

    #[test]
    fn test_check_any_point_struct() {
        let mut db = TypeSystem::new();
        let types = db.std();
        let point = alloc_struct(
            &mut db,
            &indexmap! {
                "x".to_string() => types.int,
                "y".to_string() => types.int,
            },
            Rest::Nil,
        );
        check_str(&mut db, types.any, point, "(and (l val) (not (l (f val))) (l (r val)) (not (l (f (r val)))) (not (l (r (r val)))) (= (r (r val)) 0))");
    }

    #[test]
    fn test_check_condition_agg_sig_me() {
        let mut db = TypeSystem::new();
        let types = db.std();

        let opcode = db.alloc(Type::Value(BigInt::from(49)));
        let agg_sig_unsafe = alloc_struct(
            &mut db,
            &indexmap! {
                "opcode".to_string() => opcode,
                "public_key".to_string() => types.public_key,
                "message".to_string() => types.bytes,
            },
            Rest::Nil,
        );

        let opcode = db.alloc(Type::Value(BigInt::from(50)));
        let agg_sig_me = alloc_struct(
            &mut db,
            &indexmap! {
                "opcode".to_string() => opcode,
                "public_key".to_string() => types.public_key,
                "message".to_string() => types.bytes,
            },
            Rest::Nil,
        );

        let condition = db.alloc(Type::Union(vec![agg_sig_unsafe, agg_sig_me]));

        check_str(&mut db, condition, agg_sig_me, "(= (f val) 50)");
        check_str(&mut db, types.any, agg_sig_me, "(and (l val) (not (l (f val))) (= (f val) 50) (l (r val)) (not (l (f (r val)))) (= (strlen (f (r val))) 48) (l (r (r val))) (not (l (f (r (r val))))) (not (l (r (r (r val))))) (= (r (r (r val))) 0))");
    }

    #[test]
    fn test_check_three_int_int_list() {
        let mut db = TypeSystem::new();
        let types = db.std();
        let inner = db.alloc(Type::Pair(types.int, types.int));
        let pair = db.alloc(Type::Pair(types.int, inner));
        let list = alloc_list(&mut db, types.int);
        let union = db.alloc(Type::Union(vec![pair, list]));

        check_str(&mut db, union, union, "1");
        check_str(&mut db, union, types.int, "(not (l val))");
        check_str(&mut db, union, types.nil, "(not (l val))");
        check_recursive(&mut db, union, list);
        check_str(
            &mut db,
            union,
            pair,
            "(and (l val) (l (r val)) (not (l (r (r val)))))",
        );
    }
}
