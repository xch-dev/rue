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

    let check = match (types.get(lhs), types.get(rhs)) {
        (Type::Ref(..), _) | (_, Type::Ref(..)) => unreachable!(),
        (Type::Lazy(..), _) | (_, Type::Lazy(..)) => unreachable!(),
        (Type::Alias(..), _) | (_, Type::Alias(..)) => unreachable!(),
        (Type::Struct(..), _) | (_, Type::Struct(..)) => unreachable!(),
        (Type::Callable(..), _) | (_, Type::Callable(..)) => unreachable!(),

        // TODO: Implement generic type checking?
        (Type::Generic, _) => Check::True,
        (_, Type::Generic) => Check::True,

        (Type::Unknown, _) => Check::True,
        (_, Type::Unknown) => Check::True,

        (Type::Never, _) => Check::True,
        (_, Type::Never) => Check::False,

        (Type::Atom, Type::Atom) => Check::True,
        (Type::Bytes, Type::Bytes) => Check::True,
        (Type::Bytes32, Type::Bytes32) => Check::True,
        (Type::PublicKey, Type::PublicKey) => Check::True,
        (Type::Int, Type::Int) => Check::True,
        (Type::Nil, Type::Nil) => Check::True,
        (Type::True, Type::True) => Check::True,
        (Type::False, Type::False) => Check::True,

        (Type::Bytes32, Type::Atom) => Check::True,
        (Type::PublicKey, Type::Atom) => Check::True,
        (Type::Int, Type::Atom) => Check::True,
        (Type::Bytes, Type::Atom) => Check::True,
        (Type::Nil, Type::Atom) => Check::True,

        (Type::Atom, Type::Bytes) => Check::True,
        (Type::Bytes32, Type::Bytes) => Check::True,
        (Type::PublicKey, Type::Bytes) => Check::True,
        (Type::Int, Type::Bytes) => Check::True,
        (Type::Nil, Type::Bytes) => Check::True,

        (Type::Atom, Type::Int) => Check::True,
        (Type::Bytes32, Type::Int) => Check::True,
        (Type::PublicKey, Type::Int) => Check::True,
        (Type::Bytes, Type::Int) => Check::True,
        (Type::Nil, Type::Int) => Check::True,

        (Type::Atom, Type::Nil) => Check::Value(BigInt::ZERO),
        (Type::Atom, Type::PublicKey) => Check::Length(48),
        (Type::Atom, Type::Bytes32) => Check::Length(32),

        (Type::Bytes, Type::Nil) => Check::Value(BigInt::ZERO),
        (Type::Bytes, Type::PublicKey) => Check::Length(48),
        (Type::Bytes, Type::Bytes32) => Check::Length(32),

        (Type::Int, Type::Nil) => Check::Value(BigInt::ZERO),
        (Type::Int, Type::PublicKey) => Check::Length(48),
        (Type::Int, Type::Bytes32) => Check::Length(32),

        (Type::PublicKey, Type::Bytes32) => Check::False,
        (Type::Bytes32, Type::PublicKey) => Check::False,
        (Type::Nil, Type::PublicKey) => Check::False,
        (Type::Nil, Type::Bytes32) => Check::False,
        (Type::PublicKey, Type::Nil) => Check::False,
        (Type::Bytes32, Type::Nil) => Check::False,

        (Type::True, Type::Atom) => Check::True,
        (Type::False, Type::Atom) => Check::True,
        (Type::True, Type::Nil) => Check::False,
        (Type::False, Type::Nil) => Check::True,
        (Type::True, Type::False) => Check::False,
        (Type::False, Type::True) => Check::False,
        (Type::True, Type::Bytes) => Check::True,
        (Type::False, Type::Bytes) => Check::True,
        (Type::True, Type::Bytes32) => Check::False,
        (Type::False, Type::Bytes32) => Check::False,
        (Type::True, Type::PublicKey) => Check::False,
        (Type::False, Type::PublicKey) => Check::False,
        (Type::True, Type::Int) => Check::True,
        (Type::False, Type::Int) => Check::True,

        (Type::Atom, Type::True) => Check::Value(BigInt::one()),
        (Type::Atom, Type::False) => Check::Value(BigInt::ZERO),
        (Type::Bytes, Type::True) => Check::Value(BigInt::one()),
        (Type::Bytes, Type::False) => Check::Value(BigInt::ZERO),
        (Type::Int, Type::True) => Check::Value(BigInt::one()),
        (Type::Int, Type::False) => Check::Value(BigInt::ZERO),
        (Type::Bytes32, Type::True) => Check::False,
        (Type::Bytes32, Type::False) => Check::False,
        (Type::PublicKey, Type::True) => Check::False,
        (Type::PublicKey, Type::False) => Check::False,
        (Type::Nil, Type::True) => Check::False,
        (Type::Nil, Type::False) => Check::True,

        (Type::Value(..), Type::Atom) => Check::True,
        (Type::Value(..), Type::Bytes) => Check::True,
        (Type::Value(..), Type::Int) => Check::True,
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
        (Type::Value(value), Type::Nil) => {
            if value == &BigInt::ZERO {
                Check::True
            } else {
                Check::False
            }
        }
        (Type::Value(value), Type::True) => {
            if value == &BigInt::one() {
                Check::True
            } else {
                Check::False
            }
        }
        (Type::Value(value), Type::False) => {
            if value == &BigInt::ZERO {
                Check::True
            } else {
                Check::False
            }
        }

        (Type::Atom, Type::Value(value)) => Check::Value(value.clone()),
        (Type::Bytes, Type::Value(value)) => Check::Value(value.clone()),
        (Type::Int, Type::Value(value)) => Check::Value(value.clone()),
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
        (Type::Nil, Type::Value(value)) => {
            if value == &BigInt::ZERO {
                Check::True
            } else {
                Check::False
            }
        }
        (Type::True, Type::Value(value)) => {
            if value == &BigInt::one() {
                Check::True
            } else {
                Check::False
            }
        }
        (Type::False, Type::Value(value)) => {
            if value == &BigInt::ZERO {
                Check::True
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

        (Type::Atom, Type::Pair(..)) => Check::False,
        (Type::Bytes, Type::Pair(..)) => Check::False,
        (Type::Bytes32, Type::Pair(..)) => Check::False,
        (Type::PublicKey, Type::Pair(..)) => Check::False,
        (Type::Int, Type::Pair(..)) => Check::False,
        (Type::Nil, Type::Pair(..)) => Check::False,
        (Type::True, Type::Pair(..)) => Check::False,
        (Type::False, Type::Pair(..)) => Check::False,
        (Type::Value(..), Type::Pair(..)) => Check::False,

        (Type::Pair(..), Type::Atom) => Check::False,
        (Type::Pair(..), Type::Bytes) => Check::False,
        (Type::Pair(..), Type::Bytes32) => Check::False,
        (Type::Pair(..), Type::PublicKey) => Check::False,
        (Type::Pair(..), Type::Int) => Check::False,
        (Type::Pair(..), Type::Nil) => Check::False,
        (Type::Pair(..), Type::True) => Check::False,
        (Type::Pair(..), Type::False) => Check::False,
        (Type::Pair(..), Type::Value(..)) => Check::False,

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

    Ok(match types.get(rhs) {
        Type::Ref(..) => unreachable!(),
        Type::Lazy(..) => unreachable!(),
        Type::Alias(..) => unreachable!(),
        Type::Union(..) => unreachable!(),
        Type::Struct(..) => unreachable!(),
        Type::Callable(..) => unreachable!(),
        Type::Unknown => Check::True,
        Type::Generic => Check::False,
        Type::Never => Check::False,
        Type::Atom if attrs.all_atoms() => Check::True,
        Type::Atom => Check::IsAtom,
        Type::Bytes if attrs.all_atoms() => Check::True,
        Type::Bytes => Check::IsAtom,
        Type::Int if attrs.all_atoms() => Check::True,
        Type::Int => Check::IsAtom,
        Type::Nil if attrs.all_value(&BigInt::ZERO) => Check::True,
        Type::Nil if attrs.atoms_are_value(&BigInt::ZERO) => Check::IsAtom,
        Type::Nil if attrs.all_atoms() => Check::Value(BigInt::ZERO),
        Type::Nil => Check::And(vec![Check::IsAtom, Check::Value(BigInt::ZERO)]),
        Type::False if attrs.all_value(&BigInt::ZERO) => Check::True,
        Type::False if attrs.atoms_are_value(&BigInt::ZERO) => Check::IsAtom,
        Type::False if attrs.all_atoms() => Check::Value(BigInt::ZERO),
        Type::False => Check::And(vec![Check::IsAtom, Check::Value(BigInt::ZERO)]),
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
        check_str(&mut db, types.bytes32, types.public_key, "0");
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
