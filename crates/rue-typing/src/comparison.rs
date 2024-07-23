use std::{
    cmp::{max, min},
    collections::{HashMap, HashSet},
};

use num_bigint::BigInt;
use num_traits::One;

use crate::{bigint_to_bytes, Type, TypeId, TypeSystem};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Comparison {
    Equal,
    Assignable,
    Castable,
    Superset,
    Incompatible,
}

pub(crate) struct ComparisonContext<'a> {
    pub visited: HashSet<(TypeId, TypeId)>,
    pub substitution_stack: &'a mut Vec<HashMap<TypeId, TypeId>>,
    pub initial_substitution_length: usize,
    pub generic_stack_frame: Option<usize>,
}

pub(crate) fn compare_type(
    db: &TypeSystem,
    lhs: TypeId,
    rhs: TypeId,
    ctx: &mut ComparisonContext<'_>,
) -> Comparison {
    if lhs == rhs {
        return Comparison::Equal;
    }

    if !ctx.visited.insert((lhs, rhs)) {
        return Comparison::Assignable;
    }

    let comparison = match (db.get(lhs), db.get(rhs)) {
        (Type::Ref(..), _) | (_, Type::Ref(..)) => unreachable!(),

        (_, Type::Generic) => {
            let mut found = None;

            for substititons in ctx.substitution_stack.iter().rev() {
                if let Some(&substititon) = substititons.get(&rhs) {
                    found = Some(substititon);
                }
            }

            if let Some(found) = found {
                compare_type(db, lhs, found, ctx)
            } else if let Some(generic_stack_frame) = ctx.generic_stack_frame {
                ctx.substitution_stack[generic_stack_frame].insert(rhs, lhs);
                Comparison::Assignable
            } else {
                Comparison::Incompatible
            }
        }

        (Type::Generic, _) => {
            let mut found = None;

            for (i, substititons) in ctx.substitution_stack.iter().enumerate().rev() {
                if i < ctx.initial_substitution_length {
                    break;
                }

                if let Some(&substititon) = substititons.get(&lhs) {
                    found = Some(substititon);
                }
            }

            if let Some(found) = found {
                compare_type(db, found, rhs, ctx)
            } else {
                Comparison::Incompatible
            }
        }

        (Type::Unknown, _) | (_, Type::Unknown) => Comparison::Assignable,

        (Type::Never, _) => Comparison::Assignable,
        (_, Type::Never) => Comparison::Superset,

        (Type::Union(items), _) => {
            let items = items.clone();
            let mut result = Comparison::Assignable;
            let mut incompatible_count = 0;

            let length = items.len();

            for item in items {
                let cmp = compare_type(db, item, rhs, ctx);
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
                let cmp = compare_type(db, lhs, item, ctx);
                result = min(result, cmp);
            }

            max(result, Comparison::Assignable)
        }

        (Type::Pair(lhs_first, lhs_rest), Type::Pair(rhs_first, rhs_rest)) => {
            let first = compare_type(db, *lhs_first, *rhs_first, ctx);
            let rest = compare_type(db, *lhs_rest, *rhs_rest, ctx);
            max(first, rest)
        }
        (Type::Pair(..), _) | (_, Type::Pair(..)) => Comparison::Incompatible,

        (Type::True, Type::True) => Comparison::Equal,
        (Type::False, Type::False) => Comparison::Equal,
        (Type::Atom, Type::Atom) => Comparison::Equal,
        (Type::Bytes, Type::Bytes) => Comparison::Equal,
        (Type::Bytes32, Type::Bytes32) => Comparison::Equal,
        (Type::PublicKey, Type::PublicKey) => Comparison::Equal,
        (Type::Int, Type::Int) => Comparison::Equal,
        (Type::Nil, Type::Nil) => Comparison::Equal,

        (Type::Atom, Type::Bytes32) => Comparison::Superset,
        (Type::Atom, Type::PublicKey) => Comparison::Superset,
        (Type::Atom, Type::Nil) => Comparison::Superset,
        (Type::Bytes, Type::Bytes32) => Comparison::Superset,
        (Type::Bytes, Type::PublicKey) => Comparison::Superset,
        (Type::Bytes, Type::Nil) => Comparison::Superset,
        (Type::Int, Type::Bytes32) => Comparison::Superset,
        (Type::Int, Type::PublicKey) => Comparison::Superset,
        (Type::Int, Type::Nil) => Comparison::Superset,

        (Type::Bytes32, Type::Atom) => Comparison::Assignable,
        (Type::PublicKey, Type::Atom) => Comparison::Assignable,
        (Type::Nil, Type::Atom) => Comparison::Assignable,
        (Type::Bytes, Type::Atom) => Comparison::Assignable,
        (Type::Int, Type::Atom) => Comparison::Assignable,

        (Type::Bytes32, Type::Bytes) => Comparison::Assignable,
        (Type::Nil, Type::Bytes) => Comparison::Assignable,

        (Type::Atom, Type::Int) => Comparison::Castable,
        (Type::Atom, Type::Bytes) => Comparison::Castable,
        (Type::Bytes, Type::Int) => Comparison::Castable,
        (Type::Bytes32, Type::Int) => Comparison::Castable,
        (Type::PublicKey, Type::Bytes) => Comparison::Castable,
        (Type::PublicKey, Type::Int) => Comparison::Castable,
        (Type::Int, Type::Bytes) => Comparison::Castable,
        (Type::Nil, Type::Int) => Comparison::Castable,

        (Type::Bytes32, Type::PublicKey) => Comparison::Incompatible,
        (Type::Bytes32, Type::Nil) => Comparison::Incompatible,
        (Type::PublicKey, Type::Bytes32) => Comparison::Incompatible,
        (Type::PublicKey, Type::Nil) => Comparison::Incompatible,
        (Type::Nil, Type::Bytes32) => Comparison::Incompatible,
        (Type::Nil, Type::PublicKey) => Comparison::Incompatible,

        (Type::True, Type::False) => Comparison::Incompatible,
        (Type::False, Type::True) => Comparison::Incompatible,

        (Type::True, Type::Bytes) => Comparison::Castable,
        (Type::False, Type::Bytes) => Comparison::Castable,
        (Type::True, Type::Int) => Comparison::Castable,
        (Type::False, Type::Int) => Comparison::Castable,
        (Type::True, Type::Atom) => Comparison::Assignable,
        (Type::False, Type::Atom) => Comparison::Assignable,
        (Type::True, Type::Nil) => Comparison::Incompatible,
        (Type::False, Type::Nil) => Comparison::Castable,
        (Type::True, Type::Bytes32) => Comparison::Incompatible,
        (Type::False, Type::Bytes32) => Comparison::Incompatible,
        (Type::True, Type::PublicKey) => Comparison::Incompatible,
        (Type::False, Type::PublicKey) => Comparison::Incompatible,

        (Type::Bytes, Type::True) => Comparison::Superset,
        (Type::Bytes, Type::False) => Comparison::Superset,
        (Type::Int, Type::True) => Comparison::Superset,
        (Type::Int, Type::False) => Comparison::Superset,
        (Type::Atom, Type::True) => Comparison::Superset,
        (Type::Atom, Type::False) => Comparison::Superset,
        (Type::Nil, Type::True) => Comparison::Incompatible,
        (Type::Nil, Type::False) => Comparison::Castable,
        (Type::Bytes32, Type::True) => Comparison::Incompatible,
        (Type::Bytes32, Type::False) => Comparison::Incompatible,
        (Type::PublicKey, Type::True) => Comparison::Incompatible,
        (Type::PublicKey, Type::False) => Comparison::Incompatible,

        (Type::Value(..), Type::Atom) => Comparison::Assignable,
        (Type::Value(..), Type::Bytes) => Comparison::Castable,
        (Type::Value(..), Type::Int) => Comparison::Assignable,
        (Type::Value(value), Type::Bytes32) => {
            if bigint_to_bytes(value.clone()).len() == 32 {
                Comparison::Castable
            } else {
                Comparison::Incompatible
            }
        }
        (Type::Value(value), Type::PublicKey) => {
            if bigint_to_bytes(value.clone()).len() == 48 {
                Comparison::Castable
            } else {
                Comparison::Incompatible
            }
        }
        (Type::Value(value), Type::Nil) => {
            if value == &BigInt::ZERO {
                Comparison::Castable
            } else {
                Comparison::Incompatible
            }
        }
        (Type::Value(value), Type::True) => {
            if value == &BigInt::one() {
                Comparison::Castable
            } else {
                Comparison::Incompatible
            }
        }
        (Type::Value(value), Type::False) => {
            if value == &BigInt::ZERO {
                Comparison::Castable
            } else {
                Comparison::Incompatible
            }
        }

        (Type::Atom, Type::Value(..)) => Comparison::Superset,
        (Type::Bytes, Type::Value(..)) => Comparison::Superset,
        (Type::Int, Type::Value(..)) => Comparison::Superset,
        (Type::Bytes32, Type::Value(value)) => {
            if bigint_to_bytes(value.clone()).len() == 32 {
                Comparison::Superset
            } else {
                Comparison::Incompatible
            }
        }
        (Type::PublicKey, Type::Value(value)) => {
            if bigint_to_bytes(value.clone()).len() == 48 {
                Comparison::Superset
            } else {
                Comparison::Incompatible
            }
        }
        (Type::Nil, Type::Value(value)) => {
            if value == &BigInt::ZERO {
                Comparison::Castable
            } else {
                Comparison::Incompatible
            }
        }
        (Type::True, Type::Value(value)) => {
            if value == &BigInt::one() {
                Comparison::Castable
            } else {
                Comparison::Incompatible
            }
        }
        (Type::False, Type::Value(value)) => {
            if value == &BigInt::ZERO {
                Comparison::Castable
            } else {
                Comparison::Incompatible
            }
        }
        (Type::Value(lhs), Type::Value(rhs)) => {
            if lhs == rhs {
                Comparison::Equal
            } else {
                Comparison::Incompatible
            }
        }

        (Type::Lazy(lazy), _) => {
            ctx.substitution_stack.push(lazy.substitutions.clone());
            let result = compare_type(db, lazy.type_id, rhs, ctx);
            ctx.substitution_stack.pop().unwrap();
            result
        }

        (_, Type::Lazy(lazy)) => {
            ctx.substitution_stack.push(lazy.substitutions.clone());
            let result = compare_type(db, lhs, lazy.type_id, ctx);
            ctx.substitution_stack.pop().unwrap();
            result
        }

        (Type::Alias(alias), _) => compare_type(db, alias.type_id, rhs, ctx),
        (_, Type::Alias(alias)) => compare_type(db, lhs, alias.type_id, ctx),

        (Type::Struct(lhs), _) => max(
            compare_type(db, lhs.type_id, rhs, ctx),
            Comparison::Castable,
        ),
        (_, Type::Struct(rhs)) => max(
            compare_type(db, lhs, rhs.type_id, ctx),
            Comparison::Castable,
        ),

        (Type::Variant(variant), Type::Enum(ty)) => {
            let comparison = compare_type(db, variant.type_id, ty.type_id, ctx);

            if variant.enum_type == rhs {
                max(comparison, Comparison::Assignable)
            } else {
                max(comparison, Comparison::Castable)
            }
        }

        (Type::Enum(ty), _) => max(compare_type(db, ty.type_id, rhs, ctx), Comparison::Castable),
        (_, Type::Enum(ty)) => max(compare_type(db, lhs, ty.type_id, ctx), Comparison::Castable),

        (Type::Variant(lhs), _) => max(
            compare_type(db, lhs.type_id, rhs, ctx),
            Comparison::Castable,
        ),
        (_, Type::Variant(rhs)) => max(
            compare_type(db, lhs, rhs.type_id, ctx),
            Comparison::Castable,
        ),

        (Type::Callable(lhs), Type::Callable(rhs)) => max(
            compare_type(db, lhs.parameters, rhs.parameters, ctx),
            compare_type(db, lhs.return_type, rhs.return_type, ctx),
        ),

        (Type::Callable(..), _) => compare_type(db, lhs, db.standard_types().any, ctx),
        (_, Type::Callable(..)) => Comparison::Incompatible,
    };

    ctx.visited.remove(&(lhs, rhs));

    comparison
}

#[cfg(test)]
mod tests {
    use crate::alloc_list;

    use super::*;

    #[test]
    fn test_compare_int_int() {
        let db = TypeSystem::new();
        let types = db.standard_types();
        assert_eq!(db.compare(types.int, types.int), Comparison::Equal);
    }

    #[test]
    fn test_compare_int_bytes() {
        let db = TypeSystem::new();
        let types = db.standard_types();
        assert_eq!(db.compare(types.int, types.bytes), Comparison::Castable);
    }

    #[test]
    fn test_compare_bytes_int() {
        let db = TypeSystem::new();
        let types = db.standard_types();
        assert_eq!(db.compare(types.bytes, types.int), Comparison::Castable);
    }

    #[test]
    fn test_compare_bytes_bytes32() {
        let db = TypeSystem::new();
        let types = db.standard_types();
        assert_eq!(db.compare(types.bytes, types.bytes32), Comparison::Superset);
    }

    #[test]
    fn test_compare_bytes32_bytes() {
        let db = TypeSystem::new();
        let types = db.standard_types();
        assert_eq!(
            db.compare(types.bytes32, types.bytes),
            Comparison::Assignable
        );
    }

    #[test]
    fn test_compare_bytes_public_key() {
        let db = TypeSystem::new();
        let types = db.standard_types();
        assert_eq!(
            db.compare(types.bytes, types.public_key),
            Comparison::Superset
        );
    }

    #[test]
    fn test_compare_public_key_bytes() {
        let db = TypeSystem::new();
        let types = db.standard_types();
        assert_eq!(
            db.compare(types.public_key, types.bytes),
            Comparison::Castable
        );
    }

    #[test]
    fn test_compare_bytes32_public_key() {
        let db = TypeSystem::new();
        let types = db.standard_types();
        assert_eq!(
            db.compare(types.bytes32, types.public_key),
            Comparison::Incompatible
        );
    }

    #[test]
    fn test_compare_public_key_bytes32() {
        let db = TypeSystem::new();
        let types = db.standard_types();
        assert_eq!(
            db.compare(types.public_key, types.bytes32),
            Comparison::Incompatible
        );
    }

    #[test]
    fn test_compare_bytes_any() {
        let db = TypeSystem::new();
        let types = db.standard_types();
        assert_eq!(db.compare(types.bytes, types.any), Comparison::Assignable);
    }

    #[test]
    fn test_compare_any_bytes() {
        let db = TypeSystem::new();
        let types = db.standard_types();
        assert_eq!(db.compare(types.any, types.bytes), Comparison::Superset);
    }

    #[test]
    fn test_compare_bytes32_any() {
        let db = TypeSystem::new();
        let types = db.standard_types();
        assert_eq!(db.compare(types.bytes32, types.any), Comparison::Assignable);
    }

    #[test]
    fn test_compare_any_bytes32() {
        let db = TypeSystem::new();
        let types = db.standard_types();
        assert_eq!(db.compare(types.any, types.bytes32), Comparison::Superset);
    }

    #[test]
    fn test_compare_list_any() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        let list = alloc_list(&mut db, types.int);
        assert_eq!(db.compare(list, types.any), Comparison::Assignable);
    }

    #[test]
    fn test_compare_pair_any() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        let pair = db.alloc(Type::Pair(types.int, types.public_key));
        assert_eq!(db.compare(pair, types.any), Comparison::Assignable);
    }

    #[test]
    fn test_compare_int_any() {
        let db = TypeSystem::new();
        let types = db.standard_types();
        assert_eq!(db.compare(types.int, types.any), Comparison::Assignable);
    }

    #[test]
    fn test_compare_public_key_any() {
        let db = TypeSystem::new();
        let types = db.standard_types();
        assert_eq!(
            db.compare(types.public_key, types.any),
            Comparison::Assignable
        );
    }

    #[test]
    fn test_compare_complex_any() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        let pair_inner_inner = db.alloc(Type::Pair(types.any, types.nil));
        let pair_inner = db.alloc(Type::Pair(pair_inner_inner, pair_inner_inner));
        let pair = db.alloc(Type::Pair(types.int, pair_inner));
        let list = alloc_list(&mut db, pair);
        assert_eq!(db.compare(list, types.any), Comparison::Assignable);
    }

    #[test]
    fn test_compare_any_any() {
        let db = TypeSystem::new();
        let types = db.standard_types();
        assert_eq!(db.compare(types.any, types.any), Comparison::Equal);
    }

    #[test]
    fn test_compare_incompatible_pair() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        let lhs = db.alloc(Type::Pair(types.int, types.public_key));
        let rhs = db.alloc(Type::Pair(types.bytes, types.nil));
        assert_eq!(db.compare(lhs, rhs), Comparison::Incompatible);
    }

    #[test]
    fn test_compare_castable_pair() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        let lhs = db.alloc(Type::Pair(types.int, types.public_key));
        let rhs = db.alloc(Type::Pair(types.bytes, types.bytes));
        assert_eq!(db.compare(lhs, rhs), Comparison::Castable);
    }

    #[test]
    fn test_compare_assignable_pair() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();
        let lhs = db.alloc(Type::Pair(types.int, types.public_key));
        let rhs = db.alloc(Type::Pair(types.atom, types.atom));
        assert_eq!(db.compare(lhs, rhs), Comparison::Assignable);
    }

    #[test]
    fn test_generic_inference() {
        let mut db = TypeSystem::new();
        let types = db.standard_types();

        let generic = db.alloc(Type::Generic);

        let mut stack = vec![HashMap::new()];

        assert_eq!(
            db.compare_with_generics(types.int, generic, &mut stack, true),
            Comparison::Assignable
        );

        assert_eq!(stack.len(), 1);
        assert_eq!(stack[0].get(&generic), Some(&types.int));

        for infer in [true, false] {
            assert_eq!(
                db.compare_with_generics(types.bytes, generic, &mut stack, infer),
                Comparison::Castable
            );
            assert_eq!(
                db.compare_with_generics(types.any, generic, &mut stack, infer),
                Comparison::Superset
            );
        }
    }
}
