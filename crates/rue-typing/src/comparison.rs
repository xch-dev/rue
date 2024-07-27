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

        // These types are identical.
        (Type::Unknown, Type::Unknown)
        | (Type::Never, Type::Never)
        | (Type::Any, Type::Any)
        | (Type::Bytes, Type::Bytes)
        | (Type::Bytes32, Type::Bytes32)
        | (Type::PublicKey, Type::PublicKey)
        | (Type::Int, Type::Int)
        | (Type::Nil, Type::Nil)
        | (Type::True, Type::True)
        | (Type::False, Type::False) => Comparison::Equal,

        // These are assignable since the structure and semantics match.
        (_, Type::Any | Type::Unknown)
        | (
            Type::Unknown | Type::Never,
            Type::Bytes
            | Type::Bytes32
            | Type::PublicKey
            | Type::Int
            | Type::Nil
            | Type::True
            | Type::False
            | Type::Value(..)
            | Type::Pair(..)
            | Type::Callable(..),
        )
        | (Type::Unknown, Type::Never)
        | (Type::Value(..), Type::Int)
        | (Type::Bytes32 | Type::Nil, Type::Bytes) => Comparison::Assignable,

        // These are castable since the structure matches but the semantics differ.
        (
            Type::Bytes | Type::Bytes32 | Type::PublicKey | Type::Nil | Type::True | Type::False,
            Type::Int,
        )
        | (Type::PublicKey | Type::Int | Type::True | Type::False | Type::Value(..), Type::Bytes)
        | (Type::False, Type::Nil)
        | (Type::Nil, Type::False) => Comparison::Castable,

        // These are a superset since the right hand side is castable to the left hand side.
        (
            Type::Any,
            Type::Bytes
            | Type::Bytes32
            | Type::PublicKey
            | Type::Int
            | Type::Nil
            | Type::True
            | Type::False
            | Type::Value(..)
            | Type::Pair(..),
        )
        | (
            Type::Bytes | Type::Int,
            Type::Bytes32
            | Type::PublicKey
            | Type::Nil
            | Type::True
            | Type::False
            | Type::Value(..),
        )
        | (
            Type::Any
            | Type::Bytes
            | Type::Bytes32
            | Type::PublicKey
            | Type::Int
            | Type::Nil
            | Type::True
            | Type::False
            | Type::Value(..)
            | Type::Pair(..)
            | Type::Callable(..),
            Type::Never,
        ) => Comparison::Superset,

        // These are incompatible since the structure differs.
        (
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
        )
        | (Type::Bytes32, Type::PublicKey | Type::Nil | Type::True | Type::False)
        | (Type::PublicKey, Type::Bytes32 | Type::Nil | Type::True | Type::False)
        | (Type::Nil, Type::Bytes32 | Type::PublicKey | Type::True)
        | (Type::True, Type::False | Type::Nil)
        | (Type::False, Type::True)
        | (Type::True | Type::False, Type::Bytes32 | Type::PublicKey)
        | (
            Type::Any
            | Type::Bytes
            | Type::Bytes32
            | Type::PublicKey
            | Type::Int
            | Type::Nil
            | Type::True
            | Type::False
            | Type::Value(..)
            | Type::Pair(..),
            Type::Callable(..),
        ) => Comparison::Incompatible,

        // Value is a subtype of Int, so it's castable to Bytes32 if it's 32 bytes long.
        (Type::Value(value), Type::Bytes32) => {
            if bigint_to_bytes(value.clone()).len() == 32 {
                Comparison::Castable
            } else {
                Comparison::Incompatible
            }
        }

        // Value is a subtype of Int, so it's castable to PublicKey if it's 48 bytes long.
        (Type::Value(value), Type::PublicKey) => {
            if bigint_to_bytes(value.clone()).len() == 48 {
                Comparison::Castable
            } else {
                Comparison::Incompatible
            }
        }

        // Bytes32 is a superset of Value only if the value is 32 bytes long.
        (Type::Bytes32, Type::Value(value)) => {
            if bigint_to_bytes(value.clone()).len() == 32 {
                Comparison::Superset
            } else {
                Comparison::Incompatible
            }
        }

        // PublicKey is a superset of Value only if the value is 48 bytes long.
        (Type::PublicKey, Type::Value(value)) => {
            if bigint_to_bytes(value.clone()).len() == 48 {
                Comparison::Superset
            } else {
                Comparison::Incompatible
            }
        }

        // Nil and False are supersets of Value only if the value is zero.
        (Type::Nil | Type::False, Type::Value(value))
        | (Type::Value(value), Type::Nil | Type::False) => {
            if value == &BigInt::ZERO {
                Comparison::Castable
            } else {
                Comparison::Incompatible
            }
        }

        // True is a superset of Value only if the value is one.
        (Type::True, Type::Value(value)) | (Type::Value(value), Type::True) => {
            if value == &BigInt::one() {
                Comparison::Castable
            } else {
                Comparison::Incompatible
            }
        }

        // Value is equal to other instances of Value only if the values are equal.
        (Type::Value(lhs), Type::Value(rhs)) => {
            if lhs == rhs {
                Comparison::Equal
            } else {
                Comparison::Incompatible
            }
        }

        // A comparison of pairs is done by using whichever comparison is the most restrictive.
        (Type::Pair(lhs_first, lhs_rest), Type::Pair(rhs_first, rhs_rest)) => {
            let first = compare_type(db, *lhs_first, *rhs_first, ctx);
            let rest = compare_type(db, *lhs_rest, *rhs_rest, ctx);
            max(first, rest)
        }

        // Unions can be assigned to anything so long as each of the items in the union are also.
        (Type::Union(items), _) => {
            let items = items.clone();
            let mut result = Comparison::Assignable;

            let mut any_castable = false;

            for item in items {
                let cmp = compare_type(db, item, rhs, ctx);
                result = max(result, cmp);

                if compare_type(db, rhs, item, ctx) <= Comparison::Castable {
                    any_castable = true;
                }
            }

            if result == Comparison::Incompatible && any_castable {
                Comparison::Superset
            } else {
                result
            }
        }

        // Anything can be assigned to a union so long as it's assignable to at least one of the items.
        (_, Type::Union(items)) => {
            let items = items.clone();
            let mut result = Comparison::Incompatible;
            let mut any_incompatible = false;

            for item in &items {
                if matches!(db.get_recursive(*item), Type::Never) {
                    continue;
                }

                let cmp = compare_type(db, lhs, *item, ctx);
                result = min(result, cmp);

                if cmp == Comparison::Incompatible {
                    any_incompatible = true;
                }
            }

            if any_incompatible && result == Comparison::Superset {
                Comparison::Incompatible
            } else {
                max(result, Comparison::Assignable)
            }
        }

        // We need to push substititons onto the stack in order to accurately compare them.
        (Type::Lazy(lazy), _) => {
            ctx.substitution_stack
                .push(lazy.substitutions.clone().into_iter().collect());
            let result = compare_type(db, lazy.type_id, rhs, ctx);
            ctx.substitution_stack.pop().unwrap();
            result
        }
        (_, Type::Lazy(lazy)) => {
            ctx.substitution_stack
                .push(lazy.substitutions.clone().into_iter().collect());
            let result = compare_type(db, lhs, lazy.type_id, ctx);
            ctx.substitution_stack.pop().unwrap();
            result
        }

        // Resolve the alias to the type that it's pointing to.
        (Type::Alias(alias), _) => compare_type(db, alias.type_id, rhs, ctx),
        (_, Type::Alias(alias)) => compare_type(db, lhs, alias.type_id, ctx),

        // Structs are at best castable to other types, since they have different semantics.
        (Type::Struct(lhs), Type::Struct(rhs)) if lhs.original_type_id == rhs.original_type_id => {
            compare_type(db, lhs.type_id, rhs.type_id, ctx)
        }
        (Type::Struct(lhs), _) => max(
            compare_type(db, lhs.type_id, rhs, ctx),
            Comparison::Castable,
        ),
        (_, Type::Struct(rhs)) => max(
            compare_type(db, lhs, rhs.type_id, ctx),
            Comparison::Castable,
        ),

        // Variants can be assigned to enums if the structure is assignable and it's the same enum.
        (Type::Variant(variant), Type::Enum(ty)) => {
            let comparison = compare_type(db, lhs, ty.type_id, ctx);

            if variant.original_enum_type_id == ty.original_type_id {
                max(comparison, Comparison::Assignable)
            } else {
                max(comparison, Comparison::Castable)
            }
        }

        (Type::Enum(ty), Type::Variant(variant)) => {
            let comparison = compare_type(db, ty.type_id, rhs, ctx);

            if variant.original_enum_type_id == ty.original_type_id {
                max(comparison, Comparison::Assignable)
            } else {
                max(comparison, Comparison::Castable)
            }
        }

        // Enums can be assigned if the structure is assignable and it's the same enum.
        (Type::Enum(lhs), Type::Enum(rhs)) if lhs.original_type_id == rhs.original_type_id => {
            compare_type(db, lhs.type_id, rhs.type_id, ctx)
        }
        (Type::Enum(ty), _) => max(compare_type(db, ty.type_id, rhs, ctx), Comparison::Castable),
        (_, Type::Enum(ty)) => max(compare_type(db, lhs, ty.type_id, ctx), Comparison::Castable),

        // Variants can be assigned if the structure is assignable and it's the same variant.
        (Type::Variant(lhs), Type::Variant(rhs))
            if lhs.original_type_id == rhs.original_type_id =>
        {
            compare_type(db, lhs.type_id, rhs.type_id, ctx)
        }
        (Type::Variant(lhs), _) => max(
            compare_type(db, lhs.type_id, rhs, ctx),
            Comparison::Castable,
        ),
        (_, Type::Variant(rhs)) => max(
            compare_type(db, lhs, rhs.type_id, ctx),
            Comparison::Castable,
        ),

        // Functions can be assigned to other functions if the parameters and return type are assignable.
        // They're treated like Never on the right hand side and Any on the left hand side.
        (Type::Callable(lhs), Type::Callable(rhs)) => max(
            compare_type(db, lhs.parameters, rhs.parameters, ctx),
            compare_type(db, lhs.return_type, rhs.return_type, ctx),
        ),
        (Type::Callable(..), _) => compare_type(db, lhs, db.std().any, ctx),

        // Generics are resolved by looking up the substitution in the stack.
        // If we're infering, we'll push the substitution onto the proper generic stack frame.
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

        // Generics are resolved by looking up the substitution in the stack.
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
    };

    ctx.visited.remove(&(lhs, rhs));

    comparison
}

#[cfg(test)]
mod tests {
    use indexmap::indexmap;

    use crate::{alloc_list, alloc_struct, alloc_tuple_of, Struct};

    use super::*;

    #[test]
    fn test_compare_int_int() {
        let db = TypeSystem::new();
        let types = db.std();
        assert_eq!(db.compare(types.int, types.int), Comparison::Equal);
    }

    #[test]
    fn test_compare_int_bytes() {
        let db = TypeSystem::new();
        let types = db.std();
        assert_eq!(db.compare(types.int, types.bytes), Comparison::Castable);
    }

    #[test]
    fn test_compare_bytes_int() {
        let db = TypeSystem::new();
        let types = db.std();
        assert_eq!(db.compare(types.bytes, types.int), Comparison::Castable);
    }

    #[test]
    fn test_compare_bytes_bytes32() {
        let db = TypeSystem::new();
        let types = db.std();
        assert_eq!(db.compare(types.bytes, types.bytes32), Comparison::Superset);
    }

    #[test]
    fn test_compare_bytes32_bytes() {
        let db = TypeSystem::new();
        let types = db.std();
        assert_eq!(
            db.compare(types.bytes32, types.bytes),
            Comparison::Assignable
        );
    }

    #[test]
    fn test_compare_bytes_public_key() {
        let db = TypeSystem::new();
        let types = db.std();
        assert_eq!(
            db.compare(types.bytes, types.public_key),
            Comparison::Superset
        );
    }

    #[test]
    fn test_compare_public_key_bytes() {
        let db = TypeSystem::new();
        let types = db.std();
        assert_eq!(
            db.compare(types.public_key, types.bytes),
            Comparison::Castable
        );
    }

    #[test]
    fn test_compare_bytes32_public_key() {
        let db = TypeSystem::new();
        let types = db.std();
        assert_eq!(
            db.compare(types.bytes32, types.public_key),
            Comparison::Incompatible
        );
    }

    #[test]
    fn test_compare_public_key_bytes32() {
        let db = TypeSystem::new();
        let types = db.std();
        assert_eq!(
            db.compare(types.public_key, types.bytes32),
            Comparison::Incompatible
        );
    }

    #[test]
    fn test_compare_bytes_any() {
        let db = TypeSystem::new();
        let types = db.std();
        assert_eq!(db.compare(types.bytes, types.any), Comparison::Assignable);
    }

    #[test]
    fn test_compare_any_bytes() {
        let db = TypeSystem::new();
        let types = db.std();
        assert_eq!(db.compare(types.any, types.bytes), Comparison::Superset);
    }

    #[test]
    fn test_compare_bytes32_any() {
        let db = TypeSystem::new();
        let types = db.std();
        assert_eq!(db.compare(types.bytes32, types.any), Comparison::Assignable);
    }

    #[test]
    fn test_compare_any_bytes32() {
        let db = TypeSystem::new();
        let types = db.std();
        assert_eq!(db.compare(types.any, types.bytes32), Comparison::Superset);
    }

    #[test]
    fn test_compare_list_any() {
        let mut db = TypeSystem::new();
        let types = db.std();
        let list = alloc_list(&mut db, types.int);
        assert_eq!(db.compare(list, types.any), Comparison::Assignable);
    }

    #[test]
    fn test_compare_pair_any() {
        let mut db = TypeSystem::new();
        let types = db.std();
        let pair = db.alloc(Type::Pair(types.int, types.public_key));
        assert_eq!(db.compare(pair, types.any), Comparison::Assignable);
    }

    #[test]
    fn test_compare_int_any() {
        let db = TypeSystem::new();
        let types = db.std();
        assert_eq!(db.compare(types.int, types.any), Comparison::Assignable);
    }

    #[test]
    fn test_compare_public_key_any() {
        let db = TypeSystem::new();
        let types = db.std();
        assert_eq!(
            db.compare(types.public_key, types.any),
            Comparison::Assignable
        );
    }

    #[test]
    fn test_compare_complex_any() {
        let mut db = TypeSystem::new();
        let types = db.std();
        let pair_inner_inner = db.alloc(Type::Pair(types.any, types.nil));
        let pair_inner = db.alloc(Type::Pair(pair_inner_inner, pair_inner_inner));
        let pair = db.alloc(Type::Pair(types.int, pair_inner));
        let list = alloc_list(&mut db, pair);
        assert_eq!(db.compare(list, types.any), Comparison::Assignable);
    }

    #[test]
    fn test_point_struct_any() {
        let mut db = TypeSystem::new();
        let types = db.std();
        let point = alloc_struct(
            &mut db,
            &indexmap! {
                "x".to_string() => types.int,
                "y".to_string() => types.int,
            },
            true,
        );
        assert_eq!(db.compare(point, types.any), Comparison::Assignable);
    }

    #[test]
    fn test_compare_any_any() {
        let db = TypeSystem::new();
        let types = db.std();
        assert_eq!(db.compare(types.any, types.any), Comparison::Equal);
    }

    #[test]
    fn test_compare_incompatible_pair() {
        let mut db = TypeSystem::new();
        let types = db.std();
        let lhs = db.alloc(Type::Pair(types.int, types.public_key));
        let rhs = db.alloc(Type::Pair(types.bytes, types.nil));
        assert_eq!(db.compare(lhs, rhs), Comparison::Incompatible);
    }

    #[test]
    fn test_compare_castable_pair() {
        let mut db = TypeSystem::new();
        let types = db.std();
        let lhs = db.alloc(Type::Pair(types.int, types.public_key));
        let rhs = db.alloc(Type::Pair(types.bytes, types.bytes));
        assert_eq!(db.compare(lhs, rhs), Comparison::Castable);
    }

    #[test]
    fn test_compare_assignable_pair() {
        let mut db = TypeSystem::new();
        let types = db.std();
        let lhs = db.alloc(Type::Pair(types.int, types.public_key));
        let rhs = db.alloc(Type::Pair(types.any, types.any));
        assert_eq!(db.compare(lhs, rhs), Comparison::Assignable);
    }

    #[test]
    fn test_compare_nil_list() {
        let mut db = TypeSystem::new();
        let types = db.std();
        let list = alloc_list(&mut db, types.int);
        assert_eq!(db.compare(types.nil, list), Comparison::Assignable);
    }

    #[test]
    fn test_compare_pair_list() {
        let mut db = TypeSystem::new();
        let types = db.std();
        let pair = db.alloc(Type::Pair(types.int, types.nil));
        let list = alloc_list(&mut db, types.int);
        assert_eq!(db.compare(pair, list), Comparison::Assignable);
    }

    #[test]
    fn test_compare_generic_inference() {
        let mut db = TypeSystem::new();
        let types = db.std();

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

    #[test]
    fn test_compare_union_to_rhs_incompatible() {
        let mut db = TypeSystem::new();
        let types = db.std();

        let pair = db.alloc(Type::Pair(types.int, types.public_key));
        let union = db.alloc(Type::Union(vec![types.bytes32, pair, types.nil]));
        assert_eq!(db.compare(union, types.bytes), Comparison::Incompatible);
    }

    #[test]
    fn test_compare_union_to_rhs_superset() {
        let mut db = TypeSystem::new();
        let types = db.std();

        let pair = db.alloc(Type::Pair(types.int, types.public_key));
        let union = db.alloc(Type::Union(vec![types.bytes, pair]));
        assert_eq!(db.compare(union, types.bytes), Comparison::Superset);
    }

    #[test]
    fn test_compare_union_to_rhs_assignable() {
        let mut db = TypeSystem::new();
        let types = db.std();

        let union = db.alloc(Type::Union(vec![types.bytes32, types.nil]));
        assert_eq!(db.compare(union, types.bytes), Comparison::Assignable);
    }

    #[test]
    fn test_compare_lhs_to_union_incompatible() {
        let mut db = TypeSystem::new();
        let types = db.std();

        let pair = db.alloc(Type::Pair(types.int, types.public_key));
        let union = db.alloc(Type::Union(vec![types.bytes32, pair, types.nil]));
        assert_eq!(db.compare(types.bytes, union), Comparison::Incompatible);
    }

    #[test]
    fn test_compare_same_derivative_struct() {
        let mut db = TypeSystem::new();
        let types = db.std();

        let struct_type = alloc_struct(
            &mut db,
            &indexmap! {
                "x".to_string() => types.int,
                "y".to_string() => types.int,
            },
            true,
        );

        let Type::Struct(original) = db.get(struct_type) else {
            unreachable!();
        };

        let derivative_struct_type = db.alloc(Type::Struct(Struct {
            original_type_id: struct_type,
            type_id: original.type_id,
            field_names: original.field_names.clone(),
            nil_terminated: original.nil_terminated,
            generic_types: original.generic_types.clone(),
        }));

        assert_eq!(
            db.compare(derivative_struct_type, struct_type),
            Comparison::Equal
        );

        assert_eq!(
            db.compare(struct_type, derivative_struct_type),
            Comparison::Equal
        );
    }

    #[test]
    fn test_compare_different_derivative_struct() {
        let mut db = TypeSystem::new();
        let types = db.std();

        let struct_type = alloc_struct(
            &mut db,
            &indexmap! {
                "x".to_string() => types.int,
                "y".to_string() => types.int,
            },
            true,
        );

        let Type::Struct(original) = db.get(struct_type).clone() else {
            unreachable!();
        };

        let new_inner = alloc_tuple_of(&mut db, [types.int, types.bytes, types.nil].into_iter());

        let derivative_struct_type = db.alloc(Type::Struct(Struct {
            original_type_id: struct_type,
            type_id: new_inner,
            field_names: original.field_names,
            nil_terminated: original.nil_terminated,
            generic_types: original.generic_types,
        }));

        assert_eq!(
            db.compare(derivative_struct_type, struct_type),
            Comparison::Castable
        );

        assert_eq!(
            db.compare(struct_type, derivative_struct_type),
            Comparison::Castable
        );
    }

    #[test]
    fn test_compare_different_struct() {
        let mut db = TypeSystem::new();
        let types = db.std();

        let struct_type = alloc_struct(
            &mut db,
            &indexmap! {
                "x".to_string() => types.int,
                "y".to_string() => types.int,
            },
            true,
        );

        let other_struct_type = alloc_struct(
            &mut db,
            &indexmap! {
                "x".to_string() => types.int,
                "y".to_string() => types.int,
            },
            true,
        );

        assert_eq!(
            db.compare(struct_type, other_struct_type),
            Comparison::Castable
        );
    }

    #[test]
    fn test_compare_generic_equal() {
        let mut db = TypeSystem::new();
        let types = db.std();
        let generic = db.alloc(Type::Generic);
        assert_eq!(db.compare(types.int, generic), Comparison::Incompatible);
        assert_eq!(db.compare(generic, generic), Comparison::Equal);
    }

    #[test]
    fn test_compare_generic_list_assignable() {
        let mut db = TypeSystem::new();
        let types = db.std();
        let generic = db.alloc(Type::Generic);
        let list = alloc_list(&mut db, generic);
        let pair = db.alloc(Type::Pair(generic, list));
        assert_eq!(db.compare(types.nil, list), Comparison::Assignable);
        assert_eq!(db.compare(list, list), Comparison::Equal);
        assert_eq!(db.compare(pair, list), Comparison::Assignable);
    }

    #[test]
    fn test_compare_pair_union() {
        let mut db = TypeSystem::new();
        let types = db.std();

        let pair_enum = db.alloc(Type::Pair(types.int, types.nil));
        let pair_enum = db.alloc(Type::Pair(types.int, pair_enum));
        let zero = db.alloc(Type::Value(BigInt::ZERO));
        let pair_enum = db.alloc(Type::Pair(zero, pair_enum));

        let int_enum = db.alloc(Type::Pair(types.int, types.nil));
        let one = db.alloc(Type::Value(BigInt::one()));
        let int_enum = db.alloc(Type::Pair(one, int_enum));

        let union = db.alloc(Type::Union(vec![pair_enum, int_enum]));

        assert_eq!(db.compare(pair_enum, union), Comparison::Assignable);
    }
}
