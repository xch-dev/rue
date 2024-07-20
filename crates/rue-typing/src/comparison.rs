use std::{
    cmp::{max, min},
    collections::{HashMap, HashSet},
};

use crate::{Type, TypeId, TypeSystem};

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
    types: &TypeSystem,
    lhs: TypeId,
    rhs: TypeId,
    ctx: &mut ComparisonContext<'_>,
) -> Comparison {
    if !ctx.visited.insert((lhs, rhs)) {
        return Comparison::Assignable;
    }

    let comparison = match (types.get(lhs), types.get(rhs)) {
        (Type::Ref(..), _) | (_, Type::Ref(..)) => unreachable!(),

        (_, Type::Generic) => {
            let mut found = None;

            for substititons in ctx.substitution_stack.iter().rev() {
                if let Some(&substititon) = substititons.get(&rhs) {
                    found = Some(substititon);
                }
            }

            if let Some(found) = found {
                compare_type(types, lhs, found, ctx)
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
                compare_type(types, found, rhs, ctx)
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
                let cmp = compare_type(types, item, rhs, ctx);
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
                let cmp = compare_type(types, lhs, item, ctx);
                result = min(result, cmp);
            }

            max(result, Comparison::Assignable)
        }

        (Type::Pair(lhs_first, lhs_rest), Type::Pair(rhs_first, rhs_rest)) => {
            let first = compare_type(types, *lhs_first, *rhs_first, ctx);
            let rest = compare_type(types, *lhs_rest, *rhs_rest, ctx);
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

        (Type::Lazy(lazy), _) => {
            ctx.substitution_stack.push(lazy.substitutions.clone());
            let result = compare_type(types, lazy.type_id, rhs, ctx);
            ctx.substitution_stack.pop().unwrap();
            result
        }

        (_, Type::Lazy(lazy)) => {
            ctx.substitution_stack.push(lazy.substitutions.clone());
            let result = compare_type(types, lhs, lazy.type_id, ctx);
            ctx.substitution_stack.pop().unwrap();
            result
        }

        (Type::Alias(alias), _) => compare_type(types, alias.type_id, rhs, ctx),
        (_, Type::Alias(alias)) => compare_type(types, lhs, alias.type_id, ctx),
    };

    ctx.visited.remove(&(lhs, rhs));

    comparison
}
