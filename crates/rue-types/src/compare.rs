mod atom;
mod pair;
mod unions;

use atom::*;
use pair::*;
use unions::*;

use id_arena::Arena;

use crate::{Check, Type, TypeId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Comparison {
    Unresolved,
    Assign,
    Cast,
    Check(Check),
    Invalid,
}

pub fn compare(arena: &Arena<Type>, lhs: TypeId, rhs: TypeId) -> Comparison {
    match (arena[lhs].clone(), arena[rhs].clone()) {
        (Type::Unresolved, _) | (_, Type::Unresolved) => Comparison::Unresolved,
        (Type::Generic, _) => Comparison::Invalid,
        (Type::Atom(lhs), Type::Atom(rhs)) => compare_atom(lhs, rhs),
        (Type::Pair(lhs), Type::Pair(rhs)) => compare_pair(arena, lhs, rhs),
        (Type::Pair(_), Type::Atom(_)) => Comparison::Invalid,
        (Type::Atom(_), Type::Pair(_)) => Comparison::Invalid,
        (Type::Union(lhs), _) => compare_from_union(arena, lhs, rhs),
        (_, Type::Union(rhs)) => compare_to_union(arena, lhs, rhs),
        (lhs, rhs) => todo!("{lhs:?}, {rhs:?}"),
    }
}
