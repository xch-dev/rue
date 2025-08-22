mod atom;
mod pair;

use atom::*;
use pair::*;

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
        (Type::Atom(lhs), Type::Atom(rhs)) => compare_atom(lhs, rhs),
        (Type::Pair(lhs), Type::Pair(rhs)) => compare_pair(arena, lhs, rhs),
        _ => todo!(),
    }
}
