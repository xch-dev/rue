mod atom;
mod pair;
mod unions;

use atom::*;
use pair::*;
use unions::*;

use id_arena::Arena;
use indexmap::IndexSet;

use crate::{Check, Type, TypeId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Comparison {
    Unresolved,
    Assign,
    Cast,
    Check(Check),
    Invalid,
}

#[derive(Debug, Default)]
pub struct ComparisonContext {
    stack: IndexSet<(TypeId, TypeId)>,
}

impl ComparisonContext {
    pub fn new() -> Self {
        Self::default()
    }
}

pub fn compare(arena: &Arena<Type>, lhs: TypeId, rhs: TypeId) -> Comparison {
    compare_with_context(arena, &mut ComparisonContext::new(), lhs, rhs)
}

pub(crate) fn compare_with_context(
    arena: &Arena<Type>,
    ctx: &mut ComparisonContext,
    lhs: TypeId,
    rhs: TypeId,
) -> Comparison {
    if !ctx.stack.insert((lhs, rhs)) {
        return Comparison::Assign;
    }

    let result = match (arena[lhs].clone(), arena[rhs].clone()) {
        (Type::Unresolved, _) | (_, Type::Unresolved) => Comparison::Unresolved,
        (Type::Generic, _) => Comparison::Invalid,
        (_, Type::Generic) => todo!(),
        (Type::Apply(_), _) => todo!(),
        (_, Type::Apply(_)) => todo!(),
        (Type::Atom(lhs), Type::Atom(rhs)) => compare_atom(lhs, rhs),
        (Type::Pair(lhs), Type::Pair(rhs)) => compare_pair(arena, lhs, rhs),
        (Type::Pair(_), Type::Atom(_)) => Comparison::Invalid,
        (Type::Atom(_), Type::Pair(_)) => Comparison::Invalid,
        (Type::Alias(lhs), _) => compare_with_context(arena, ctx, lhs.inner, rhs),
        (_, Type::Alias(rhs)) => compare_with_context(arena, ctx, lhs, rhs.inner),
        (Type::Struct(lhs), Type::Struct(rhs)) => {
            match compare_with_context(arena, ctx, lhs.inner, rhs.inner) {
                Comparison::Assign if lhs.semantic != rhs.semantic => Comparison::Cast,
                comparison => comparison,
            }
        }
        (Type::Struct(lhs), _) => match compare_with_context(arena, ctx, lhs.inner, rhs) {
            Comparison::Assign => Comparison::Cast,
            comparison => comparison,
        },
        (_, Type::Struct(rhs)) => match compare_with_context(arena, ctx, lhs, rhs.inner) {
            Comparison::Assign => Comparison::Cast,
            comparison => comparison,
        },
        (Type::Union(lhs), _) => compare_from_union(arena, ctx, lhs, rhs),
        (_, Type::Union(rhs)) => compare_to_union(arena, ctx, lhs, rhs),
    };

    ctx.stack.pop().unwrap();

    result
}
