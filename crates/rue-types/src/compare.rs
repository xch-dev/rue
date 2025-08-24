mod atom;
mod function;
mod pair;
mod unions;

use atom::*;
use function::*;
use pair::*;
use unions::*;

use id_arena::Arena;
use indexmap::IndexSet;

use crate::{Check, Type, TypeId, Union, substitute};

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
    comparison_stack: IndexSet<(TypeId, TypeId)>,
    refinement_stack: IndexSet<(Vec<TypeId>, Vec<TypeId>)>,
}

impl ComparisonContext {
    pub fn new() -> Self {
        Self::default()
    }
}

pub fn compare(arena: &mut Arena<Type>, lhs: TypeId, rhs: TypeId) -> Comparison {
    let lhs = substitute(arena, lhs);
    let rhs = substitute(arena, rhs);
    compare_with_context(arena, &mut ComparisonContext::new(), lhs, rhs)
}

pub(crate) fn compare_with_context(
    arena: &Arena<Type>,
    ctx: &mut ComparisonContext,
    lhs: TypeId,
    rhs: TypeId,
) -> Comparison {
    if !ctx.comparison_stack.insert((lhs, rhs)) {
        return Comparison::Assign;
    }

    let result = match (arena[lhs].clone(), arena[rhs].clone()) {
        (Type::Apply(_), _) | (_, Type::Apply(_)) => unreachable!(),
        (Type::Ref(lhs), _) => compare_with_context(arena, ctx, lhs, rhs),
        (_, Type::Ref(rhs)) => compare_with_context(arena, ctx, lhs, rhs),
        (Type::Unresolved, _) | (_, Type::Unresolved) => Comparison::Unresolved,
        (Type::Generic, _) => Comparison::Invalid,
        (_, Type::Generic) => todo!(),
        (Type::Function(lhs), Type::Function(rhs)) => compare_function(arena, ctx, lhs, rhs),
        (Type::Function(_), _) => compare_from_union(arena, ctx, Union::new(vec![lhs]), rhs),
        (_, Type::Function(_)) => Comparison::Invalid,
        (Type::Atom(lhs), Type::Atom(rhs)) => compare_atom(lhs, rhs),
        (Type::Pair(lhs), Type::Pair(rhs)) => compare_pair(arena, ctx, lhs, rhs),
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
        (Type::Union(lhs), _) => compare_from_union(arena, ctx, lhs, rhs),
        (_, Type::Union(rhs)) => compare_to_union(arena, ctx, lhs, rhs),
        (Type::Struct(lhs), _) => match compare_with_context(arena, ctx, lhs.inner, rhs) {
            Comparison::Assign => Comparison::Cast,
            comparison => comparison,
        },
        (_, Type::Struct(rhs)) => match compare_with_context(arena, ctx, lhs, rhs.inner) {
            Comparison::Assign => Comparison::Cast,
            comparison => comparison,
        },
    };

    ctx.comparison_stack.pop().unwrap();

    result
}
