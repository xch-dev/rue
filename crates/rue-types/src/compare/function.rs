use id_arena::Arena;

use crate::{Comparison, ComparisonContext, FunctionType, Type, compare_with_context};

pub fn compare_function(
    arena: &mut Arena<Type>,
    ctx: &mut ComparisonContext,
    lhs: FunctionType,
    rhs: FunctionType,
) -> Comparison {
    if lhs.params.len() != rhs.params.len() {
        return Comparison::Invalid;
    }

    if lhs.nil_terminated != rhs.nil_terminated {
        // TODO: Make this work
        return Comparison::Invalid;
    }

    let mut cast = false;

    for (i, &lhs) in lhs.params.iter().enumerate() {
        let rhs = rhs.params[i];

        let comparison = compare_with_context(arena, ctx, lhs, rhs);

        match comparison {
            Comparison::Assign => {}
            Comparison::Cast => {
                cast = true;
            }
            Comparison::Check(_) | Comparison::Invalid => return Comparison::Invalid,
        }
    }

    let comparison = compare_with_context(arena, ctx, lhs.ret, rhs.ret);

    match comparison {
        Comparison::Assign => {}
        Comparison::Cast => {
            cast = true;
        }
        Comparison::Check(_) | Comparison::Invalid => return Comparison::Invalid,
    }

    if cast {
        Comparison::Cast
    } else {
        Comparison::Assign
    }
}
