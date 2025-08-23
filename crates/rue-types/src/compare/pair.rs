use id_arena::Arena;

use crate::{Check, Comparison, ComparisonContext, Pair, Type, compare_with_context};

pub(crate) fn compare_pair(
    arena: &Arena<Type>,
    ctx: &mut ComparisonContext,
    lhs: Pair,
    rhs: Pair,
) -> Comparison {
    let first = compare_with_context(arena, ctx, lhs.first, rhs.first);

    let rest = compare_with_context(arena, ctx, lhs.rest, rhs.rest);

    match (first, rest) {
        (Comparison::Unresolved, _) | (_, Comparison::Unresolved) => Comparison::Unresolved,
        (Comparison::Invalid, _) | (_, Comparison::Invalid) => Comparison::Invalid,
        (Comparison::Assign, Comparison::Assign) => Comparison::Assign,
        (Comparison::Cast, Comparison::Cast) => Comparison::Cast,
        (Comparison::Assign, Comparison::Cast) | (Comparison::Cast, Comparison::Assign) => {
            Comparison::Cast
        }
        (Comparison::Check(lhs), Comparison::Check(rhs)) => {
            Comparison::Check(Check::Pair(Box::new(lhs), Box::new(rhs)))
        }
        (Comparison::Assign | Comparison::Cast, Comparison::Check(rhs)) => {
            Comparison::Check(Check::Pair(Box::new(Check::None), Box::new(rhs)))
        }
        (Comparison::Check(lhs), Comparison::Assign | Comparison::Cast) => {
            Comparison::Check(Check::Pair(Box::new(lhs), Box::new(Check::None)))
        }
    }
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use id_arena::Arena;

    use crate::{Atom, AtomKind, AtomRestriction, Type, compare};

    use super::*;

    #[test]
    fn test_pair_assignable() {
        let mut arena = Arena::new();
        let nil = arena.alloc(Type::Atom(Atom::NIL));
        let lhs = arena.alloc(Type::Pair(Pair::new(nil, nil)));
        let rhs = arena.alloc(Type::Pair(Pair::new(nil, nil)));
        assert_eq!(compare(&mut arena, lhs, rhs), Comparison::Assign);
    }

    #[test]
    fn test_pair_castable() {
        let mut arena = Arena::new();
        let nil = arena.alloc(Type::Atom(Atom::NIL));
        let num = arena.alloc(Type::Atom(Atom::new(
            AtomKind::Int,
            Some(AtomRestriction::Value(Cow::Borrowed(&[]))),
        )));
        let lhs = arena.alloc(Type::Pair(Pair::new(nil, num)));
        let rhs = arena.alloc(Type::Pair(Pair::new(num, nil)));
        assert_eq!(compare(&mut arena, lhs, rhs), Comparison::Cast);
    }

    #[test]
    fn test_pair_check() {
        let mut arena = Arena::new();
        let bytes = arena.alloc(Type::Atom(Atom::BYTES));
        let bytes32 = arena.alloc(Type::Atom(Atom::BYTES_32));
        let lhs = arena.alloc(Type::Pair(Pair::new(bytes, bytes)));
        let rhs = arena.alloc(Type::Pair(Pair::new(bytes32, bytes)));
        assert_eq!(
            compare(&mut arena, lhs, rhs),
            Comparison::Check(Check::Pair(
                Box::new(Check::Atom(AtomRestriction::Length(32))),
                Box::new(Check::None)
            ))
        );
    }

    #[test]
    fn test_pair_invalid() {
        let mut arena = Arena::new();
        let nil = arena.alloc(Type::Atom(Atom::NIL));
        let incompatible = arena.alloc(Type::Atom(Atom::TRUE));
        let lhs = arena.alloc(Type::Pair(Pair::new(nil, incompatible)));
        let rhs = arena.alloc(Type::Pair(Pair::new(nil, nil)));
        assert_eq!(compare(&mut arena, lhs, rhs), Comparison::Invalid);
    }
}
