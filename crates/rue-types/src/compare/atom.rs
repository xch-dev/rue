use crate::{Atom, AtomRestriction, Check, Comparison};

pub(crate) fn compare_atom(lhs: Atom, rhs: Atom) -> Comparison {
    let valid = if lhs.kind == rhs.kind {
        Comparison::Assign
    } else {
        Comparison::Cast
    };

    match (lhs.restriction, rhs.restriction) {
        (_, None) => valid,
        (Some(AtomRestriction::Value(lhs)), Some(AtomRestriction::Value(rhs))) => {
            if lhs == rhs {
                valid
            } else {
                Comparison::Invalid
            }
        }
        (Some(AtomRestriction::Length(lhs)), Some(AtomRestriction::Length(rhs))) => {
            if lhs == rhs {
                valid
            } else {
                Comparison::Invalid
            }
        }
        (Some(AtomRestriction::Value(lhs)), Some(AtomRestriction::Length(rhs))) => {
            if lhs.len() == rhs {
                valid
            } else {
                Comparison::Invalid
            }
        }
        (Some(AtomRestriction::Length(lhs)), Some(AtomRestriction::Value(rhs))) => {
            if lhs == rhs.len() {
                Comparison::Check(Check::Atom(AtomRestriction::Value(rhs)))
            } else {
                Comparison::Invalid
            }
        }
        (None, Some(restriction)) => Comparison::Check(Check::Atom(restriction)),
    }
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use id_arena::Arena;
    use rstest::rstest;

    use crate::{Atom, AtomKind, Type, compare};

    use super::*;

    #[rstest]
    #[case(Atom::NIL, Atom::NIL, Comparison::Assign)]
    #[case(Atom::NIL, Atom::FALSE, Comparison::Cast)]
    #[case(Atom::NIL, Atom::TRUE, Comparison::Invalid)]
    #[case(Atom::NIL, Atom::BYTES, Comparison::Assign)]
    #[case(Atom::NIL, Atom::BYTES_32, Comparison::Invalid)]
    #[case(Atom::NIL, Atom::PUBLIC_KEY, Comparison::Invalid)]
    #[case(Atom::NIL, Atom::INT, Comparison::Cast)]
    #[case(Atom::FALSE, Atom::NIL, Comparison::Cast)]
    #[case(Atom::FALSE, Atom::FALSE, Comparison::Assign)]
    #[case(Atom::FALSE, Atom::TRUE, Comparison::Invalid)]
    #[case(Atom::FALSE, Atom::BYTES, Comparison::Cast)]
    #[case(Atom::FALSE, Atom::BYTES_32, Comparison::Invalid)]
    #[case(Atom::FALSE, Atom::PUBLIC_KEY, Comparison::Invalid)]
    #[case(Atom::FALSE, Atom::INT, Comparison::Cast)]
    #[case(Atom::TRUE, Atom::NIL, Comparison::Invalid)]
    #[case(Atom::TRUE, Atom::FALSE, Comparison::Invalid)]
    #[case(Atom::TRUE, Atom::TRUE, Comparison::Assign)]
    #[case(Atom::TRUE, Atom::BYTES, Comparison::Cast)]
    #[case(Atom::TRUE, Atom::BYTES_32, Comparison::Invalid)]
    #[case(Atom::TRUE, Atom::PUBLIC_KEY, Comparison::Invalid)]
    #[case(Atom::TRUE, Atom::INT, Comparison::Cast)]
    #[case(Atom::BYTES, Atom::NIL, Comparison::Check(Check::Atom(AtomRestriction::Value(Cow::Borrowed(&[])))))]
    #[case(Atom::BYTES, Atom::FALSE, Comparison::Check(Check::Atom(AtomRestriction::Value(Cow::Borrowed(&[])))))]
    #[case(Atom::BYTES, Atom::TRUE, Comparison::Check(Check::Atom(AtomRestriction::Value(Cow::Borrowed(&[1])))))]
    #[case(Atom::BYTES, Atom::BYTES, Comparison::Assign)]
    #[case(
        Atom::BYTES,
        Atom::BYTES_32,
        Comparison::Check(Check::Atom(AtomRestriction::Length(32)))
    )]
    #[case(
        Atom::BYTES,
        Atom::PUBLIC_KEY,
        Comparison::Check(Check::Atom(AtomRestriction::Length(48)))
    )]
    #[case(Atom::BYTES, Atom::INT, Comparison::Cast)]
    #[case(Atom::BYTES_32, Atom::NIL, Comparison::Invalid)]
    #[case(Atom::BYTES_32, Atom::FALSE, Comparison::Invalid)]
    #[case(Atom::BYTES_32, Atom::TRUE, Comparison::Invalid)]
    #[case(Atom::BYTES_32, Atom::BYTES, Comparison::Assign)]
    #[case(Atom::BYTES_32, Atom::BYTES_32, Comparison::Assign)]
    #[case(Atom::BYTES_32, Atom::PUBLIC_KEY, Comparison::Invalid)]
    #[case(Atom::BYTES_32, Atom::INT, Comparison::Cast)]
    #[case(Atom::PUBLIC_KEY, Atom::NIL, Comparison::Invalid)]
    #[case(Atom::PUBLIC_KEY, Atom::FALSE, Comparison::Invalid)]
    #[case(Atom::PUBLIC_KEY, Atom::TRUE, Comparison::Invalid)]
    #[case(Atom::PUBLIC_KEY, Atom::BYTES, Comparison::Cast)]
    #[case(Atom::PUBLIC_KEY, Atom::BYTES_32, Comparison::Invalid)]
    #[case(Atom::PUBLIC_KEY, Atom::PUBLIC_KEY, Comparison::Assign)]
    #[case(Atom::PUBLIC_KEY, Atom::INT, Comparison::Cast)]
    #[case(Atom::INT, Atom::NIL, Comparison::Check(Check::Atom(AtomRestriction::Value(Cow::Borrowed(&[])))))]
    #[case(Atom::INT, Atom::FALSE, Comparison::Check(Check::Atom(AtomRestriction::Value(Cow::Borrowed(&[])))))]
    #[case(Atom::INT, Atom::TRUE, Comparison::Check(Check::Atom(AtomRestriction::Value(Cow::Borrowed(&[1])))))]
    #[case(Atom::INT, Atom::BYTES, Comparison::Cast)]
    #[case(
        Atom::INT,
        Atom::BYTES_32,
        Comparison::Check(Check::Atom(AtomRestriction::Length(32)))
    )]
    #[case(
        Atom::INT,
        Atom::PUBLIC_KEY,
        Comparison::Check(Check::Atom(AtomRestriction::Length(48)))
    )]
    #[case(Atom::INT, Atom::INT, Comparison::Assign)]
    #[case(Atom::new(AtomKind::Int, Some(AtomRestriction::Value(Cow::Borrowed(&[1])))), Atom::INT, Comparison::Assign)]
    #[case(Atom::new(AtomKind::Int, Some(AtomRestriction::Value(Cow::Borrowed(&[1])))), Atom::BYTES, Comparison::Cast)]
    fn test_atoms(#[case] lhs: Atom, #[case] rhs: Atom, #[case] expected: Comparison) {
        let mut arena = Arena::new();
        let lhs_id = arena.alloc(Type::Atom(lhs.clone()));
        let rhs_id = arena.alloc(Type::Atom(rhs.clone()));
        assert_eq!(
            compare(&mut arena, lhs_id, rhs_id),
            expected,
            "{} -> {}",
            lhs,
            rhs
        );
    }
}
