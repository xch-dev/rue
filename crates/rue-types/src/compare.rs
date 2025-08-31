use std::{
    cmp::{max, min},
    collections::HashMap,
};

use id_arena::Arena;

use crate::{AtomRestriction, AtomSemantic, Type, TypeId, substitute};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Comparison {
    Assign,
    Cast,
    Invalid,
}

#[derive(Debug)]
pub struct ComparisonContext<'a> {
    infer: Option<&'a mut HashMap<TypeId, TypeId>>,
}

pub fn compare_with_inference(
    arena: &mut Arena<Type>,

    lhs: TypeId,
    rhs: TypeId,
    infer: Option<&mut HashMap<TypeId, TypeId>>,
) -> Comparison {
    let lhs = substitute(arena, lhs);
    let rhs = substitute(arena, rhs);
    compare_impl(arena, &mut ComparisonContext { infer }, lhs, rhs)
}

pub fn compare(arena: &mut Arena<Type>, lhs: TypeId, rhs: TypeId) -> Comparison {
    compare_with_inference(arena, lhs, rhs, None)
}

fn compare_impl(
    arena: &Arena<Type>,
    ctx: &mut ComparisonContext,
    lhs: TypeId,
    rhs: TypeId,
) -> Comparison {
    match (arena[lhs].clone(), arena[rhs].clone()) {
        (Type::Apply(_), _) | (_, Type::Apply(_)) => unreachable!(),
        (Type::Unresolved, _) | (_, Type::Unresolved) => Comparison::Assign,
        (Type::Never, _) => Comparison::Assign,
        (_, Type::Any) => Comparison::Assign,
        (Type::Atom(lhs), Type::Atom(rhs)) => {
            let semantic = if lhs.semantic == rhs.semantic || rhs.semantic == AtomSemantic::Any {
                Comparison::Assign
            } else {
                Comparison::Cast
            };

            let restriction = match (lhs.restriction, rhs.restriction) {
                (_, None) => Comparison::Assign,
                (None, _) => Comparison::Invalid,
                (Some(AtomRestriction::Length(lhs)), Some(AtomRestriction::Length(rhs))) => {
                    if lhs == rhs {
                        Comparison::Assign
                    } else {
                        Comparison::Invalid
                    }
                }
                (Some(AtomRestriction::Value(lhs)), Some(AtomRestriction::Value(rhs))) => {
                    if lhs == rhs {
                        Comparison::Assign
                    } else {
                        Comparison::Invalid
                    }
                }
                (Some(AtomRestriction::Length(_)), Some(AtomRestriction::Value(_))) => {
                    Comparison::Invalid
                }
                (Some(AtomRestriction::Value(lhs)), Some(AtomRestriction::Length(rhs))) => {
                    if lhs.len() == rhs {
                        Comparison::Assign
                    } else {
                        Comparison::Invalid
                    }
                }
            };

            max(semantic, restriction)
        }
        (Type::Pair(lhs), Type::Pair(rhs)) => {
            let first = compare_impl(arena, ctx, lhs.first, rhs.first);
            let rest = compare_impl(arena, ctx, lhs.rest, rhs.rest);
            max(first, rest)
        }
        (Type::Atom(_), Type::Pair(_)) => Comparison::Invalid,
        (Type::Pair(_), Type::Atom(_)) => Comparison::Invalid,
        (Type::List(lhs), Type::List(rhs)) => compare_impl(arena, ctx, lhs, rhs),
        (Type::Atom(atom), Type::List(_)) => {
            let semantic = if atom.semantic == AtomSemantic::Bytes {
                Comparison::Assign
            } else {
                Comparison::Cast
            };

            let restriction = match atom.restriction {
                None => Comparison::Invalid,
                Some(AtomRestriction::Length(length)) => {
                    if length == 0 {
                        Comparison::Assign
                    } else {
                        Comparison::Invalid
                    }
                }
                Some(AtomRestriction::Value(value)) => {
                    if value.is_empty() {
                        Comparison::Assign
                    } else {
                        Comparison::Invalid
                    }
                }
            };

            max(semantic, restriction)
        }
        (Type::List(_), Type::Atom(_)) => Comparison::Invalid,
        (Type::List(_), Type::Pair(_)) => Comparison::Invalid,
        (Type::Pair(pair), Type::List(inner)) => {
            let first = compare_impl(arena, ctx, pair.first, inner);
            let rest = compare_impl(arena, ctx, pair.rest, rhs);
            max(first, rest)
        }
        (Type::Any, Type::Atom(_)) => Comparison::Invalid,
        (Type::Any, Type::Pair(_)) => Comparison::Invalid,
        (Type::Any, Type::List(_)) => Comparison::Invalid,
        (Type::Any, Type::Function(_)) => Comparison::Invalid,
        (Type::Function(_), Type::Atom(_)) => Comparison::Invalid,
        (Type::Function(_), Type::Pair(_)) => Comparison::Invalid,
        (Type::Function(_), Type::List(_)) => Comparison::Invalid,
        (Type::Atom(_), Type::Function(_)) => Comparison::Invalid,
        (Type::Pair(_), Type::Function(_)) => Comparison::Invalid,
        (Type::List(_), Type::Function(_)) => Comparison::Invalid,
        (Type::Function(lhs), Type::Function(rhs)) => {
            // TODO: We could relax the identical parameter requirement

            if lhs.nil_terminated != rhs.nil_terminated {
                return Comparison::Invalid;
            }

            if lhs.params.len() != rhs.params.len() {
                return Comparison::Invalid;
            }

            let mut result = compare_impl(arena, ctx, lhs.ret, rhs.ret);

            for (i, param) in lhs.params.iter().enumerate() {
                result = max(result, compare_impl(arena, ctx, *param, rhs.params[i]));
            }

            result
        }
        (_, Type::Generic) => {
            if lhs == rhs {
                return Comparison::Assign;
            }

            if let Some(infer) = &mut ctx.infer {
                if let Some(lhs) = infer.get(&lhs).copied() {
                    return compare_impl(arena, ctx, lhs, rhs);
                }

                infer.insert(rhs, lhs);
                return Comparison::Assign;
            }

            Comparison::Invalid
        }
        (Type::Union(lhs), _) => {
            let mut result = Comparison::Assign;

            for &id in &lhs.types {
                result = max(result, compare_impl(arena, ctx, id, rhs));
            }

            result
        }
        (_, Type::Union(rhs)) => {
            let mut result = Comparison::Invalid;

            for &id in &rhs.types {
                result = min(result, compare_impl(arena, ctx, lhs, id));
            }

            result
        }
        (Type::Struct(lhs), Type::Struct(rhs)) => max(
            compare_impl(arena, ctx, lhs.inner, rhs.inner),
            if lhs.semantic == rhs.semantic {
                Comparison::Assign
            } else {
                Comparison::Cast
            },
        ),
        (Type::Struct(lhs), _) => max(compare_impl(arena, ctx, lhs.inner, rhs), Comparison::Cast),
        (_, Type::Struct(rhs)) => max(compare_impl(arena, ctx, lhs, rhs.inner), Comparison::Cast),
        (Type::Alias(lhs), _) => compare_impl(arena, ctx, lhs.inner, rhs),
        (_, Type::Alias(rhs)) => compare_impl(arena, ctx, lhs, rhs.inner),
        (_, Type::Never) => Comparison::Invalid,
        (Type::Generic, _) => Comparison::Invalid,
    }
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use id_arena::Arena;
    use rstest::rstest;

    use crate::{Atom, Type, compare};

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
    #[case(Atom::BYTES, Atom::NIL, Comparison::Invalid)]
    #[case(Atom::BYTES, Atom::FALSE, Comparison::Invalid)]
    #[case(Atom::BYTES, Atom::TRUE, Comparison::Invalid)]
    #[case(Atom::BYTES, Atom::BYTES, Comparison::Assign)]
    #[case(Atom::BYTES, Atom::BYTES_32, Comparison::Invalid)]
    #[case(Atom::BYTES, Atom::PUBLIC_KEY, Comparison::Invalid)]
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
    #[case(Atom::INT, Atom::NIL, Comparison::Invalid)]
    #[case(Atom::INT, Atom::FALSE, Comparison::Invalid)]
    #[case(Atom::INT, Atom::TRUE, Comparison::Invalid)]
    #[case(Atom::INT, Atom::BYTES, Comparison::Cast)]
    #[case(Atom::INT, Atom::BYTES_32, Comparison::Invalid)]
    #[case(Atom::INT, Atom::PUBLIC_KEY, Comparison::Invalid)]
    #[case(Atom::INT, Atom::INT, Comparison::Assign)]
    #[case(Atom::new(AtomSemantic::Int, Some(AtomRestriction::Value(Cow::Borrowed(&[1])))), Atom::INT, Comparison::Assign)]
    #[case(Atom::new(AtomSemantic::Int, Some(AtomRestriction::Value(Cow::Borrowed(&[1])))), Atom::BYTES, Comparison::Cast)]
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
