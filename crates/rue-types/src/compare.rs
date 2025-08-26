mod atom;
mod function;
mod pair;
mod unions;

use std::borrow::Cow;

use atom::*;
use function::*;
use pair::*;
use unions::*;

use id_arena::Arena;

use crate::{AtomKind, AtomRestriction, Check, Type, TypeId, substitute};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Comparison {
    Assign,
    Cast,
    Check(Check),
    Invalid,
}

#[derive(Debug, Default)]
pub struct ComparisonContext {}

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
    arena: &mut Arena<Type>,
    ctx: &mut ComparisonContext,
    lhs: TypeId,
    rhs: TypeId,
) -> Comparison {
    match (arena[lhs].clone(), arena[rhs].clone()) {
        (Type::Apply(_), _) | (_, Type::Apply(_)) => unreachable!(),
        (Type::Unresolved, _) | (_, Type::Unresolved) => Comparison::Assign,
        (Type::Generic, _) => Comparison::Invalid,
        (_, Type::Generic) => todo!(),
        (Type::Never, _) => Comparison::Assign,
        (_, Type::Never) => Comparison::Invalid,
        (_, Type::Any) => Comparison::Assign,
        (Type::Any, Type::Atom(atom)) => {
            let Some(restriction) = atom.restriction else {
                return Comparison::Check(Check::IsAtom);
            };
            Comparison::Check(Check::And(vec![Check::IsAtom, Check::Atom(restriction)]))
        }
        (Type::Any, Type::Pair(pair)) => {
            let first = compare_with_context(arena, ctx, lhs, pair.first);
            let rest = compare_with_context(arena, ctx, lhs, pair.rest);
            match (first, rest) {
                (Comparison::Invalid, _) | (_, Comparison::Invalid) => Comparison::Invalid,
                (Comparison::Assign, Comparison::Assign) => Comparison::Assign,
                (Comparison::Cast, Comparison::Cast) => Comparison::Cast,
                (Comparison::Assign, Comparison::Cast) => Comparison::Cast,
                (Comparison::Cast, Comparison::Assign) => Comparison::Cast,
                (Comparison::Check(first), Comparison::Check(rest)) => {
                    Comparison::Check(Check::And(vec![
                        Check::IsPair,
                        Check::Pair(Box::new(first), Box::new(rest)),
                    ]))
                }
                (Comparison::Check(first), Comparison::Assign | Comparison::Cast) => {
                    Comparison::Check(Check::And(vec![
                        Check::IsPair,
                        Check::Pair(Box::new(first), Box::new(Check::None)),
                    ]))
                }
                (Comparison::Assign | Comparison::Cast, Comparison::Check(rest)) => {
                    Comparison::Check(Check::And(vec![
                        Check::IsPair,
                        Check::Pair(Box::new(Check::None), Box::new(rest)),
                    ]))
                }
            }
        }
        (Type::List(lhs), Type::List(rhs)) => compare_with_context(arena, ctx, lhs, rhs),
        (Type::Atom(atom), Type::List(_)) => {
            let Some(restriction) = atom.restriction else {
                return Comparison::Check(Check::Atom(AtomRestriction::Value(Cow::Borrowed(&[]))));
            };

            match restriction {
                AtomRestriction::Length(length) => {
                    if length == 0 {
                        if atom.kind == AtomKind::Bytes {
                            Comparison::Assign
                        } else {
                            Comparison::Cast
                        }
                    } else {
                        Comparison::Invalid
                    }
                }
                AtomRestriction::Value(value) => {
                    if value.is_empty() {
                        if atom.kind == AtomKind::Bytes {
                            Comparison::Assign
                        } else {
                            Comparison::Cast
                        }
                    } else {
                        Comparison::Invalid
                    }
                }
            }
        }
        (Type::List(_), Type::Atom(atom)) => {
            let Some(restriction) = atom.restriction else {
                return Comparison::Check(Check::And(vec![
                    Check::IsAtom,
                    Check::Atom(AtomRestriction::Value(Cow::Borrowed(&[]))),
                ]));
            };

            match restriction {
                AtomRestriction::Length(length) => {
                    if length == 0 {
                        Comparison::Check(Check::IsAtom)
                    } else {
                        Comparison::Invalid
                    }
                }
                AtomRestriction::Value(value) => {
                    if value.is_empty() {
                        Comparison::Check(Check::IsAtom)
                    } else {
                        Comparison::Invalid
                    }
                }
            }
        }
        (Type::List(inner), Type::Pair(pair)) => {
            let first = compare_with_context(arena, ctx, inner, pair.first);
            let rest = compare_with_context(arena, ctx, lhs, pair.rest);

            match (first, rest) {
                (Comparison::Invalid, _) | (_, Comparison::Invalid) => Comparison::Invalid,
                (Comparison::Assign, Comparison::Assign) => Comparison::Assign,
                (Comparison::Cast, Comparison::Cast) => Comparison::Cast,
                (Comparison::Assign, Comparison::Cast) => Comparison::Cast,
                (Comparison::Cast, Comparison::Assign) => Comparison::Cast,
                (Comparison::Check(first), Comparison::Check(rest)) => {
                    Comparison::Check(Check::And(vec![
                        Check::IsPair,
                        Check::Pair(Box::new(first), Box::new(rest)),
                    ]))
                }
                (Comparison::Check(first), Comparison::Assign | Comparison::Cast) => {
                    Comparison::Check(Check::And(vec![
                        Check::IsPair,
                        Check::Pair(Box::new(first), Box::new(Check::None)),
                    ]))
                }
                (Comparison::Assign | Comparison::Cast, Comparison::Check(rest)) => {
                    Comparison::Check(Check::And(vec![
                        Check::IsPair,
                        Check::Pair(Box::new(Check::None), Box::new(rest)),
                    ]))
                }
            }
        }
        (Type::Pair(pair), Type::List(inner)) => {
            let first = compare_with_context(arena, ctx, pair.first, inner);
            let rest = compare_with_context(arena, ctx, pair.rest, rhs);

            match (first, rest) {
                (Comparison::Invalid, _) | (_, Comparison::Invalid) => Comparison::Invalid,
                (Comparison::Assign, Comparison::Assign) => Comparison::Assign,
                (Comparison::Cast, Comparison::Cast) => Comparison::Cast,
                (Comparison::Assign, Comparison::Cast) => Comparison::Cast,
                (Comparison::Cast, Comparison::Assign) => Comparison::Cast,
                (Comparison::Check(first), Comparison::Check(rest)) => {
                    Comparison::Check(Check::Pair(Box::new(first), Box::new(rest)))
                }
                (Comparison::Check(first), Comparison::Assign | Comparison::Cast) => {
                    Comparison::Check(Check::Pair(Box::new(first), Box::new(Check::None)))
                }
                (Comparison::Assign | Comparison::Cast, Comparison::Check(rest)) => {
                    Comparison::Check(Check::Pair(Box::new(Check::None), Box::new(rest)))
                }
            }
        }
        (Type::Any, Type::List(_)) => Comparison::Invalid,
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
        (Type::Function(lhs), Type::Function(rhs)) => compare_function(arena, ctx, lhs, rhs),
        (Type::Function(_), _) | (_, Type::Function(_)) => Comparison::Invalid,
    }
}
