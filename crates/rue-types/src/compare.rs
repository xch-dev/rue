mod atom;
mod function;
mod pair;
mod unions;

use std::{borrow::Cow, collections::HashMap};

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

#[derive(Debug)]
pub struct ComparisonContext<'a> {
    mappings: Option<&'a mut HashMap<TypeId, TypeId>>,
}

pub fn compare_with_mappings(
    arena: &mut Arena<Type>,
    lhs: TypeId,
    rhs: TypeId,
    mappings: Option<&mut HashMap<TypeId, TypeId>>,
) -> Comparison {
    let lhs = substitute(arena, lhs);
    let rhs = substitute(arena, rhs);
    compare_with_context(arena, &mut ComparisonContext { mappings }, lhs, rhs)
}

pub fn compare(arena: &mut Arena<Type>, lhs: TypeId, rhs: TypeId) -> Comparison {
    let lhs = substitute(arena, lhs);
    let rhs = substitute(arena, rhs);
    compare_with_context(arena, &mut ComparisonContext { mappings: None }, lhs, rhs)
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
        (_, Type::Generic) => {
            if lhs == rhs {
                return Comparison::Assign;
            }

            if let Some(mappings) = &mut ctx.mappings {
                if let Some(lhs) = mappings.get(&lhs).copied() {
                    return compare_with_context(arena, ctx, lhs, rhs);
                }

                mappings.insert(rhs, lhs);
                return Comparison::Assign;
            }

            Comparison::Invalid
        }
        (Type::Never, _) => Comparison::Assign,
        (_, Type::Never) => Comparison::Invalid,
        (_, Type::Any) => Comparison::Assign,
        (Type::Any | Type::Generic, Type::Atom(atom)) => {
            if let Some(restriction) = atom.restriction {
                Comparison::Check(Check::And(vec![Check::IsAtom, Check::Atom(restriction)]))
            } else {
                Comparison::Check(Check::IsAtom)
            }
        }
        (Type::Any | Type::Generic, Type::Pair(pair)) => {
            let first = compare_with_context(arena, ctx, lhs, pair.first);
            let rest = compare_with_context(arena, ctx, lhs, pair.rest);
            match (first, rest) {
                (Comparison::Invalid, _) | (_, Comparison::Invalid) => Comparison::Invalid,
                (Comparison::Assign, Comparison::Assign) => Comparison::Check(Check::IsPair),
                (Comparison::Cast, Comparison::Cast) => Comparison::Check(Check::IsPair),
                (Comparison::Assign, Comparison::Cast) => Comparison::Check(Check::IsPair),
                (Comparison::Cast, Comparison::Assign) => Comparison::Check(Check::IsPair),
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
        (Type::List(lhs), Type::List(rhs)) => match compare_with_context(arena, ctx, lhs, rhs) {
            Comparison::Assign => Comparison::Assign,
            Comparison::Cast => Comparison::Cast,
            _ => Comparison::Invalid,
        },
        (Type::Atom(atom), Type::List(_)) => {
            if let Some(restriction) = atom.restriction {
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
            } else {
                Comparison::Check(Check::Atom(AtomRestriction::Value(Cow::Borrowed(&[]))))
            }
        }
        (Type::List(_), Type::Atom(atom)) => {
            if let Some(restriction) = atom.restriction {
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
            } else {
                Comparison::Check(Check::And(vec![
                    Check::IsAtom,
                    Check::Atom(AtomRestriction::Value(Cow::Borrowed(&[]))),
                ]))
            }
        }
        (Type::List(inner), Type::Pair(pair)) => {
            let first = compare_with_context(arena, ctx, inner, pair.first);
            let rest = compare_with_context(arena, ctx, lhs, pair.rest);

            match (first, rest) {
                (Comparison::Invalid, _) | (_, Comparison::Invalid) => Comparison::Invalid,
                (Comparison::Assign, Comparison::Assign) => Comparison::Check(Check::IsPair),
                (Comparison::Cast, Comparison::Cast) => Comparison::Check(Check::IsPair),
                (Comparison::Assign, Comparison::Cast) => Comparison::Check(Check::IsPair),
                (Comparison::Cast, Comparison::Assign) => Comparison::Check(Check::IsPair),
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

            match (first.clone(), rest.clone()) {
                (Comparison::Invalid, _) | (_, Comparison::Invalid) => Comparison::Invalid,
                (Comparison::Assign, Comparison::Assign) => Comparison::Assign,
                (Comparison::Cast, Comparison::Cast) => Comparison::Cast,
                (Comparison::Assign, Comparison::Cast) => Comparison::Cast,
                (Comparison::Cast, Comparison::Assign) => Comparison::Cast,
                (_, Comparison::Check(_)) => Comparison::Invalid,
                (Comparison::Check(first), Comparison::Assign | Comparison::Cast) => {
                    Comparison::Check(Check::Pair(Box::new(first), Box::new(Check::None)))
                }
            }
        }
        (Type::Any | Type::Generic, Type::List(_)) => Comparison::Invalid,
        (Type::Atom(lhs), Type::Atom(rhs)) => compare_atom(lhs, rhs),
        (Type::Pair(lhs), Type::Pair(rhs)) => compare_pair(arena, ctx, lhs, rhs),
        (Type::Pair(_), Type::Atom(_)) => Comparison::Invalid,
        (Type::Atom(_), Type::Pair(_)) => Comparison::Invalid,
        (Type::Alias(lhs), _) => compare_with_context(arena, ctx, lhs.inner, rhs),
        (_, Type::Alias(rhs)) => compare_with_context(arena, ctx, lhs, rhs.inner),
        (Type::Struct(lhs), _) => {
            // TODO: Require cast for structs
            compare_with_context(arena, ctx, lhs.inner, rhs)
        }
        (_, Type::Struct(rhs)) => compare_with_context(arena, ctx, lhs, rhs.inner),
        (Type::Function(lhs), Type::Function(rhs)) => compare_function(arena, ctx, lhs, rhs),
        (Type::Function(_), _) | (_, Type::Function(_)) => Comparison::Invalid,
        (Type::Union(lhs), _) => compare_from_union(arena, ctx, lhs, rhs),
        (_, Type::Union(rhs)) => compare_to_union(arena, ctx, lhs, rhs),
    }
}
