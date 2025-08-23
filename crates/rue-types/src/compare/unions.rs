use std::ops::Not;

use id_arena::Arena;
use indexmap::{IndexSet, indexset};

use crate::{AtomRestriction, Check, Comparison, Type, TypeId, Union, compare};

pub(crate) fn compare_from_union(arena: &Arena<Type>, lhs: Union, rhs: TypeId) -> Comparison {
    if lhs.types.is_empty() {
        return Comparison::Invalid;
    }

    let mut result = None;

    for &id in &lhs.types {
        match compare(arena, id, rhs) {
            Comparison::Unresolved => {
                return Comparison::Unresolved;
            }
            Comparison::Assign => {
                if result.is_none() {
                    result = Some(Comparison::Assign);
                }
            }
            Comparison::Cast => {
                result = Some(Comparison::Cast);
            }
            Comparison::Check(_) | Comparison::Invalid => {
                result = None;
                break;
            }
        }
    }

    if let Some(result) = result {
        return result;
    }

    refine_union(
        arena,
        lhs.types.into_iter().enumerate().collect(),
        Union::new(vec![rhs]),
    )
    .0
    .map_or(Comparison::Invalid, Comparison::Check)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Shape {
    atom: bool,
    pair: bool,
}

impl Shape {
    pub const NONE: Self = Self {
        atom: false,
        pair: false,
    };

    pub const ATOM: Self = Self {
        atom: true,
        pair: false,
    };

    pub const PAIR: Self = Self {
        atom: false,
        pair: true,
    };
}

impl Not for Shape {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self {
            atom: !self.atom,
            pair: !self.pair,
        }
    }
}

fn shape_of(arena: &Arena<Type>, ty: Type) -> Shape {
    match ty {
        Type::Unresolved | Type::Apply(_) | Type::Generic => unreachable!(),
        Type::Alias(alias) => shape_of(arena, arena[alias.inner].clone()),
        Type::Struct(ty) => shape_of(arena, arena[ty.inner].clone()),
        Type::Atom(_) => Shape::ATOM,
        Type::Pair(_) => Shape::PAIR,
        Type::Union(ty) => {
            let mut shape = Shape::NONE;

            for &id in &ty.types {
                let inner = shape_of(arena, arena[id].clone());
                shape.atom |= inner.atom;
                shape.pair |= inner.pair;
            }

            shape
        }
    }
}

#[derive(Debug, Clone)]
enum AtomRestrictions {
    Unrestricted,
    Either(IndexSet<AtomRestriction>),
    NotAtom,
}

fn atom_restrictions_of(arena: &Arena<Type>, ty: Type) -> AtomRestrictions {
    match ty {
        Type::Unresolved | Type::Apply(_) | Type::Generic => unreachable!(),
        Type::Alias(alias) => atom_restrictions_of(arena, arena[alias.inner].clone()),
        Type::Struct(ty) => atom_restrictions_of(arena, arena[ty.inner].clone()),
        Type::Atom(atom) => atom
            .restriction
            .map_or(AtomRestrictions::Unrestricted, |restriction| {
                AtomRestrictions::Either(indexset![restriction])
            }),
        Type::Pair(_) => AtomRestrictions::NotAtom,
        Type::Union(ty) => {
            let mut restrictions = IndexSet::new();
            let mut has_atom = false;

            for &id in &ty.types {
                match atom_restrictions_of(arena, arena[id].clone()) {
                    AtomRestrictions::Unrestricted => return AtomRestrictions::Unrestricted,
                    AtomRestrictions::Either(inner) => {
                        for restriction in inner {
                            match &restriction {
                                AtomRestriction::Value(value) => {
                                    if restrictions.contains(&AtomRestriction::Length(value.len()))
                                    {
                                        continue;
                                    }
                                }
                                AtomRestriction::Length(length) => {
                                    restrictions.retain(|restriction| match restriction {
                                        AtomRestriction::Value(value) => value.len() != *length,
                                        AtomRestriction::Length(_) => true,
                                    });
                                }
                            }
                            restrictions.insert(restriction);
                        }
                        has_atom = true;
                    }
                    AtomRestrictions::NotAtom => {}
                }
            }

            if has_atom {
                AtomRestrictions::Either(restrictions)
            } else {
                AtomRestrictions::NotAtom
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct PairInfo {
    id: Option<TypeId>,
    first: TypeId,
    rest: TypeId,
}

fn pairs_of(arena: &Arena<Type>, id: Option<TypeId>, ty: Type) -> Vec<PairInfo> {
    match ty {
        Type::Unresolved | Type::Apply(_) | Type::Generic => unreachable!(),
        Type::Alias(alias) => pairs_of(arena, Some(alias.inner), arena[alias.inner].clone()),
        Type::Struct(ty) => pairs_of(arena, Some(ty.inner), arena[ty.inner].clone()),
        Type::Atom(_) => vec![],
        Type::Pair(pair) => vec![PairInfo {
            id,
            first: pair.first,
            rest: pair.rest,
        }],
        Type::Union(ty) => {
            let mut pairs = vec![];

            for &id in &ty.types {
                let inner = pairs_of(arena, Some(id), arena[id].clone());
                pairs.extend(inner);
            }

            pairs
        }
    }
}

fn refine_union(
    arena: &Arena<Type>,
    mut lhs: Vec<(usize, TypeId)>,
    rhs: Union,
) -> (Option<Check>, Vec<(usize, TypeId)>) {
    let target_shape = shape_of(arena, Type::Union(rhs.clone()));

    if !target_shape.atom && !target_shape.pair {
        return (None, lhs);
    }

    let mut shape_check = None;

    if !(target_shape.atom && target_shape.pair) {
        let mut check = false;

        lhs.retain(|&(_, id)| {
            if shape_of(arena, arena[id].clone()) != target_shape {
                check = true;
                return false;
            }
            true
        });

        if check {
            shape_check = Some(if target_shape.atom {
                Check::IsAtom
            } else {
                Check::IsPair
            });
        }
    }

    if lhs.is_empty() {
        return (None, lhs);
    }

    let mut atom_restriction_check = None;

    if target_shape.atom
        && let AtomRestrictions::Either(target_restrictions) =
            atom_restrictions_of(arena, Type::Union(rhs.clone()))
    {
        let mut overlap = IndexSet::new();
        let mut exceeds_overlap = false;
        let mut unrestricted = false;

        lhs.retain(
            |&(_, id)| match atom_restrictions_of(arena, arena[id].clone()) {
                AtomRestrictions::Unrestricted => {
                    unrestricted = true;
                    true
                }
                AtomRestrictions::Either(restrictions) => {
                    let mut has_overlap = false;

                    for restriction in restrictions {
                        if target_restrictions.contains(&restriction) {
                            overlap.insert(restriction);
                            has_overlap = true;
                        } else if let AtomRestriction::Value(value) = restriction
                            && target_restrictions.contains(&AtomRestriction::Length(value.len()))
                        {
                            overlap.insert(AtomRestriction::Length(value.len()));
                            has_overlap = true;
                        } else {
                            exceeds_overlap = true;
                        }
                    }

                    has_overlap
                }
                AtomRestrictions::NotAtom => true,
            },
        );

        if unrestricted {
            atom_restriction_check = Some(if target_restrictions.len() == 1 {
                Check::Atom(target_restrictions.into_iter().next().unwrap())
            } else {
                Check::Or(target_restrictions.into_iter().map(Check::Atom).collect())
            });
        } else if overlap.is_empty() {
            return (None, lhs);
        } else if exceeds_overlap {
            atom_restriction_check = Some(if overlap.len() == 1 {
                Check::Atom(overlap.into_iter().next().unwrap())
            } else {
                Check::Or(overlap.into_iter().map(Check::Atom).collect())
            });
        }
    }

    if lhs.is_empty() {
        return (None, lhs);
    }

    let mut pair_check = None;

    if target_shape.pair {
        let target_pairs = pairs_of(arena, None, Type::Union(rhs.clone()));

        let target_firsts = target_pairs
            .iter()
            .map(|pair| pair.first)
            .collect::<Vec<_>>();

        let target_rests = target_pairs
            .iter()
            .map(|pair| pair.rest)
            .collect::<Vec<_>>();

        let mut pairs = Vec::new();

        for &(_, id) in &lhs {
            let inner = pairs_of(arena, Some(id), arena[id].clone());
            pairs.extend(inner);
        }

        let (first_check, first_remaining) = refine_union(
            arena,
            pairs.iter().map(|pair| pair.first).enumerate().collect(),
            Union::new(target_firsts),
        );

        let Some(first_check) = first_check else {
            return (None, lhs);
        };

        let rest_needed = if first_check == Check::None {
            true
        } else {
            let pair_ids: Vec<TypeId> = first_remaining
                .into_iter()
                .map(|(i, _)| pairs[i].id.unwrap())
                .collect();

            let mut rest_needed = false;

            for pair_id in pair_ids {
                match compare_to_union(arena, pair_id, rhs.clone()) {
                    Comparison::Unresolved | Comparison::Assign | Comparison::Cast => {}
                    Comparison::Check(_) | Comparison::Invalid => {
                        rest_needed = true;
                        break;
                    }
                }
            }

            rest_needed
        };

        let rest_check = if rest_needed {
            let Some(rest_check) = refine_union(
                arena,
                pairs.iter().map(|pair| pair.rest).enumerate().collect(),
                Union::new(target_rests),
            )
            .0
            else {
                return (None, lhs);
            };

            rest_check
        } else {
            Check::None
        };

        pair_check = match (&first_check, &rest_check) {
            (Check::None, Check::None) => None,
            _ => Some(Check::Pair(Box::new(first_check), Box::new(rest_check))),
        };
    }

    let check = match (shape_check, atom_restriction_check, pair_check) {
        (Some(shape_check), Some(atom_restriction_check), None) => {
            Check::And(vec![shape_check, atom_restriction_check])
        }
        (Some(shape_check), None, Some(pair_check)) => Check::And(vec![shape_check, pair_check]),
        (Some(shape_check), None, None) => shape_check,
        (None, Some(atom_restriction_check), None) => {
            if target_shape.pair {
                Check::Or(vec![Check::IsPair, atom_restriction_check])
            } else {
                atom_restriction_check
            }
        }
        (None, None, Some(pair_check)) => {
            if target_shape.atom {
                Check::Or(vec![Check::IsAtom, pair_check])
            } else {
                pair_check
            }
        }
        (_, Some(atom_restriction_check), Some(pair_check)) => Check::Or(vec![
            Check::And(vec![Check::IsAtom, atom_restriction_check]),
            Check::And(vec![Check::IsPair, pair_check]),
        ]),
        (None, None, None) => Check::None,
    };

    (Some(check), lhs)
}

pub(crate) fn compare_to_union(arena: &Arena<Type>, lhs: TypeId, rhs: Union) -> Comparison {
    let mut result = Comparison::Invalid;

    for &id in &rhs.types {
        let comparison = compare(arena, lhs, id);

        match (&mut result, &comparison) {
            (Comparison::Unresolved, _) | (_, Comparison::Unresolved) => {
                return Comparison::Unresolved;
            }
            (Comparison::Invalid, _) => {
                result = comparison;
            }
            (_, Comparison::Assign) => {
                result = Comparison::Assign;
            }
            (Comparison::Assign | Comparison::Cast, Comparison::Cast | Comparison::Check(_)) => {}
            (Comparison::Check(_), Comparison::Cast) => {
                result = Comparison::Cast;
            }
            (Comparison::Check(old), Comparison::Check(new)) => {
                *old = Check::Or(vec![old.clone(), new.clone()]);
            }
            (_, Comparison::Invalid) => {}
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use crate::{Atom, AtomKind, Pair};

    use super::*;

    #[test]
    fn test_union_assign() {
        let mut arena = Arena::new();
        let bytes_32 = arena.alloc(Type::Atom(Atom::BYTES_32));
        let nil = arena.alloc(Type::Atom(Atom::NIL));
        let lhs = arena.alloc(Type::Union(Union::new(vec![bytes_32, nil])));
        let rhs = arena.alloc(Type::Atom(Atom::BYTES));
        assert_eq!(compare(&arena, lhs, rhs), Comparison::Assign);
    }

    #[test]
    fn test_union_cast() {
        let mut arena = Arena::new();
        let int = arena.alloc(Type::Atom(Atom::INT));
        let nil = arena.alloc(Type::Atom(Atom::NIL));
        let lhs = arena.alloc(Type::Union(Union::new(vec![int, nil])));
        let rhs = arena.alloc(Type::Atom(Atom::BYTES));
        assert_eq!(compare(&arena, lhs, rhs), Comparison::Cast);
    }

    #[test]
    fn test_union_shape_check_atom() {
        let mut arena = Arena::new();
        let atom = arena.alloc(Type::Atom(Atom::INT));
        let pair = arena.alloc(Type::Pair(Pair::new(atom, atom)));
        let lhs = arena.alloc(Type::Union(Union::new(vec![atom, pair])));
        assert_eq!(compare(&arena, lhs, atom), Comparison::Check(Check::IsAtom));
    }

    #[test]
    fn test_union_shape_check_pair() {
        let mut arena = Arena::new();
        let int = arena.alloc(Type::Atom(Atom::INT));
        let pair = arena.alloc(Type::Pair(Pair::new(int, int)));
        let lhs = arena.alloc(Type::Union(Union::new(vec![int, pair])));
        assert_eq!(compare(&arena, lhs, pair), Comparison::Check(Check::IsPair));
    }

    #[test]
    fn test_union_shape_restrict_length() {
        let mut arena = Arena::new();
        let int = arena.alloc(Type::Atom(Atom::INT));
        let pair = arena.alloc(Type::Pair(Pair::new(int, int)));
        let lhs = arena.alloc(Type::Union(Union::new(vec![int, pair])));
        let rhs = arena.alloc(Type::Atom(Atom::BYTES_32));
        assert_eq!(
            compare(&arena, lhs, rhs),
            Comparison::Check(Check::And(vec![
                Check::IsAtom,
                Check::Atom(AtomRestriction::Length(32))
            ]))
        );
    }

    #[test]
    fn test_union_shape_restrict_value() {
        let mut arena = Arena::new();
        let int = arena.alloc(Type::Atom(Atom::INT));
        let pair = arena.alloc(Type::Pair(Pair::new(int, int)));
        let lhs = arena.alloc(Type::Union(Union::new(vec![int, pair])));
        let rhs = arena.alloc(Type::Atom(Atom::NIL));
        assert_eq!(
            compare(&arena, lhs, rhs),
            Comparison::Check(Check::And(vec![
                Check::IsAtom,
                Check::Atom(AtomRestriction::Value(Cow::Borrowed(&[])))
            ]))
        );
    }

    #[test]
    fn test_union_restrict_length() {
        let mut arena = Arena::new();
        let lhs = arena.alloc(Type::Atom(Atom::INT));
        let rhs = arena.alloc(Type::Atom(Atom::BYTES_32));
        assert_eq!(
            compare(&arena, lhs, rhs),
            Comparison::Check(Check::Atom(AtomRestriction::Length(32)))
        );
    }

    #[test]
    fn test_union_restrict_value() {
        let mut arena = Arena::new();
        let lhs = arena.alloc(Type::Atom(Atom::INT));
        let rhs = arena.alloc(Type::Atom(Atom::NIL));
        assert_eq!(
            compare(&arena, lhs, rhs),
            Comparison::Check(Check::Atom(AtomRestriction::Value(Cow::Borrowed(&[]))))
        );
    }

    #[test]
    fn test_union_unnecessary_restrict() {
        let mut arena = Arena::new();
        let nil = arena.alloc(Type::Atom(Atom::NIL));
        assert_eq!(compare(&arena, nil, nil), Comparison::Assign);
    }

    #[test]
    fn test_union_restrict_pair_first() {
        let mut arena = Arena::new();
        let int_51 = arena.alloc(Type::Atom(Atom::new(
            AtomKind::Int,
            Some(AtomRestriction::Value(Cow::Borrowed(&[51]))),
        )));
        let int_52 = arena.alloc(Type::Atom(Atom::new(
            AtomKind::Int,
            Some(AtomRestriction::Value(Cow::Borrowed(&[52]))),
        )));
        let nil = arena.alloc(Type::Atom(Atom::NIL));
        let create_coin = arena.alloc(Type::Pair(Pair::new(int_51, nil)));
        let reserve_fee = arena.alloc(Type::Pair(Pair::new(int_52, nil)));
        let lhs = arena.alloc(Type::Union(Union::new(vec![create_coin, reserve_fee])));
        assert_eq!(
            compare(&arena, lhs, create_coin),
            Comparison::Check(Check::Pair(
                Box::new(Check::Atom(AtomRestriction::Value(Cow::Borrowed(&[51])))),
                Box::new(Check::None)
            ))
        );
        assert_eq!(
            compare(&arena, lhs, reserve_fee),
            Comparison::Check(Check::Pair(
                Box::new(Check::Atom(AtomRestriction::Value(Cow::Borrowed(&[52])))),
                Box::new(Check::None)
            ))
        );
    }

    #[test]
    fn test_union_restrict_pair_rest() {
        let mut arena = Arena::new();
        let true_bool = arena.alloc(Type::Atom(Atom::TRUE));
        let false_bool = arena.alloc(Type::Atom(Atom::FALSE));
        let nil = arena.alloc(Type::Atom(Atom::NIL));
        let true_pair = arena.alloc(Type::Pair(Pair::new(nil, true_bool)));
        let false_pair = arena.alloc(Type::Pair(Pair::new(nil, false_bool)));
        let lhs = arena.alloc(Type::Union(Union::new(vec![true_pair, false_pair])));
        assert_eq!(
            compare(&arena, lhs, true_pair),
            Comparison::Check(Check::Pair(
                Box::new(Check::None),
                Box::new(Check::Atom(AtomRestriction::Value(Cow::Borrowed(&[1])))),
            ))
        );
        assert_eq!(
            compare(&arena, lhs, false_pair),
            Comparison::Check(Check::Pair(
                Box::new(Check::None),
                Box::new(Check::Atom(AtomRestriction::Value(Cow::Borrowed(&[])))),
            ))
        );
    }

    #[test]
    fn test_union_restrict_pair_both() {
        let mut arena = Arena::new();
        let true_bool = arena.alloc(Type::Atom(Atom::TRUE));
        let false_bool = arena.alloc(Type::Atom(Atom::FALSE));
        let pair_1 = arena.alloc(Type::Pair(Pair::new(false_bool, true_bool)));
        let pair_2 = arena.alloc(Type::Pair(Pair::new(true_bool, false_bool)));
        let lhs = arena.alloc(Type::Union(Union::new(vec![pair_1, pair_2])));
        assert_eq!(
            compare(&arena, lhs, pair_1),
            Comparison::Check(Check::Pair(
                Box::new(Check::Atom(AtomRestriction::Value(Cow::Borrowed(&[])))),
                Box::new(Check::None),
            ))
        );
        assert_eq!(
            compare(&arena, lhs, pair_2),
            Comparison::Check(Check::Pair(
                Box::new(Check::Atom(AtomRestriction::Value(Cow::Borrowed(&[1])))),
                Box::new(Check::None),
            ))
        );
    }

    #[test]
    fn test_assign_union() {
        let mut arena = Arena::new();
        let nil = arena.alloc(Type::Atom(Atom::NIL));
        let bytes_32 = arena.alloc(Type::Atom(Atom::BYTES_32));
        let rhs = arena.alloc(Type::Union(Union::new(vec![nil, bytes_32])));
        assert_eq!(compare(&arena, nil, rhs), Comparison::Assign);
        assert_eq!(compare(&arena, bytes_32, rhs), Comparison::Assign);
    }

    #[test]
    fn test_cast_union() {
        let mut arena = Arena::new();
        let false_value = arena.alloc(Type::Atom(Atom::FALSE));
        let nil = arena.alloc(Type::Atom(Atom::NIL));
        let bytes_32 = arena.alloc(Type::Atom(Atom::BYTES_32));
        let rhs = arena.alloc(Type::Union(Union::new(vec![nil, bytes_32])));
        assert_eq!(compare(&arena, false_value, rhs), Comparison::Cast);
    }

    #[test]
    fn test_check_union() {
        let mut arena = Arena::new();
        let int = arena.alloc(Type::Atom(Atom::INT));
        let nil = arena.alloc(Type::Atom(Atom::NIL));
        let bytes_32 = arena.alloc(Type::Atom(Atom::BYTES_32));
        let rhs = arena.alloc(Type::Union(Union::new(vec![nil, bytes_32])));
        assert_eq!(
            compare(&arena, int, rhs),
            Comparison::Check(Check::Or(vec![
                Check::Atom(AtomRestriction::Value(Cow::Borrowed(&[]))),
                Check::Atom(AtomRestriction::Length(32)),
            ]))
        );
    }
}
