use id_arena::Arena;

use crate::{Alias, AtomRestriction, Pair, Struct, Type, TypeId, Union, substitute};

pub enum Subtraction {
    Old,
    New(TypeId),
    Empty,
}

impl Subtraction {
    pub fn map<F>(self, f: F) -> Subtraction
    where
        F: FnOnce(TypeId) -> TypeId,
    {
        match self {
            Subtraction::Old => Subtraction::Old,
            Subtraction::New(id) => Subtraction::New(f(id)),
            Subtraction::Empty => Subtraction::Empty,
        }
    }
}

pub fn subtract(arena: &mut Arena<Type>, lhs_id: TypeId, rhs_id: TypeId) -> Subtraction {
    let lhs_id = substitute(arena, lhs_id);
    let rhs_id = substitute(arena, rhs_id);
    subtract_impl(arena, lhs_id, rhs_id)
}

fn subtract_impl(arena: &mut Arena<Type>, lhs_id: TypeId, rhs_id: TypeId) -> Subtraction {
    match (arena[lhs_id].clone(), arena[rhs_id].clone()) {
        (Type::Apply(_), _) | (_, Type::Apply(_)) => unreachable!(),
        (Type::Unresolved, _) | (_, Type::Unresolved) => Subtraction::Old,
        (Type::Function(_), _) | (_, Type::Function(_)) => Subtraction::Old,
        (Type::Generic, _) => Subtraction::Old,
        (_, Type::Generic) => Subtraction::Empty,
        (Type::Ref(lhs), _) => subtract_impl(arena, lhs, rhs_id),
        (_, Type::Ref(rhs)) => subtract_impl(arena, lhs_id, rhs),
        (Type::Alias(lhs), _) => subtract_impl(arena, lhs.inner, rhs_id)
            .map(|inner| arena.alloc(Type::Alias(Alias { inner, ..lhs }))),
        (_, Type::Alias(rhs)) => subtract_impl(arena, lhs_id, rhs.inner)
            .map(|inner| arena.alloc(Type::Alias(Alias { inner, ..rhs }))),
        (Type::Struct(lhs), _) => subtract_impl(arena, lhs.inner, rhs_id)
            .map(|inner| arena.alloc(Type::Struct(Struct { inner, ..lhs }))),
        (_, Type::Struct(rhs)) => subtract_impl(arena, lhs_id, rhs.inner)
            .map(|inner| arena.alloc(Type::Struct(Struct { inner, ..rhs }))),
        (Type::Atom(lhs), Type::Atom(rhs)) => {
            let Some(rhs) = rhs.restriction else {
                return Subtraction::Empty;
            };

            let Some(lhs) = lhs.restriction else {
                return Subtraction::Old;
            };

            match (lhs, rhs) {
                (AtomRestriction::Length(lhs), AtomRestriction::Length(rhs)) => {
                    if lhs == rhs {
                        Subtraction::Empty
                    } else {
                        Subtraction::Old
                    }
                }
                (AtomRestriction::Length(_), AtomRestriction::Value(_)) => Subtraction::Old,
                (AtomRestriction::Value(value), AtomRestriction::Length(length)) => {
                    if value.len() == length {
                        Subtraction::Empty
                    } else {
                        Subtraction::Old
                    }
                }
                (AtomRestriction::Value(lhs), AtomRestriction::Value(rhs)) => {
                    if lhs == rhs {
                        Subtraction::Empty
                    } else {
                        Subtraction::Old
                    }
                }
            }
        }
        (Type::Atom(_), Type::Pair(_)) => Subtraction::Old,
        (Type::Pair(_), Type::Atom(_)) => Subtraction::Old,
        (_, Type::Union(ty)) => {
            let mut result = lhs_id;
            let mut is_new = false;

            for &id in &ty.types {
                match subtract_impl(arena, result, id) {
                    Subtraction::Empty => return Subtraction::Empty,
                    Subtraction::Old => {}
                    Subtraction::New(id) => {
                        result = id;
                        is_new = true;
                    }
                }
            }

            if is_new {
                Subtraction::New(result)
            } else {
                Subtraction::Old
            }
        }
        (Type::Union(ty), _) => {
            let mut ids = Vec::new();

            for &id in &ty.types {
                match subtract_impl(arena, id, rhs_id) {
                    Subtraction::Empty => {}
                    Subtraction::Old => ids.push(id),
                    Subtraction::New(id) => ids.push(id),
                }
            }

            if ids.is_empty() {
                Subtraction::Empty
            } else if ids.len() == 1 {
                Subtraction::New(ids[0])
            } else {
                Subtraction::New(arena.alloc(Type::Union(Union::new(ids))))
            }
        }
        (Type::Pair(lhs), Type::Pair(rhs)) => {
            let first = subtract_impl(arena, lhs.first, rhs.first);
            let rest = subtract_impl(arena, lhs.rest, rhs.rest);

            match (first, rest) {
                (Subtraction::New(first), Subtraction::New(rest)) => {
                    let first_pair = arena.alloc(Type::Pair(Pair::new(first, lhs.rest)));
                    let rest_pair = arena.alloc(Type::Pair(Pair::new(lhs.first, rest)));
                    Subtraction::New(
                        arena.alloc(Type::Union(Union::new(vec![first_pair, rest_pair]))),
                    )
                }
                (Subtraction::Old, Subtraction::Old) => Subtraction::Old,
                (Subtraction::New(first), Subtraction::Old) => {
                    Subtraction::New(arena.alloc(Type::Pair(Pair::new(first, lhs.rest))))
                }
                (Subtraction::Old, Subtraction::New(rest)) => {
                    Subtraction::New(arena.alloc(Type::Pair(Pair::new(lhs.first, rest))))
                }
                (Subtraction::New(_) | Subtraction::Old, Subtraction::Empty) => Subtraction::Old,
                (Subtraction::Empty, Subtraction::New(_) | Subtraction::Old) => Subtraction::Old,
                (Subtraction::Empty, Subtraction::Empty) => Subtraction::Empty,
            }
        }
    }
}
