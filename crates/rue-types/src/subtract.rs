use id_arena::Arena;

use crate::{Alias, AtomRestriction, Pair, Struct, Type, TypeId, Union};

pub fn subtract(arena: &mut Arena<Type>, lhs_id: TypeId, rhs_id: TypeId) -> Option<TypeId> {
    match (arena[lhs_id].clone(), arena[rhs_id].clone()) {
        (Type::Apply(_), _) | (_, Type::Apply(_)) => unreachable!(),
        (Type::Unresolved, _) | (_, Type::Unresolved) => Some(lhs_id),
        (Type::Generic, _) => Some(lhs_id),
        (_, Type::Generic) => None,
        (Type::Ref(lhs), _) => subtract(arena, lhs, rhs_id),
        (_, Type::Ref(rhs)) => subtract(arena, lhs_id, rhs),
        (Type::Alias(lhs), _) => {
            let inner = subtract(arena, lhs.inner, rhs_id)?;
            Some(arena.alloc(Type::Alias(Alias { inner })))
        }
        (_, Type::Alias(rhs)) => {
            let inner = subtract(arena, lhs_id, rhs.inner)?;
            Some(arena.alloc(Type::Alias(Alias { inner })))
        }
        (Type::Struct(lhs), _) => {
            let inner = subtract(arena, lhs.inner, rhs_id)?;
            Some(arena.alloc(Type::Struct(Struct { inner, ..lhs })))
        }
        (_, Type::Struct(rhs)) => {
            let inner = subtract(arena, lhs_id, rhs.inner)?;
            Some(arena.alloc(Type::Struct(Struct { inner, ..rhs })))
        }
        (Type::Atom(lhs), Type::Atom(rhs)) => {
            let Some(lhs) = lhs.restriction else {
                return Some(lhs_id);
            };

            let rhs = rhs.restriction?;

            match (lhs, rhs) {
                (AtomRestriction::Length(lhs), AtomRestriction::Length(rhs)) => {
                    if lhs == rhs {
                        None
                    } else {
                        Some(lhs_id)
                    }
                }
                (AtomRestriction::Length(_), AtomRestriction::Value(_)) => Some(lhs_id),
                (AtomRestriction::Value(value), AtomRestriction::Length(length)) => {
                    if value.len() == length {
                        None
                    } else {
                        Some(lhs_id)
                    }
                }
                (AtomRestriction::Value(lhs), AtomRestriction::Value(rhs)) => {
                    if lhs == rhs {
                        None
                    } else {
                        Some(lhs_id)
                    }
                }
            }
        }
        (Type::Atom(_), Type::Pair(_)) => Some(lhs_id),
        (Type::Pair(_), Type::Atom(_)) => Some(lhs_id),
        (_, Type::Union(ty)) => {
            let mut result = lhs_id;

            for &id in &ty.types {
                result = subtract(arena, result, id)?;
            }

            Some(result)
        }
        (Type::Union(ty), _) => {
            let mut ids = Vec::new();

            for &id in &ty.types {
                let Some(inner) = subtract(arena, id, rhs_id) else {
                    continue;
                };

                ids.push(inner);
            }

            if ids.is_empty() {
                None
            } else if ids.len() == 1 {
                Some(ids[0])
            } else {
                Some(arena.alloc(Type::Union(Union::new(ids))))
            }
        }
        (Type::Pair(lhs), Type::Pair(rhs)) => {
            let first = subtract(arena, lhs.first, rhs.first);
            let rest = subtract(arena, lhs.rest, rhs.rest);

            match (first, rest) {
                (Some(first), Some(rest)) => {
                    let first_pair = arena.alloc(Type::Pair(Pair::new(first, lhs.rest)));
                    let rest_pair = arena.alloc(Type::Pair(Pair::new(lhs.first, rest)));
                    Some(arena.alloc(Type::Union(Union::new(vec![first_pair, rest_pair]))))
                }
                (Some(_), None) => Some(lhs_id),
                (None, Some(_)) => Some(lhs_id),
                (None, None) => None,
            }
        }
    }
}
