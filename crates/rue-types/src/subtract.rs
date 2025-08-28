use id_arena::Arena;

use crate::{Alias, Atom, AtomRestriction, Pair, Struct, Type, TypeId, Union, substitute};

pub fn subtract(arena: &mut Arena<Type>, lhs_id: TypeId, rhs_id: TypeId) -> TypeId {
    let lhs_id = substitute(arena, lhs_id);
    let rhs_id = substitute(arena, rhs_id);
    subtract_impl(arena, lhs_id, rhs_id)
}

fn subtract_impl(arena: &mut Arena<Type>, lhs_id: TypeId, rhs_id: TypeId) -> TypeId {
    match (arena[lhs_id].clone(), arena[rhs_id].clone()) {
        (Type::Apply(_), _) | (_, Type::Apply(_)) => unreachable!(),
        (Type::Unresolved, _) | (_, Type::Unresolved) => lhs_id,
        (Type::Function(_), _) | (_, Type::Function(_)) => lhs_id,
        (Type::Never, _) => lhs_id,
        (_, Type::Never) => lhs_id,
        (_, Type::Generic) => arena.alloc(Type::Never),
        (_, Type::Any) => arena.alloc(Type::Never),
        (Type::Generic, _) => lhs_id,
        (Type::Any, Type::List(_)) => lhs_id,
        (Type::Any, Type::Atom(atom)) => {
            if atom.restriction.is_none() {
                return arena.alloc(Type::Pair(Pair::new(lhs_id, lhs_id)));
            }
            lhs_id
        }
        (Type::Any, Type::Pair(pair)) => {
            let first = subtract_impl(arena, lhs_id, pair.first);
            let rest = subtract_impl(arena, lhs_id, pair.rest);

            if first == lhs_id && rest == lhs_id {
                return lhs_id;
            }

            let bytes = arena.alloc(Type::Atom(Atom::BYTES));

            match (arena[first].clone(), arena[rest].clone()) {
                (Type::Never, Type::Never) => bytes,
                _ => {
                    let pair = arena.alloc(Type::Pair(Pair::new(first, rest)));
                    arena.alloc(Type::Union(Union::new(vec![bytes, pair])))
                }
            }
        }
        (Type::List(inner), Type::Pair(pair)) => {
            let first = subtract_impl(arena, inner, pair.first);
            let rest = subtract_impl(arena, lhs_id, pair.rest);

            if first == inner && rest == lhs_id {
                return lhs_id;
            }

            let nil = arena.alloc(Type::Atom(Atom::NIL));

            match (arena[first].clone(), arena[rest].clone()) {
                (Type::Never, Type::Never) => nil,
                _ => {
                    let pair = arena.alloc(Type::Pair(Pair::new(first, rest)));
                    arena.alloc(Type::Union(Union::new(vec![nil, pair])))
                }
            }
        }
        (Type::Pair(pair), Type::List(inner)) => {
            let first = subtract_impl(arena, pair.first, inner);
            let rest = subtract_impl(arena, pair.rest, rhs_id);

            if first == pair.first && rest == pair.rest {
                return lhs_id;
            }

            match (arena[first].clone(), arena[rest].clone()) {
                (Type::Never, Type::Never) => arena.alloc(Type::Never),
                _ => arena.alloc(Type::Pair(Pair::new(first, rest))),
            }
        }
        (Type::List(inner), Type::Atom(atom)) => {
            let Some(restriction) = atom.restriction else {
                return arena.alloc(Type::Pair(Pair::new(inner, lhs_id)));
            };

            match restriction {
                AtomRestriction::Length(length) => {
                    if length == 0 {
                        arena.alloc(Type::Pair(Pair::new(inner, lhs_id)))
                    } else {
                        lhs_id
                    }
                }
                AtomRestriction::Value(value) => {
                    if value.is_empty() {
                        arena.alloc(Type::Pair(Pair::new(inner, lhs_id)))
                    } else {
                        lhs_id
                    }
                }
            }
        }
        (Type::Atom(atom), Type::List(_)) => {
            let Some(restriction) = atom.restriction else {
                return lhs_id;
            };

            match restriction {
                AtomRestriction::Length(length) => {
                    if length == 0 {
                        arena.alloc(Type::Never)
                    } else {
                        lhs_id
                    }
                }
                AtomRestriction::Value(value) => {
                    if value.is_empty() {
                        arena.alloc(Type::Never)
                    } else {
                        lhs_id
                    }
                }
            }
        }
        (Type::List(lhs), Type::List(rhs)) => {
            let inner = subtract_impl(arena, lhs, rhs);

            if inner == lhs {
                return lhs_id;
            }

            if matches!(arena[inner].clone(), Type::Never) {
                return arena.alloc(Type::Never);
            }

            arena.alloc(Type::List(inner))
        }
        (Type::Alias(lhs), _) => {
            let inner = subtract_impl(arena, lhs.inner, rhs_id);

            if inner == lhs.inner {
                return lhs_id;
            }

            if matches!(arena[inner].clone(), Type::Never) {
                return arena.alloc(Type::Never);
            }

            arena.alloc(Type::Alias(Alias { inner, ..lhs }))
        }
        (_, Type::Alias(rhs)) => {
            let inner = subtract_impl(arena, lhs_id, rhs.inner);

            if inner == lhs_id {
                return lhs_id;
            }

            if matches!(arena[inner].clone(), Type::Never) {
                return arena.alloc(Type::Never);
            }

            arena.alloc(Type::Alias(Alias { inner, ..rhs }))
        }
        (Type::Struct(lhs), _) => {
            let inner = subtract_impl(arena, lhs.inner, rhs_id);

            if inner == lhs.inner {
                return lhs_id;
            }

            if matches!(arena[inner].clone(), Type::Never) {
                return arena.alloc(Type::Never);
            }

            arena.alloc(Type::Struct(Struct { inner, ..lhs }))
        }
        (_, Type::Struct(rhs)) => {
            let inner = subtract_impl(arena, lhs_id, rhs.inner);

            if inner == lhs_id {
                return lhs_id;
            }

            if matches!(arena[inner].clone(), Type::Never) {
                return arena.alloc(Type::Never);
            }

            arena.alloc(Type::Struct(Struct { inner, ..rhs }))
        }
        (Type::Atom(lhs), Type::Atom(rhs)) => {
            let Some(rhs) = rhs.restriction else {
                return arena.alloc(Type::Never);
            };

            let Some(lhs) = lhs.restriction else {
                return lhs_id;
            };

            match (lhs, rhs) {
                (AtomRestriction::Length(lhs), AtomRestriction::Length(rhs)) => {
                    if lhs == rhs {
                        arena.alloc(Type::Never)
                    } else {
                        lhs_id
                    }
                }
                (AtomRestriction::Length(_), AtomRestriction::Value(_)) => lhs_id,
                (AtomRestriction::Value(value), AtomRestriction::Length(length)) => {
                    if value.len() == length {
                        arena.alloc(Type::Never)
                    } else {
                        lhs_id
                    }
                }
                (AtomRestriction::Value(lhs), AtomRestriction::Value(rhs)) => {
                    if lhs == rhs {
                        arena.alloc(Type::Never)
                    } else {
                        lhs_id
                    }
                }
            }
        }
        (Type::Atom(_), Type::Pair(_)) => lhs_id,
        (Type::Pair(_), Type::Atom(_)) => lhs_id,
        (_, Type::Union(ty)) => {
            let mut result = lhs_id;

            for &id in &ty.types {
                result = subtract_impl(arena, result, id);
            }

            result
        }
        (Type::Union(ty), _) => {
            let mut ids = Vec::new();
            let mut changed = false;

            for &old_id in &ty.types {
                let new_id = subtract_impl(arena, old_id, rhs_id);

                if new_id == old_id {
                    ids.push(old_id);
                    continue;
                }

                changed = true;

                if matches!(arena[new_id].clone(), Type::Never) {
                    continue;
                }

                ids.push(new_id);
            }

            if !changed {
                return lhs_id;
            }

            if ids.is_empty() {
                arena.alloc(Type::Never)
            } else if ids.len() == 1 {
                ids[0]
            } else {
                arena.alloc(Type::Union(Union::new(ids)))
            }
        }
        (Type::Pair(lhs), Type::Pair(rhs)) => {
            // TODO: Add other variants?

            let first = subtract_impl(arena, lhs.first, rhs.first);
            let rest = subtract_impl(arena, lhs.rest, rhs.rest);

            match (arena[first].clone(), arena[rest].clone()) {
                (Type::Never, Type::Never) => arena.alloc(Type::Never),
                (Type::Never, _) => arena.alloc(Type::Pair(Pair::new(lhs.first, rest))),
                (_, Type::Never) => arena.alloc(Type::Pair(Pair::new(first, lhs.rest))),
                _ => lhs_id,
            }
        }
    }
}
