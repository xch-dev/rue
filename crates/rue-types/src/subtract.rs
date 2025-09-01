use id_arena::Arena;
use indexmap::IndexMap;
use log::debug;

use crate::{
    Alias, AtomRestriction, Pair, Struct, Type, TypeId, Union, stringify_impl, substitute,
};

pub fn subtract(arena: &mut Arena<Type>, lhs_id: TypeId, rhs_id: TypeId) -> TypeId {
    let lhs_id = substitute(arena, lhs_id);
    let rhs_id = substitute(arena, rhs_id);
    let lhs_name = stringify_impl(arena, lhs_id, &mut IndexMap::new());
    let rhs_name = stringify_impl(arena, rhs_id, &mut IndexMap::new());
    debug!("Subtracting {lhs_name} from {rhs_name}");
    let result = subtract_impl(arena, lhs_id, rhs_id, &mut IndexMap::new());
    let new_name = stringify_impl(arena, result, &mut IndexMap::new());
    debug!("Subtraction from {lhs_name} to {rhs_name} yielded {new_name}");
    result
}

fn subtract_impl(
    arena: &mut Arena<Type>,
    lhs_id: TypeId,
    rhs_id: TypeId,
    cache: &mut IndexMap<(TypeId, TypeId), TypeId>,
) -> TypeId {
    if let Some(id) = cache.get(&(lhs_id, rhs_id)) {
        return *id;
    }

    let new_id = arena.alloc(Type::Unresolved);
    cache.insert((lhs_id, rhs_id), new_id);

    let result = match (arena[lhs_id].clone(), arena[rhs_id].clone()) {
        (Type::Apply(_), _) | (_, Type::Apply(_)) => unreachable!(),
        (Type::Ref(lhs), _) => subtract_impl(arena, lhs, rhs_id, cache),
        (_, Type::Ref(rhs)) => subtract_impl(arena, lhs_id, rhs, cache),
        (Type::Unresolved, _) | (_, Type::Unresolved) => lhs_id,
        (Type::Function(_), _) => lhs_id,
        (Type::Never, _) => lhs_id,
        (_, Type::Never) => lhs_id,
        (_, Type::Generic) => arena.alloc(Type::Never),
        (_, Type::Function(_)) => arena.alloc(Type::Never),
        (Type::Generic, _) => lhs_id,
        (Type::Alias(lhs), _) => {
            let inner = subtract_impl(arena, lhs.inner, rhs_id, cache);

            if inner == lhs.inner {
                lhs_id
            } else if matches!(arena[inner].clone(), Type::Never) {
                arena.alloc(Type::Never)
            } else {
                arena.alloc(Type::Alias(Alias { inner, ..lhs }))
            }
        }
        (_, Type::Alias(rhs)) => {
            let inner = subtract_impl(arena, lhs_id, rhs.inner, cache);

            if inner == lhs_id {
                lhs_id
            } else if matches!(arena[inner].clone(), Type::Never) {
                arena.alloc(Type::Never)
            } else {
                arena.alloc(Type::Alias(Alias { inner, ..rhs }))
            }
        }
        (Type::Struct(lhs), _) => {
            let inner = subtract_impl(arena, lhs.inner, rhs_id, cache);

            if inner == lhs.inner {
                lhs_id
            } else if matches!(arena[inner].clone(), Type::Never) {
                arena.alloc(Type::Never)
            } else {
                arena.alloc(Type::Struct(Struct { inner, ..lhs }))
            }
        }
        (_, Type::Struct(rhs)) => {
            let inner = subtract_impl(arena, lhs_id, rhs.inner, cache);

            if inner == lhs_id {
                lhs_id
            } else if matches!(arena[inner].clone(), Type::Never) {
                arena.alloc(Type::Never)
            } else {
                arena.alloc(Type::Struct(Struct { inner, ..rhs }))
            }
        }
        (Type::Atom(lhs), Type::Atom(rhs)) => 'result: {
            let Some(rhs) = rhs.restriction else {
                break 'result arena.alloc(Type::Never);
            };

            let Some(lhs) = lhs.restriction else {
                break 'result lhs_id;
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
                result = subtract_impl(arena, result, id, cache);
            }

            result
        }
        (Type::Union(ty), _) => {
            let mut ids = Vec::new();
            let mut changed = false;

            for &old_id in &ty.types {
                let new_id = subtract_impl(arena, old_id, rhs_id, cache);

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
                lhs_id
            } else if ids.is_empty() {
                arena.alloc(Type::Never)
            } else if ids.len() == 1 {
                ids[0]
            } else {
                arena.alloc(Type::Union(Union::new(ids)))
            }
        }
        (Type::Pair(lhs), Type::Pair(rhs)) => {
            // TODO: Add other variants?

            let first = subtract_impl(arena, lhs.first, rhs.first, cache);
            let rest = subtract_impl(arena, lhs.rest, rhs.rest, cache);

            match (arena[first].clone(), arena[rest].clone()) {
                (Type::Never, Type::Never) => arena.alloc(Type::Never),
                (Type::Never, _) => arena.alloc(Type::Pair(Pair::new(lhs.first, rest))),
                (_, Type::Never) => arena.alloc(Type::Pair(Pair::new(first, lhs.rest))),
                _ => lhs_id,
            }
        }
    };

    cache.pop().unwrap();

    *arena.get_mut(new_id).unwrap() = Type::Ref(result);

    result
}
