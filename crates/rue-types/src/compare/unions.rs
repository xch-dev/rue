use id_arena::Arena;

use crate::{Comparison, Type, TypeId, Union, compare};

pub(crate) fn compare_union(arena: &Arena<Type>, lhs: Union, rhs: TypeId) -> Comparison {
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
            _ => {
                result = None;
                break;
            }
        }
    }

    if let Some(result) = result {
        return result;
    }

    Comparison::Invalid
}

#[cfg(test)]
mod tests {
    use crate::Atom;

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
}
