use std::collections::{HashMap, HashSet};

use id_arena::Arena;

use crate::{Type, TypeId};

/// Returns `true` if the type is part of a cycle of type declarations where one of the
/// declarations is referenced with generic arguments that aren't plain generic parameters.
///
/// Such a type (for example `type L<T> = nil | (T, L<(T, T)>);`) is non-regular, since each
/// level of recursion has new generic arguments, so expanding it never reaches a type that has
/// already been seen and it can't be represented as a finite (cyclic) type. Recursive references
/// that pass the generic parameters through, possibly reordered (such as `List<T>` or the
/// `Alt<B, A>` in `type Alt<A, B> = nil | (A, Alt<B, A>);`), only ever map the generic parameters
/// to the arguments of the outermost reference, so they expand to a finite type.
///
/// This must be checked before the type is substituted, since substituting a non-regular type
/// would never terminate.
pub fn is_non_regular(arena: &Arena<Type>, id: TypeId) -> bool {
    let mut references = HashMap::new();
    let mut stack = vec![id];

    while let Some(declaration) = stack.pop() {
        if references.contains_key(&declaration) {
            continue;
        }

        let inner = match &arena[declaration] {
            Type::Alias(alias) => alias.inner,
            Type::Struct(ty) => ty.inner,
            _ => unreachable!(),
        };

        let mut list = Vec::new();
        collect_references(arena, inner, &mut list, &mut HashSet::new());

        stack.extend(list.iter().map(|&(target, _)| target));
        references.insert(declaration, list);
    }

    let in_cycle: HashSet<TypeId> = references
        .keys()
        .copied()
        .filter(|&declaration| reaches(&references, declaration, id))
        .collect();

    in_cycle.iter().any(|declaration| {
        references[declaration]
            .iter()
            .any(|&(target, regular)| !regular && in_cycle.contains(&target))
    })
}

/// Collects the type declarations referenced by a declaration's inner type, along with whether
/// the generic arguments of each reference are all generic parameters.
fn collect_references(
    arena: &Arena<Type>,
    id: TypeId,
    references: &mut Vec<(TypeId, bool)>,
    visited: &mut HashSet<TypeId>,
) {
    if !visited.insert(id) {
        return;
    }

    match &arena[id] {
        Type::Alias(_) | Type::Struct(_) => references.push((id, true)),
        Type::Apply(apply) => {
            let inner = resolve_refs(arena, apply.inner);

            if matches!(arena[inner], Type::Alias(_) | Type::Struct(_)) {
                let regular = apply
                    .generics
                    .values()
                    .all(|&arg| matches!(arena[resolve_refs(arena, arg)], Type::Generic(_)));

                references.push((inner, regular));
            } else {
                collect_references(arena, inner, references, visited);
            }

            for &arg in apply.generics.values() {
                collect_references(arena, arg, references, visited);
            }
        }
        Type::Ref(id) => collect_references(arena, *id, references, visited),
        Type::Pair(pair) => {
            collect_references(arena, pair.first, references, visited);
            collect_references(arena, pair.rest, references, visited);
        }
        Type::Union(union) => {
            for &id in &union.types {
                collect_references(arena, id, references, visited);
            }
        }
        Type::Function(function) => {
            for &id in function.params.values() {
                collect_references(arena, id, references, visited);
            }

            collect_references(arena, function.ret, references, visited);
        }
        Type::Unresolved | Type::Generic(_) | Type::Atom(_) | Type::Never | Type::Any => {}
    }
}

fn resolve_refs(arena: &Arena<Type>, mut id: TypeId) -> TypeId {
    while let Type::Ref(inner) = arena[id] {
        id = inner;
    }

    id
}

fn reaches(references: &HashMap<TypeId, Vec<(TypeId, bool)>>, from: TypeId, to: TypeId) -> bool {
    let mut visited = HashSet::new();
    let mut stack = vec![from];

    while let Some(declaration) = stack.pop() {
        if !visited.insert(declaration) {
            continue;
        }

        for &(target, _) in &references[&declaration] {
            if target == to {
                return true;
            }

            stack.push(target);
        }
    }

    false
}
