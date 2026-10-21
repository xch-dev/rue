use std::collections::{HashMap, HashSet};

use id_arena::Arena;

use crate::{Alias, Struct, Type, TypeId, substitute_with_mappings};

/// Returns `true` if the type can reach itself without passing through a pair or function type.
///
/// Such a type (for example `type A = B; type B = A;` or `type Loop = Loop | Int;`) has no
/// structure that bounds its recursion, so it cannot be meaningfully compared or checked and
/// must be rejected. Recursion through a pair (such as `List<T>`) is fine, since each level
/// of recursion corresponds to a level of the CLVM tree.
pub fn is_non_contractive(arena: &mut Arena<Type>, id: TypeId) -> bool {
    // Substitution expands generic applications, so a cycle that is only created once the
    // generic arguments are filled in (e.g. `type A<T> = T; type B = A<B>;`) is visible.
    // The generic parameters are mapped to themselves, since that's how recursive references
    // such as `type A<T> = A<T> | nil;` substitute them, and in-progress types are tracked
    // separately for each mapping.
    let generics = match &arena[id] {
        Type::Alias(Alias { generics, .. }) | Type::Struct(Struct { generics, .. }) => {
            generics.iter().map(|&generic| (generic, generic)).collect()
        }
        _ => HashMap::new(),
    };

    let root = substitute_with_mappings(arena, id, &generics);

    let mut visited = HashSet::new();
    let mut stack = unguarded_edges(arena, root);

    while let Some(id) = stack.pop() {
        if id == root {
            return true;
        }

        if visited.insert(id) {
            stack.extend(unguarded_edges(arena, id));
        }
    }

    false
}

/// Returns `true` if a non-contractive cycle can be reached from the type (including through
/// pairs and functions), even if the type itself isn't part of the cycle. The type must already
/// be substituted.
pub(crate) fn reaches_non_contractive_cycle(arena: &Arena<Type>, id: TypeId) -> bool {
    let mut reachable = HashSet::new();
    let mut stack = vec![id];

    while let Some(id) = stack.pop() {
        if reachable.insert(id) {
            stack.extend(unguarded_edges(arena, id));

            match &arena[id] {
                Type::Pair(pair) => stack.extend([pair.first, pair.rest]),
                Type::Function(function) => {
                    stack.extend(function.params.values().copied());
                    stack.push(function.ret);
                }
                _ => {}
            }
        }
    }

    let mut done = HashSet::new();

    reachable
        .into_iter()
        .any(|id| has_unguarded_cycle(arena, id, &mut HashSet::new(), &mut done))
}

fn has_unguarded_cycle(
    arena: &Arena<Type>,
    id: TypeId,
    path: &mut HashSet<TypeId>,
    done: &mut HashSet<TypeId>,
) -> bool {
    if done.contains(&id) {
        return false;
    }

    if !path.insert(id) {
        return true;
    }

    for edge in unguarded_edges(arena, id) {
        if has_unguarded_cycle(arena, edge, path, done) {
            return true;
        }
    }

    path.remove(&id);
    done.insert(id);

    false
}

fn unguarded_edges(arena: &Arena<Type>, id: TypeId) -> Vec<TypeId> {
    match &arena[id] {
        Type::Ref(id) => vec![*id],
        Type::Alias(alias) => vec![alias.inner],
        Type::Struct(ty) => vec![ty.inner],
        Type::Union(ty) => ty.types.clone(),
        Type::Apply(apply) => vec![apply.inner],
        Type::Unresolved
        | Type::Generic(_)
        | Type::Any
        | Type::Never
        | Type::Atom(_)
        | Type::Pair(_)
        | Type::Function(_) => vec![],
    }
}
