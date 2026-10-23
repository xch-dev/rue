use std::collections::HashSet;

use id_arena::Arena;

use crate::{Alias, Struct, Type, TypeId, free_generics, resolve_refs};

/// Returns `true` if the type is part of a cycle of type declarations that, once expanded, has
/// infinitely many distinct generic arguments.
///
/// Such a type (for example `type L<T> = nil | (T, L<(T, T)>);`) is non-regular, since each
/// level of recursion has new generic arguments, so expanding it never reaches a type that has
/// already been seen and it can't be represented as a finite (cyclic) type.
///
/// Each generic argument of a reference to a type declaration passes the generic parameters it
/// refers to on to the referenced declaration's generic parameter. An argument that is just a
/// generic parameter (such as the `B` in `type Alt<A, B> = nil | (A, Alt<B, A>);`) passes it on
/// unchanged, and any other argument that refers to one (such as the `(T, T)` above) grows it.
/// The generic arguments are only infinite if a generic parameter can be grown and passed back
/// to itself. An argument that doesn't refer to any generic parameters (such as the `Int` in
/// `type L<T> = nil | (T, L<Int>);`) is the same at every level, so it's fine.
///
/// This must be checked before the type is substituted, since substituting a non-regular type
/// would never terminate.
pub fn is_non_regular(arena: &Arena<Type>, id: TypeId) -> bool {
    let mut edges = Vec::new();
    let mut visited = HashSet::new();
    let mut stack = vec![id];

    while let Some(declaration) = stack.pop() {
        if !visited.insert(declaration) {
            continue;
        }

        let inner = match &arena[declaration] {
            Type::Alias(alias) => alias.inner,
            Type::Struct(ty) => ty.inner,
            _ => unreachable!(),
        };

        collect_references(arena, inner, &mut stack, &mut edges, &mut HashSet::new());
    }

    let (Type::Alias(Alias { generics, .. }) | Type::Struct(Struct { generics, .. })) = &arena[id]
    else {
        unreachable!()
    };

    // Only cycles through this declaration's generic parameters are checked, since the others
    // have already been checked when the declarations they belong to were compiled
    generics.iter().any(|&generic| {
        let forward = reachable(&edges, generic, |edge| (edge.from, edge.to));
        let backward = reachable(&edges, generic, |edge| (edge.to, edge.from));

        edges
            .iter()
            .any(|edge| edge.grows && forward.contains(&edge.from) && backward.contains(&edge.to))
    })
}

/// A generic parameter that is passed on to another generic parameter by a generic argument.
#[derive(Debug, Clone, Copy)]
struct Edge {
    from: TypeId,
    to: TypeId,
    /// Whether the argument is something other than the generic parameter itself.
    grows: bool,
}

/// Collects the type declarations referenced by a declaration's inner type, along with how their
/// generic arguments pass on the generic parameters they refer to.
fn collect_references(
    arena: &Arena<Type>,
    id: TypeId,
    declarations: &mut Vec<TypeId>,
    edges: &mut Vec<Edge>,
    visited: &mut HashSet<TypeId>,
) {
    if !visited.insert(id) {
        return;
    }

    match &arena[id] {
        Type::Alias(_) | Type::Struct(_) => declarations.push(id),
        Type::Apply(apply) => {
            let inner = resolve_refs(arena, apply.inner);

            if matches!(arena[inner], Type::Alias(_) | Type::Struct(_)) {
                declarations.push(inner);

                for (&to, &arg) in &apply.generics {
                    let resolved = resolve_refs(arena, arg);

                    edges.extend(free_generics(arena, arg).into_iter().map(|from| Edge {
                        from,
                        to,
                        grows: from != resolved,
                    }));
                }
            } else {
                collect_references(arena, inner, declarations, edges, visited);
            }

            for &arg in apply.generics.values() {
                collect_references(arena, arg, declarations, edges, visited);
            }
        }
        Type::Ref(id) => collect_references(arena, *id, declarations, edges, visited),
        Type::Pair(pair) => {
            collect_references(arena, pair.first, declarations, edges, visited);
            collect_references(arena, pair.rest, declarations, edges, visited);
        }
        Type::Union(union) => {
            for &id in &union.types {
                collect_references(arena, id, declarations, edges, visited);
            }
        }
        Type::Function(function) => {
            for &id in function.params.values() {
                collect_references(arena, id, declarations, edges, visited);
            }

            collect_references(arena, function.ret, declarations, edges, visited);
        }
        Type::Unresolved | Type::Generic(_) | Type::Atom(_) | Type::Never | Type::Any => {}
    }
}

/// Returns the generic parameters that can be reached from the given one along the edges, which
/// are given as pairs of generic parameters to follow from and to.
fn reachable(
    edges: &[Edge],
    from: TypeId,
    direction: impl Fn(&Edge) -> (TypeId, TypeId),
) -> HashSet<TypeId> {
    let mut visited = HashSet::new();
    let mut stack = vec![from];

    while let Some(generic) = stack.pop() {
        if !visited.insert(generic) {
            continue;
        }

        for (source, target) in edges.iter().map(&direction) {
            if source == generic {
                stack.push(target);
            }
        }
    }

    visited
}
