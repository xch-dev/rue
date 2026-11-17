use std::collections::{HashMap, HashSet};

use id_arena::Arena;

use crate::{Alias, Struct, Type, TypeId, grow_stack, substitute_instances};

/// Returns `true` if the type can reach itself without passing through a pair or function type.
///
/// Such a type (for example `type A = B; type B = A;` or `type Loop = Loop | Int;`) has no
/// structure that bounds its recursion, so it cannot be meaningfully compared or checked and
/// must be rejected. Recursion through a pair (such as `List<T>`) is fine, since each level
/// of recursion corresponds to a level of the CLVM tree.
///
/// For a generic type, this includes any application of it that substituting it reaches, even if
/// the type with its own generic parameters isn't part of the cycle. For example, in
/// `type T1<A> = A | (nil, A) | T0; type T0 = T1<Int>;`, `T1<Int>` reaches itself through `T0`.
pub fn is_non_contractive(arena: &mut Arena<Type>, id: TypeId) -> bool {
    !non_contractive_types(arena, &[id]).is_empty()
}

/// Returns the types that [`is_non_contractive`] is `true` for, in the order they're given. The
/// types are substituted together, so this takes about as long as substituting all of them once.
pub fn non_contractive_types(arena: &mut Arena<Type>, ids: &[TypeId]) -> Vec<TypeId> {
    // Substitution expands generic applications, so a cycle that is only created once the
    // generic arguments are filled in (e.g. `type A<T> = T; type B = A<B>;`) is visible.
    // The generic parameters are mapped to themselves, since that's how recursive references
    // such as `type A<T> = A<T> | nil;` substitute them, and in-progress types are tracked
    // separately for each mapping.
    let types: Vec<(TypeId, HashMap<TypeId, TypeId>)> = ids
        .iter()
        .map(|&id| {
            let generics = match &arena[id] {
                Type::Alias(Alias { generics, .. }) | Type::Struct(Struct { generics, .. }) => {
                    generics.iter().map(|&generic| (generic, generic)).collect()
                }
                _ => HashMap::new(),
            };
            (id, generics)
        })
        .collect();

    let instances = substitute_instances(arena, &types);
    let cyclic = on_unguarded_cycles(arena, instances.values().flatten().copied());

    ids.iter()
        .copied()
        .filter(|id| {
            instances[id]
                .iter()
                .any(|instance| cyclic.contains(instance))
        })
        .collect()
}

/// Returns the types reachable from the given ones without passing through a pair or function
/// type that can reach themselves that way. They must already be substituted.
fn on_unguarded_cycles(
    arena: &Arena<Type>,
    roots: impl IntoIterator<Item = TypeId>,
) -> HashSet<TypeId> {
    let mut tarjan = Tarjan::default();

    for root in roots {
        if !tarjan.index.contains_key(&root) {
            tarjan.visit(arena, root);
        }
    }

    tarjan.cyclic
}

/// Finds the strongly connected components of the graph of unguarded edges (Tarjan's algorithm).
#[derive(Default)]
struct Tarjan {
    index: HashMap<TypeId, usize>,
    lowlink: HashMap<TypeId, usize>,
    stack: Vec<TypeId>,
    on_stack: HashSet<TypeId>,
    /// The types in a component with more than one type, or with an edge to itself.
    cyclic: HashSet<TypeId>,
}

impl Tarjan {
    fn visit(&mut self, arena: &Arena<Type>, id: TypeId) {
        grow_stack(|| self.visit_inner(arena, id));
    }

    fn visit_inner(&mut self, arena: &Arena<Type>, id: TypeId) {
        let index = self.index.len();
        self.index.insert(id, index);
        self.lowlink.insert(id, index);
        self.stack.push(id);
        self.on_stack.insert(id);

        let edges = unguarded_edges(arena, id);

        for &edge in &edges {
            if !self.index.contains_key(&edge) {
                self.visit(arena, edge);
                let lowlink = self.lowlink[&id].min(self.lowlink[&edge]);
                self.lowlink.insert(id, lowlink);
            } else if self.on_stack.contains(&edge) {
                let lowlink = self.lowlink[&id].min(self.index[&edge]);
                self.lowlink.insert(id, lowlink);
            }
        }

        if self.lowlink[&id] != index {
            return;
        }

        let mut component = Vec::new();

        loop {
            let member = self.stack.pop().unwrap();
            self.on_stack.remove(&member);
            component.push(member);

            if member == id {
                break;
            }
        }

        if component.len() > 1 || edges.contains(&id) {
            self.cyclic.extend(component);
        }
    }
}

/// Returns `true` if a non-contractive cycle can be reached from the type (including through
/// pairs and functions), even if the type itself isn't part of the cycle. The type must already
/// be substituted.
pub fn reaches_non_contractive_cycle(arena: &Arena<Type>, id: TypeId) -> bool {
    has_non_contractive_cycle(arena, &reachable(arena, id))
}

/// Returns the types reachable from the type (including through pairs and functions), which must
/// already be substituted.
pub(crate) fn reachable(arena: &Arena<Type>, id: TypeId) -> HashSet<TypeId> {
    let mut reachable = HashSet::new();
    let mut stack = vec![id];

    while let Some(id) = stack.pop() {
        if reachable.insert(id) {
            stack.extend(unguarded_edges(arena, id));

            match &arena[id] {
                Type::Pair(pair) => stack.extend([pair.first, pair.rest]),
                Type::Function(function) => {
                    stack.extend(function.param_types());
                    stack.push(function.ret);
                }
                _ => {}
            }
        }
    }

    reachable
}

/// Returns `true` if any of the types is part of (or can reach) a non-contractive cycle.
pub(crate) fn has_non_contractive_cycle(arena: &Arena<Type>, types: &HashSet<TypeId>) -> bool {
    let mut done = HashSet::new();

    types
        .iter()
        .any(|&id| has_unguarded_cycle(arena, id, &mut HashSet::new(), &mut done))
}

fn has_unguarded_cycle(
    arena: &Arena<Type>,
    id: TypeId,
    path: &mut HashSet<TypeId>,
    done: &mut HashSet<TypeId>,
) -> bool {
    grow_stack(|| has_unguarded_cycle_inner(arena, id, path, done))
}

fn has_unguarded_cycle_inner(
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
