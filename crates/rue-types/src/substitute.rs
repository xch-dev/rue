use std::collections::{BTreeMap, HashMap, HashSet};

use id_arena::Arena;
use indexmap::{IndexMap, IndexSet};
use log::{Level, log_enabled, trace};

use crate::{Alias, FunctionType, Pair, Struct, Type, TypeId, Union, stringify_impl};

pub fn substitute(arena: &mut Arena<Type>, id: TypeId) -> TypeId {
    substitute_with_mappings(arena, id, &HashMap::new())
}

pub fn substitute_with_mappings(
    arena: &mut Arena<Type>,
    id: TypeId,
    mappings: &HashMap<TypeId, TypeId>,
) -> TypeId {
    let name = log_enabled!(Level::Trace).then(|| stringify_impl(arena, id, &mut IndexMap::new()));

    if let Some(name) = &name {
        trace!("Substituting {name}");
    }

    let mut ctx = SubstitutionContext::default();

    // The mapped types belong to the caller's scope, so they're substituted without the mappings
    let empty = HashMap::new();
    let empty_scope = ctx.scope(&empty);
    let mappings = mappings
        .iter()
        .map(|(&generic, &ty)| {
            let ty = substitute_impl(arena, ty, &empty, empty_scope, &mut ctx);
            (generic, ty)
        })
        .collect();

    let scope = ctx.scope(&mappings);
    let result = substitute_impl(arena, id, &mappings, scope, &mut ctx);

    if let Some(name) = &name {
        let new_name = stringify_impl(arena, result, &mut IndexMap::new());
        trace!("Substitution from {name} yielded {new_name}");
    }

    result
}

#[derive(Debug, Default)]
struct SubstitutionContext {
    /// The types currently being substituted (keyed by the type and the index of the mappings in
    /// `scopes`), along with the placeholders for their results. The same type can be substituted
    /// under different mappings at the same time (for example, the inner `Alt<B, A>` in
    /// `type Alt<A, B> = nil | (A, Alt<B, A>)`), so it isn't keyed by the type alone.
    stack: IndexMap<(TypeId, usize), TypeId>,
    /// The generic mappings that have been in effect, so that substitutions can be keyed by them.
    scopes: IndexSet<BTreeMap<TypeId, TypeId>>,
    /// Completed substitutions, keyed by the type and the index of the mappings in `scopes`.
    memo: HashMap<(TypeId, usize), TypeId>,
    /// Copies of function generics, which they're renamed to when they would capture a mapped
    /// type. The first copy that isn't referenced is reused, so that there are finitely many
    /// distinct mappings and recursive types still reach the substitutions that are in progress.
    renames: HashMap<TypeId, Vec<TypeId>>,
}

impl SubstitutionContext {
    fn scope(&mut self, map: &HashMap<TypeId, TypeId>) -> usize {
        self.scopes
            .insert_full(map.iter().map(|(&generic, &ty)| (generic, ty)).collect())
            .0
    }

    /// Returns `true` if the mapped type may reference the generic. A placeholder for a type that
    /// is still being substituted may reference whatever the type or its mappings do.
    fn may_reference(&self, arena: &Arena<Type>, ty: TypeId, generic: TypeId) -> bool {
        references_generic(arena, ty, generic, |id| {
            self.stack
                .iter()
                .filter(|&(_, &placeholder)| placeholder == id)
                .flat_map(|(&(old_id, scope), _)| {
                    std::iter::once(old_id).chain(self.scopes[scope].values().copied())
                })
                .collect()
        })
    }
}

fn substitute_impl(
    arena: &mut Arena<Type>,
    old_id: TypeId,
    map: &HashMap<TypeId, TypeId>,
    scope: usize,
    ctx: &mut SubstitutionContext,
) -> TypeId {
    // The mapped type has already been substituted in the scope it came from
    if let Some(&id) = map.get(&old_id) {
        return id;
    }

    if let Some(&id) = ctx.stack.get(&(old_id, scope)) {
        return id;
    }

    if let Some(&result) = ctx.memo.get(&(old_id, scope)) {
        // Each reference to an alias or struct is substituted into a distinct type, since their
        // ids are used to tell apart the variants that came from them (for example, when
        // subtracting from a union). The inner type can still be shared between them.
        return match arena[result] {
            Type::Alias(_) | Type::Struct(_) => arena.alloc(arena[result].clone()),
            _ => result,
        };
    }

    let new_id = arena.alloc(Type::Unresolved);
    ctx.stack.insert((old_id, scope), new_id);

    let result = match arena[old_id].clone() {
        Type::Apply(apply) => {
            let mut new_map = apply.generics.clone();

            for arg in new_map.values_mut() {
                *arg = substitute_impl(arena, *arg, map, scope, ctx);
            }

            let new_scope = ctx.scope(&new_map);
            substitute_impl(arena, apply.inner, &new_map, new_scope, ctx)
        }
        // This may be the placeholder for a type that is still being substituted, in which case
        // it will be resolved once that's done
        Type::Unresolved | Type::Generic(_) | Type::Atom(_) | Type::Never | Type::Any => old_id,
        Type::Ref(id) => substitute_impl(arena, id, map, scope, ctx),
        Type::Pair(pair) => {
            let first = substitute_impl(arena, pair.first, map, scope, ctx);
            let rest = substitute_impl(arena, pair.rest, map, scope, ctx);
            arena.alloc(Type::Pair(Pair::new(first, rest)))
        }
        Type::Struct(ty) => {
            let inner = substitute_impl(arena, ty.inner, map, scope, ctx);
            let generics = ty
                .generics
                .iter()
                .map(|&id| substitute_impl(arena, id, map, scope, ctx))
                .collect();
            arena.alloc(Type::Struct(Struct {
                inner,
                generics,
                ..ty
            }))
        }
        Type::Alias(alias) => {
            let inner = substitute_impl(arena, alias.inner, map, scope, ctx);
            let generics = alias
                .generics
                .iter()
                .map(|&id| substitute_impl(arena, id, map, scope, ctx))
                .collect();
            arena.alloc(Type::Alias(Alias {
                inner,
                generics,
                ..alias
            }))
        }
        Type::Function(function) => {
            // The function's own generics are bound by it, so they shadow any mappings for them.
            // If a mapped type references one of them, it's renamed so that it isn't captured.
            let mut inner_map: HashMap<TypeId, TypeId> = map
                .iter()
                .filter(|(generic, _)| !function.generics.contains(generic))
                .map(|(&generic, &ty)| (generic, ty))
                .collect();

            let generics = function
                .generics
                .iter()
                .map(|&generic| {
                    let mut renamed = generic;

                    for index in 0.. {
                        if !inner_map
                            .values()
                            .any(|&ty| ctx.may_reference(arena, ty, renamed))
                        {
                            break;
                        }

                        let copies = ctx.renames.entry(generic).or_default();

                        if index == copies.len() {
                            copies.push(arena.alloc(arena[generic].clone()));
                        }

                        renamed = copies[index];
                    }

                    if renamed != generic {
                        inner_map.insert(generic, renamed);
                    }

                    renamed
                })
                .collect();

            let inner_scope = ctx.scope(&inner_map);
            let params = function
                .params
                .iter()
                .map(|(name, id)| {
                    let id = substitute_impl(arena, *id, &inner_map, inner_scope, ctx);
                    (name.clone(), id)
                })
                .collect();
            let ret = substitute_impl(arena, function.ret, &inner_map, inner_scope, ctx);
            arena.alloc(Type::Function(FunctionType {
                generics,
                params,
                ret,
                ..function
            }))
        }
        Type::Union(union) => {
            let types = union
                .types
                .iter()
                .map(|id| substitute_impl(arena, *id, map, scope, ctx))
                .collect();
            arena.alloc(Type::Union(Union::new(types)))
        }
    };

    ctx.stack.pop().unwrap();

    *arena.get_mut(new_id).unwrap() = Type::Ref(result);

    ctx.memo.insert((old_id, scope), result);

    result
}

/// Returns `true` if the generic is referenced anywhere in the type.
pub fn contains_generic(arena: &mut Arena<Type>, id: TypeId, generic: TypeId) -> bool {
    // Substitution expands generic applications, so the generic is only visible where it's used
    let root = substitute(arena, id);
    references_generic(arena, root, generic, |_| Vec::new())
}

/// Returns `true` if the generic is referenced in the type, other than where it's bound by a
/// function type. An unresolved type (such as the placeholder for a type that is still being
/// substituted) may reference the types given for it.
pub(crate) fn references_generic(
    arena: &Arena<Type>,
    id: TypeId,
    generic: TypeId,
    unresolved: impl Fn(TypeId) -> Vec<TypeId>,
) -> bool {
    let mut visited = HashSet::new();
    let mut stack = vec![id];

    while let Some(id) = stack.pop() {
        if id == generic {
            return true;
        }

        if !visited.insert(id) {
            continue;
        }

        match &arena[id] {
            Type::Unresolved => stack.extend(unresolved(id)),
            Type::Generic(_) | Type::Any | Type::Never | Type::Atom(_) => {}
            Type::Ref(id) => stack.push(*id),
            Type::Pair(pair) => stack.extend([pair.first, pair.rest]),
            Type::Alias(alias) => stack.push(alias.inner),
            Type::Struct(ty) => stack.push(ty.inner),
            Type::Apply(apply) => {
                stack.push(apply.inner);
                stack.extend(apply.generics.values().copied());
            }
            Type::Function(function) => {
                if !function.generics.contains(&generic) {
                    stack.push(function.ret);
                    stack.extend(function.params.values().copied());
                }
            }
            Type::Union(union) => stack.extend(union.types.iter().copied()),
        }
    }

    false
}
