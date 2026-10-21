use std::collections::{BTreeMap, HashMap};

use id_arena::Arena;
use indexmap::{IndexMap, IndexSet};
use log::{Level, log_enabled, trace};

use crate::{Alias, FunctionType, Memo, Pair, Struct, Type, TypeId, Union, stringify_impl};

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
    let scope = ctx.scope(mappings);
    let result = substitute_impl(arena, id, mappings, scope, &mut ctx);

    if let Some(name) = &name {
        let new_name = stringify_impl(arena, result, &mut IndexMap::new());
        trace!("Substitution from {name} yielded {new_name}");
    }

    result
}

#[derive(Debug, Default)]
struct SubstitutionContext {
    /// The types currently being substituted, along with the placeholders for their results.
    stack: IndexMap<TypeId, TypeId>,
    /// The generic mappings that have been in effect, so that substitutions can be keyed by them.
    scopes: IndexSet<BTreeMap<TypeId, TypeId>>,
    /// Completed substitutions, keyed by the type and the index of the mappings in `scopes`.
    memo: Memo<TypeId, (TypeId, usize), TypeId>,
}

impl SubstitutionContext {
    fn scope(&mut self, map: &HashMap<TypeId, TypeId>) -> usize {
        self.scopes
            .insert_full(map.iter().map(|(&generic, &ty)| (generic, ty)).collect())
            .0
    }
}

fn substitute_impl(
    arena: &mut Arena<Type>,
    mut old_id: TypeId,
    map: &HashMap<TypeId, TypeId>,
    scope: usize,
    ctx: &mut SubstitutionContext,
) -> TypeId {
    if let Some(id) = map.get(&old_id) {
        old_id = *id;
    }

    if let Some((index, _, id)) = ctx.stack.get_full(&old_id) {
        ctx.memo.reached(index);
        return *id;
    }

    if let Some(result) = ctx.memo.get(&(old_id, scope), ctx.stack.keys().copied()) {
        // Each reference to an alias or struct is substituted into a distinct type, since their
        // ids are used to tell apart the variants that came from them (for example, when
        // subtracting from a union). The inner type can still be shared between them.
        return match arena[result] {
            Type::Alias(_) | Type::Struct(_) => arena.alloc(arena[result].clone()),
            _ => result,
        };
    }

    let frame = ctx.memo.enter(ctx.stack.len());

    let new_id = arena.alloc(Type::Unresolved);
    ctx.stack.insert(old_id, new_id);

    let result = match arena[old_id].clone() {
        Type::Apply(apply) => {
            let mut new_map = apply.generics.clone();

            for arg in new_map.values_mut() {
                *arg = substitute_impl(arena, *arg, map, scope, ctx);
            }

            let new_scope = ctx.scope(&new_map);
            substitute_impl(arena, apply.inner, &new_map, new_scope, ctx)
        }
        Type::Unresolved => {
            // This may be the placeholder for a type that is still being substituted, in which
            // case it will be resolved once that's done
            if let Some(index) = ctx.stack.values().position(|&id| id == old_id) {
                ctx.memo.reached(index);
            }

            old_id
        }
        Type::Generic(_) | Type::Atom(_) | Type::Never | Type::Any => old_id,
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
            let params = function
                .params
                .iter()
                .map(|(name, id)| (name.clone(), substitute_impl(arena, *id, map, scope, ctx)))
                .collect();
            let ret = substitute_impl(arena, function.ret, map, scope, ctx);
            // Generics that have been substituted are no longer generic parameters
            let generics = function
                .generics
                .iter()
                .copied()
                .filter(|generic| !map.contains_key(generic))
                .collect();
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

    ctx.memo.exit(frame, old_id, Some((old_id, scope)), result);

    result
}
