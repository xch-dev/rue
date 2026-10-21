use std::collections::{BTreeMap, HashMap};

use id_arena::Arena;
use indexmap::{IndexMap, IndexSet};
use log::trace;

use crate::{Alias, FunctionType, Pair, Struct, Type, TypeId, Union, stringify_impl};

pub fn substitute(arena: &mut Arena<Type>, id: TypeId) -> TypeId {
    substitute_with_mappings(arena, id, &HashMap::new())
}

pub fn substitute_with_mappings(
    arena: &mut Arena<Type>,
    id: TypeId,
    mappings: &HashMap<TypeId, TypeId>,
) -> TypeId {
    let name = stringify_impl(arena, id, &mut IndexMap::new());
    trace!("Substituting {name}");
    let mut ctx = SubstitutionContext::default();

    // The mapped types belong to the caller's scope, so they're substituted without the mappings
    let empty = ctx.scope(BTreeMap::new());
    let mappings = mappings
        .iter()
        .map(|(&generic, &ty)| (generic, substitute_impl(arena, ty, empty, &mut ctx)))
        .collect();

    let scope = ctx.scope(mappings);
    let result = substitute_impl(arena, id, scope, &mut ctx);
    let new_name = stringify_impl(arena, result, &mut IndexMap::new());
    trace!("Substitution from {name} yielded {new_name}");
    result
}

/// A scope is the set of generic mappings that are in effect while substituting a type. The same
/// type can be substituted under different scopes at the same time (for example, the inner
/// `Alt<B, A>` in `type Alt<A, B> = nil | (A, Alt<B, A>)`), so in-progress types are tracked
/// per scope rather than by type alone.
#[derive(Default)]
struct SubstitutionContext {
    scopes: IndexSet<BTreeMap<TypeId, TypeId>>,
    stack: IndexMap<(TypeId, usize), TypeId>,
}

impl SubstitutionContext {
    fn scope(&mut self, mappings: BTreeMap<TypeId, TypeId>) -> usize {
        self.scopes.insert_full(mappings).0
    }
}

fn substitute_impl(
    arena: &mut Arena<Type>,
    old_id: TypeId,
    scope: usize,
    ctx: &mut SubstitutionContext,
) -> TypeId {
    // The mapped type has already been substituted in the scope it came from
    if let Some(&id) = ctx.scopes[scope].get(&old_id) {
        return id;
    }

    if let Some(&id) = ctx.stack.get(&(old_id, scope)) {
        return id;
    }

    let new_id = arena.alloc(Type::Unresolved);
    ctx.stack.insert((old_id, scope), new_id);

    let result = match arena[old_id].clone() {
        Type::Apply(apply) => {
            let mappings = apply
                .generics
                .iter()
                .map(|(&generic, &ty)| (generic, substitute_impl(arena, ty, scope, ctx)))
                .collect();

            let inner_scope = ctx.scope(mappings);
            substitute_impl(arena, apply.inner, inner_scope, ctx)
        }
        Type::Unresolved | Type::Generic(_) | Type::Atom(_) | Type::Never | Type::Any => old_id,
        Type::Ref(id) => substitute_impl(arena, id, scope, ctx),
        Type::Pair(pair) => {
            let first = substitute_impl(arena, pair.first, scope, ctx);
            let rest = substitute_impl(arena, pair.rest, scope, ctx);
            arena.alloc(Type::Pair(Pair::new(first, rest)))
        }
        Type::Struct(ty) => {
            let inner = substitute_impl(arena, ty.inner, scope, ctx);
            let generics = ty
                .generics
                .iter()
                .map(|&id| substitute_impl(arena, id, scope, ctx))
                .collect();
            arena.alloc(Type::Struct(Struct {
                inner,
                generics,
                ..ty
            }))
        }
        Type::Alias(alias) => {
            let inner = substitute_impl(arena, alias.inner, scope, ctx);
            let generics = alias
                .generics
                .iter()
                .map(|&id| substitute_impl(arena, id, scope, ctx))
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
                .map(|(name, id)| (name.clone(), substitute_impl(arena, *id, scope, ctx)))
                .collect();
            let ret = substitute_impl(arena, function.ret, scope, ctx);
            // Generics that have been substituted are no longer generic parameters
            let generics = function
                .generics
                .iter()
                .copied()
                .filter(|generic| !ctx.scopes[scope].contains_key(generic))
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
                .map(|id| substitute_impl(arena, *id, scope, ctx))
                .collect();
            arena.alloc(Type::Union(Union::new(types)))
        }
    };

    ctx.stack.pop().unwrap();

    *arena.get_mut(new_id).unwrap() = Type::Ref(result);

    result
}
