use std::collections::{BTreeMap, HashMap, HashSet};

use id_arena::Arena;
use indexmap::IndexMap;
use log::{Level, log_enabled, trace};

use crate::{
    Alias, FunctionParameter, FunctionType, FxHashMap, FxHashSet, FxIndexMap, FxIndexSet, Pair,
    Struct, Type, TypeId, Union, grow_stack, step, stringify_impl,
};

pub fn substitute(arena: &mut Arena<Type>, id: TypeId) -> TypeId {
    substitute_with_mappings(arena, id, &HashMap::new())
}

pub fn substitute_with_mappings(
    arena: &mut Arena<Type>,
    id: TypeId,
    mappings: &HashMap<TypeId, TypeId>,
) -> TypeId {
    substitute_in(arena, &mut SubstitutionContext::default(), id, mappings)
}

/// Substitutes each of the types with its mappings, sharing the work between them, and returns
/// every type that each of them was substituted into along the way, under any mappings. For an
/// alias, these are its substitution and every application of it that is substituted (such as
/// `T1<Int>` in `type T1<A> = A | T0; type T0 = T1<Int>;`).
pub(crate) fn substitute_instances(
    arena: &mut Arena<Type>,
    types: &[(TypeId, HashMap<TypeId, TypeId>)],
) -> FxHashMap<TypeId, Vec<TypeId>> {
    let mut ctx = SubstitutionContext {
        instances: types.iter().map(|&(id, _)| (id, Vec::new())).collect(),
        ..SubstitutionContext::default()
    };

    for (id, mappings) in types {
        substitute_in(arena, &mut ctx, *id, mappings);
    }

    ctx.instances
}

/// The mappings, in the order of the generics they're for. Most have one or none, which are in
/// that order already, and most others only a few, which are sorted in place, so only a larger
/// map is collected into a `Vec` to be sorted.
fn sorted_by_generic(mappings: &HashMap<TypeId, TypeId>) -> SortedByGeneric<'_> {
    let len = mappings.len();

    if len <= 1 {
        return SortedByGeneric::Map(mappings.iter());
    }

    let mut iter = mappings.iter();

    if len <= SMALL_MAPPINGS {
        let first = iter.next().unwrap();
        let mut items = [first; SMALL_MAPPINGS];

        for (slot, item) in items[1..].iter_mut().zip(iter) {
            *slot = item;
        }

        items[..len].sort_unstable_by_key(|&(&generic, _)| generic);
        return SortedByGeneric::Small(items.into_iter().take(len));
    }

    let mut sorted: Vec<_> = iter.collect();
    sorted.sort_unstable_by_key(|&(&generic, _)| generic);
    SortedByGeneric::Sorted(sorted.into_iter())
}

/// How many mappings `sorted_by_generic` sorts in place.
const SMALL_MAPPINGS: usize = 4;

/// See `sorted_by_generic`.
enum SortedByGeneric<'a> {
    Map(std::collections::hash_map::Iter<'a, TypeId, TypeId>),
    Small(std::iter::Take<std::array::IntoIter<(&'a TypeId, &'a TypeId), SMALL_MAPPINGS>>),
    Sorted(std::vec::IntoIter<(&'a TypeId, &'a TypeId)>),
}

impl<'a> Iterator for SortedByGeneric<'a> {
    type Item = (&'a TypeId, &'a TypeId);

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Map(iter) => iter.next(),
            Self::Small(iter) => iter.next(),
            Self::Sorted(iter) => iter.next(),
        }
    }
}

fn substitute_in(
    arena: &mut Arena<Type>,
    ctx: &mut SubstitutionContext,
    id: TypeId,
    mappings: &HashMap<TypeId, TypeId>,
) -> TypeId {
    let name = log_enabled!(Level::Trace).then(|| stringify_impl(arena, id, &mut IndexMap::new()));

    if let Some(name) = &name {
        trace!("Substituting {name}");
    }

    // The mapped types belong to the caller's scope, so they're substituted without the mappings,
    // in the order of their generics, since the order types are made in decides the work that
    // comparing them later does (and a `HashMap`'s order differs from one process to the next)
    let (empty_scope, empty) = ctx.scope(BTreeMap::new(), HashMap::new());
    let mappings: HashMap<TypeId, TypeId> = sorted_by_generic(mappings)
        .map(|(&generic, &ty)| {
            let ty = substitute_impl(arena, ty, &empty, empty_scope, ctx);
            (generic, ty)
        })
        .collect();

    let bindings = mappings
        .iter()
        .map(|(&generic, &ty)| (generic, Binding::Type(ty)))
        .collect();
    let (scope, mappings) = ctx.scope(bindings, mappings);
    let result = substitute_impl(arena, id, &mappings, scope, ctx);

    if let Some(name) = &name {
        let new_name = stringify_impl(arena, result, &mut IndexMap::new());
        trace!("Substitution from {name} yielded {new_name}");
    }

    result
}

/// Identifies what a generic is mapped to, so that substitutions can be keyed by it.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Binding {
    /// A type that was mapped to the generic before substituting.
    Type(TypeId),
    /// A generic argument that hasn't been substituted, along with the index of the mappings in
    /// `scopes` (of only the generics it refers to) that it's substituted with. Keying arguments
    /// by the type they're substituted into instead would make every level of a recursive type
    /// with an argument other than a generic (such as `type L<T> = nil | (T, L<Int>);`) have new
    /// mappings, even if the argument is the same at each level.
    Argument(TypeId, usize),
}

#[derive(Debug, Default)]
struct SubstitutionContext {
    /// The types currently being substituted (keyed by the type and the index of the mappings in
    /// `scopes`), along with the placeholders for their results. The same type can be substituted
    /// under different mappings at the same time (for example, the inner `Alt<B, A>` in
    /// `type Alt<A, B> = nil | (A, Alt<B, A>)`), so it isn't keyed by the type alone.
    stack: FxIndexMap<(TypeId, usize), TypeId>,
    /// The generic mappings that have been in effect, keyed by their bindings. Mappings with the
    /// same bindings substitute to the same types, so the ones that were inserted first are used.
    scopes: IndexMap<BTreeMap<TypeId, Binding>, HashMap<TypeId, TypeId>>,
    /// Completed substitutions, keyed by the type and the index of the mappings in `scopes`.
    memo: FxHashMap<(TypeId, usize), TypeId>,
    /// Copies of function generics, which they're renamed to when they would capture a mapped
    /// type. The first copy that isn't referenced is reused, so that there are finitely many
    /// distinct mappings and recursive types still reach the substitutions that are in progress.
    renames: FxHashMap<TypeId, Vec<TypeId>>,
    /// Every type that each of the types it's keyed by was substituted into, for
    /// `substitute_instances`.
    instances: FxHashMap<TypeId, Vec<TypeId>>,
}

impl SubstitutionContext {
    fn scope(
        &mut self,
        bindings: BTreeMap<TypeId, Binding>,
        map: HashMap<TypeId, TypeId>,
    ) -> (usize, HashMap<TypeId, TypeId>) {
        let entry = self.scopes.entry(bindings);
        let index = entry.index();
        (index, entry.or_insert(map).clone())
    }

    fn bindings(&self, scope: usize) -> &BTreeMap<TypeId, Binding> {
        self.scopes.get_index(scope).unwrap().0
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
    grow_stack(|| substitute_impl_inner(arena, old_id, map, scope, ctx))
}

fn substitute_impl_inner(
    arena: &mut Arena<Type>,
    old_id: TypeId,
    map: &HashMap<TypeId, TypeId>,
    scope: usize,
    ctx: &mut SubstitutionContext,
) -> TypeId {
    step();

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
        let result = match arena[result] {
            Type::Alias(_) | Type::Struct(_) => arena.alloc(arena[result].clone()),
            _ => result,
        };

        if !ctx.instances.is_empty()
            && let Some(instances) = ctx.instances.get_mut(&old_id)
        {
            instances.push(result);
        }

        return result;
    }

    let new_id = arena.alloc(Type::Unresolved);
    ctx.stack.insert((old_id, scope), new_id);

    let result = match arena[old_id].clone() {
        Type::Apply(apply) => {
            let mut bindings = BTreeMap::new();
            let mut new_map = HashMap::new();

            // In the order of their generics, as in `substitute_in`
            for (&generic, &arg) in sorted_by_generic(&apply.generics) {
                let (binding, ty) = substitute_argument(arena, arg, map, scope, ctx);
                bindings.insert(generic, binding);
                new_map.insert(generic, ty);
            }

            let (new_scope, new_map) = ctx.scope(bindings, new_map);
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

            let mut inner_bindings: BTreeMap<TypeId, Binding> = ctx
                .bindings(scope)
                .iter()
                .filter(|(generic, _)| !function.generics.contains(generic))
                .map(|(&generic, binding)| (generic, binding.clone()))
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
                        inner_bindings.insert(generic, Binding::Type(renamed));
                    }

                    renamed
                })
                .collect();

            let (inner_scope, inner_map) = ctx.scope(inner_bindings, inner_map);
            let params = function
                .params
                .iter()
                .map(|param| {
                    let ty = substitute_impl(arena, param.ty, &inner_map, inner_scope, ctx);
                    FunctionParameter::new(param.name.clone(), ty)
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

    if !ctx.instances.is_empty()
        && let Some(instances) = ctx.instances.get_mut(&old_id)
    {
        instances.push(result);
    }

    result
}

/// Substitutes a generic argument, and returns it along with its binding. A generic that is passed
/// through keeps its binding, and any other argument is substituted with only the mappings of the
/// generics it refers to.
fn substitute_argument(
    arena: &mut Arena<Type>,
    arg: TypeId,
    map: &HashMap<TypeId, TypeId>,
    scope: usize,
    ctx: &mut SubstitutionContext,
) -> (Binding, TypeId) {
    let generic = resolve_refs(arena, arg);
    let bindings = ctx.bindings(scope);

    if let Some(binding) = bindings.get(&generic) {
        return (binding.clone(), map[&generic]);
    }

    let generics = free_generics(arena, arg);

    let bindings = bindings
        .iter()
        .filter(|&(generic, _)| generics.contains(generic))
        .map(|(&generic, binding)| (generic, binding.clone()))
        .collect();

    let map = map
        .iter()
        .filter(|&(generic, _)| generics.contains(generic))
        .map(|(&generic, &ty)| (generic, ty))
        .collect();

    let (arg_scope, map) = ctx.scope(bindings, map);
    let ty = substitute_impl(arena, arg, &map, arg_scope, ctx);

    (Binding::Argument(arg, arg_scope), ty)
}

/// Returns the generics that may be replaced when substituting the type. This over-approximates
/// them (for example, it includes generics bound by function types), but excludes the inner type
/// of a generic application, since that is only substituted with the application's arguments.
///
/// They're in the order they're found, so that what's built from them, such as the order of a
/// union's members, is the same from run to run.
pub(crate) fn free_generics(arena: &Arena<Type>, id: TypeId) -> FxIndexSet<TypeId> {
    let mut generics = FxIndexSet::default();
    let mut visited = FxHashSet::default();
    let mut stack = vec![id];

    while let Some(id) = stack.pop() {
        if !visited.insert(id) {
            continue;
        }

        match &arena[id] {
            Type::Generic(_) => {
                generics.insert(id);
            }
            Type::Unresolved | Type::Any | Type::Never | Type::Atom(_) => {}
            Type::Ref(id) => stack.push(*id),
            Type::Pair(pair) => stack.extend([pair.first, pair.rest]),
            Type::Alias(alias) => {
                stack.push(alias.inner);
                stack.extend(alias.generics.iter().copied());
            }
            Type::Struct(ty) => {
                stack.push(ty.inner);
                stack.extend(ty.generics.iter().copied());
            }
            Type::Apply(apply) => {
                // Its arguments are in a `HashMap`, so they're visited in the order of the
                // generics they're for
                let mut arguments: Vec<_> = apply.generics.iter().collect();
                arguments.sort_unstable_by_key(|&(&generic, _)| generic);
                stack.extend(arguments.into_iter().map(|(_, &ty)| ty));
            }
            Type::Function(function) => {
                stack.push(function.ret);
                stack.extend(function.param_types());
            }
            Type::Union(union) => stack.extend(union.types.iter().copied()),
        }
    }

    generics
}

/// Whether the type has no values: `Never`, a pair or struct with a part that has none, or a union
/// whose members all have none, such as `(Never, Int)`. A type that only has values through
/// itself has none either, such as `type Forever = (Int, Forever)`, since a value is finite. A
/// generic counts as having values, since it can stand for a type that does.
pub fn is_uninhabited(arena: &mut Arena<Type>, id: TypeId) -> bool {
    // Substitution expands generic applications, so that their arguments are where they're used
    let root = substitute(arena, id);

    // The types that the root is made of, in the order they're found
    let mut types = Vec::new();
    let mut visited = HashSet::new();
    let mut stack = vec![root];

    while let Some(id) = stack.pop() {
        if !visited.insert(id) {
            continue;
        }

        types.push(id);

        match &arena[id] {
            Type::Unresolved
            | Type::Generic(_)
            | Type::Any
            | Type::Never
            | Type::Atom(_)
            | Type::Function(_)
            | Type::Apply(_) => {}
            Type::Ref(id) => stack.push(*id),
            Type::Pair(pair) => stack.extend([pair.first, pair.rest]),
            Type::Alias(alias) => stack.push(alias.inner),
            Type::Struct(ty) => stack.push(ty.inner),
            Type::Union(union) => stack.extend(union.types.iter().copied()),
        }
    }

    // The types found to have a value so far, which only grows: a type has one once the types
    // it's made of do, so this ends when a pass over them finds no more
    let mut inhabited = HashSet::new();

    loop {
        let before = inhabited.len();

        for &id in &types {
            let has_values = match &arena[id] {
                Type::Never => false,
                Type::Unresolved
                | Type::Generic(_)
                | Type::Any
                | Type::Atom(_)
                | Type::Function(_)
                | Type::Apply(_) => true,
                Type::Ref(id) => inhabited.contains(id),
                Type::Pair(pair) => {
                    inhabited.contains(&pair.first) && inhabited.contains(&pair.rest)
                }
                Type::Alias(alias) => inhabited.contains(&alias.inner),
                Type::Struct(ty) => inhabited.contains(&ty.inner),
                Type::Union(union) => union.types.iter().any(|ty| inhabited.contains(ty)),
            };

            if has_values {
                inhabited.insert(id);
            }
        }

        if inhabited.len() == before {
            break;
        }
    }

    !inhabited.contains(&root)
}

pub(crate) fn resolve_refs(arena: &Arena<Type>, mut id: TypeId) -> TypeId {
    while let Type::Ref(inner) = arena[id] {
        id = inner;
    }

    id
}

/// Returns `true` if the generic is referenced anywhere in the type.
pub fn contains_generic(arena: &mut Arena<Type>, id: TypeId, generic: TypeId) -> bool {
    // Substitution expands generic applications, so the generic is only visible where it's used
    let root = substitute(arena, id);
    references_generic(arena, root, generic, |_| Vec::new())
}

/// The generics among `generics` that are referenced anywhere in the type, as `contains_generic`
/// finds them, in the order they're given. The type is only substituted once.
pub fn contained_generics(
    arena: &mut Arena<Type>,
    id: TypeId,
    generics: impl IntoIterator<Item = TypeId>,
) -> Vec<TypeId> {
    let generics: Vec<TypeId> = generics.into_iter().collect();

    // Most calls are to functions without generics, and there's nothing to substitute for them
    if generics.is_empty() {
        return generics;
    }

    let root = substitute(arena, id);

    generics
        .into_iter()
        .filter(|&generic| references_generic(arena, root, generic, |_| Vec::new()))
        .collect()
}

/// The generics bound by the function types anywhere in the type, such as `T` in
/// `(fn<T>(a: T) -> T, Int)`, in the order they're found.
pub fn function_type_generics(arena: &mut Arena<Type>, id: TypeId) -> Vec<TypeId> {
    // Substitution expands generic applications, such as an alias of a list of function types
    let root = substitute(arena, id);
    let mut generics = Vec::new();
    let mut visited = HashSet::new();
    let mut stack = vec![root];

    while let Some(id) = stack.pop() {
        if !visited.insert(id) {
            continue;
        }

        match &arena[id] {
            Type::Unresolved | Type::Generic(_) | Type::Any | Type::Never | Type::Atom(_) => {}
            Type::Ref(id) => stack.push(*id),
            Type::Pair(pair) => stack.extend([pair.first, pair.rest]),
            Type::Alias(alias) => stack.push(alias.inner),
            Type::Struct(ty) => stack.push(ty.inner),
            Type::Apply(apply) => {
                stack.push(apply.inner);
                stack.extend(apply.generics.values().copied());
            }
            Type::Function(function) => {
                generics.extend(function.generics.iter().copied());
                stack.push(function.ret);
                stack.extend(function.param_types());
            }
            Type::Union(union) => stack.extend(union.types.iter().copied()),
        }
    }

    generics
}

/// The generics that the type references other than where a function type in it binds them, as
/// `contains_generic` finds them, such as `T` but not `U` in `(T, fn<U>(a: U) -> U)`. The type is
/// only substituted once.
pub fn referenced_generics(arena: &mut Arena<Type>, id: TypeId) -> Vec<TypeId> {
    let root = substitute(arena, id);

    free_generics(arena, root)
        .into_iter()
        .filter(|&generic| references_generic(arena, root, generic, |_| Vec::new()))
        .collect()
}

/// Returns `true` if the type is unresolved, or contains an unresolved type anywhere inside of it
/// (such as an element of a pair or a field of a struct), because an error has already been
/// reported for it.
pub fn contains_unresolved(arena: &Arena<Type>, id: TypeId) -> bool {
    let mut visited = HashSet::new();
    let mut stack = vec![id];

    while let Some(id) = stack.pop() {
        if !visited.insert(id) {
            continue;
        }

        match &arena[id] {
            Type::Unresolved => return true,
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
                stack.push(function.ret);
                stack.extend(function.param_types());
            }
            Type::Union(union) => stack.extend(union.types.iter().copied()),
        }
    }

    false
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
                    stack.extend(function.param_types());
                }
            }
            Type::Union(union) => stack.extend(union.types.iter().copied()),
        }
    }

    false
}
