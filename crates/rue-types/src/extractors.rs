use std::collections::HashSet;

use id_arena::Arena;
use indexmap::{IndexSet, indexset};

use crate::{AtomRestriction, FunctionType, Pair, Struct, Type, TypeId, substitute};

#[derive(Debug, Clone)]
pub enum Atoms {
    Unrestricted,
    Restricted(IndexSet<AtomRestriction>),
}

pub fn extract_atoms(arena: &mut Arena<Type>, id: TypeId, strict: bool) -> Option<Atoms> {
    let id = substitute(arena, id);
    extract_atoms_impl(arena, id, strict, &mut HashSet::new())
}

fn extract_atoms_impl(
    arena: &Arena<Type>,
    id: TypeId,
    strict: bool,
    stack: &mut HashSet<TypeId>,
) -> Option<Atoms> {
    // A non-contractive cycle doesn't contribute anything
    if !stack.insert(id) {
        return None;
    }

    let result = match arena[id].clone() {
        Type::Apply(_) => unreachable!(),
        Type::Ref(id) => extract_atoms_impl(arena, id, strict, stack),
        Type::Unresolved => Some(Atoms::Unrestricted),
        Type::Generic(_) | Type::Never | Type::Function(_) | Type::Pair(_) | Type::Any => None,
        Type::Atom(atom) => atom
            .restriction
            .map_or(Some(Atoms::Unrestricted), |restriction| {
                Some(Atoms::Restricted(indexset![restriction]))
            }),
        Type::Struct(ty) => extract_atoms_impl(arena, ty.inner, strict, stack),
        Type::Alias(alias) => extract_atoms_impl(arena, alias.inner, strict, stack),
        Type::Union(ty) => {
            let mut result = None;

            for ty in ty.types {
                let inner = extract_atoms_impl(arena, ty, strict, stack);

                let inner = if strict {
                    inner?
                } else if let Some(inner) = inner {
                    inner
                } else {
                    continue;
                };

                match (&result, &inner) {
                    (None, _) | (Some(_), Atoms::Unrestricted) => result = Some(inner),
                    (Some(Atoms::Unrestricted), _) => {}
                    (Some(Atoms::Restricted(restrictions)), Atoms::Restricted(inner)) => {
                        let mut restrictions = restrictions.clone();
                        restrictions.extend(inner.clone());
                        result = Some(Atoms::Restricted(restrictions));
                    }
                }
            }

            result
        }
    };

    stack.remove(&id);

    result
}

pub fn extract_pairs(arena: &mut Arena<Type>, id: TypeId, strict: bool) -> Vec<Pair> {
    let id = substitute(arena, id);
    extract_pairs_impl(arena, id, strict, &mut HashSet::new()).unwrap_or_default()
}

fn extract_pairs_impl(
    arena: &Arena<Type>,
    id: TypeId,
    strict: bool,
    stack: &mut HashSet<TypeId>,
) -> Option<Vec<Pair>> {
    // A non-contractive cycle doesn't contribute anything
    if !stack.insert(id) {
        return Some(vec![]);
    }

    let result = match arena[id].clone() {
        Type::Apply(_) => unreachable!(),
        Type::Ref(id) => extract_pairs_impl(arena, id, strict, stack),
        Type::Unresolved => Some(vec![]),
        Type::Generic(_) | Type::Atom(_) | Type::Function(_) | Type::Any => None,
        Type::Never => Some(vec![]),
        Type::Pair(pair) => Some(vec![pair]),
        Type::Struct(ty) => extract_pairs_impl(arena, ty.inner, strict, stack),
        Type::Alias(alias) => extract_pairs_impl(arena, alias.inner, strict, stack),
        Type::Union(ty) => {
            let mut pairs = Vec::new();

            for ty in ty.types {
                let inner = extract_pairs_impl(arena, ty, strict, stack);

                if strict {
                    pairs.extend(inner?);
                } else {
                    pairs.extend(inner.unwrap_or_default());
                }
            }

            Some(pairs)
        }
    };

    stack.remove(&id);

    result
}

pub fn extract_functions(arena: &mut Arena<Type>, id: TypeId) -> Vec<FunctionType> {
    let id = substitute(arena, id);
    extract_functions_impl(arena, id, &mut HashSet::new()).unwrap_or_default()
}

fn extract_functions_impl(
    arena: &Arena<Type>,
    id: TypeId,
    stack: &mut HashSet<TypeId>,
) -> Option<Vec<FunctionType>> {
    // A non-contractive cycle doesn't contribute anything
    if !stack.insert(id) {
        return Some(vec![]);
    }

    let result = match arena[id].clone() {
        Type::Apply(_) => unreachable!(),
        Type::Ref(id) => extract_functions_impl(arena, id, stack),
        Type::Unresolved | Type::Never => Some(vec![]),
        Type::Generic(_) | Type::Atom(_) | Type::Pair(_) | Type::Any => None,
        Type::Function(function) => Some(vec![function]),
        Type::Struct(ty) => extract_functions_impl(arena, ty.inner, stack),
        Type::Alias(alias) => extract_functions_impl(arena, alias.inner, stack),
        Type::Union(ty) => {
            let mut pairs = Vec::new();

            for ty in ty.types {
                pairs.extend(extract_functions_impl(arena, ty, stack)?);
            }

            Some(pairs)
        }
    };

    stack.remove(&id);

    result
}

/// If every variant of the type is an instance of the same struct (such as a struct and a
/// narrowed copy of it), returns each of those instances.
pub fn extract_structs(arena: &mut Arena<Type>, id: TypeId) -> Option<Vec<Struct>> {
    let id = substitute(arena, id);

    let mut structs = Vec::new();

    if !extract_structs_impl(arena, id, &mut structs, &mut HashSet::new()) {
        return None;
    }

    let first = structs.first()?;

    if structs
        .iter()
        .any(|ty| ty.semantic != first.semantic || ty.fields != first.fields)
    {
        return None;
    }

    Some(structs)
}

fn extract_structs_impl(
    arena: &Arena<Type>,
    id: TypeId,
    structs: &mut Vec<Struct>,
    stack: &mut HashSet<TypeId>,
) -> bool {
    // A non-contractive cycle doesn't contribute anything
    if !stack.insert(id) {
        return true;
    }

    let result = match arena[id].clone() {
        Type::Apply(_) => unreachable!(),
        Type::Ref(id) => extract_structs_impl(arena, id, structs, stack),
        Type::Never => true,
        Type::Unresolved
        | Type::Generic(_)
        | Type::Atom(_)
        | Type::Pair(_)
        | Type::Function(_)
        | Type::Any => false,
        Type::Struct(ty) => {
            structs.push(ty);
            true
        }
        Type::Alias(alias) => extract_structs_impl(arena, alias.inner, structs, stack),
        Type::Union(ty) => ty
            .types
            .into_iter()
            .all(|ty| extract_structs_impl(arena, ty, structs, stack)),
    };

    stack.remove(&id);

    result
}
