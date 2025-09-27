use id_arena::Arena;
use indexmap::{IndexSet, indexset};

use crate::{AtomRestriction, FunctionType, Pair, Type, TypeId, substitute};

#[derive(Debug, Clone)]
pub enum Atoms {
    Unrestricted,
    Restricted(IndexSet<AtomRestriction>),
}

pub fn extract_atoms(arena: &mut Arena<Type>, id: TypeId, strict: bool) -> Option<Atoms> {
    let id = substitute(arena, id);
    extract_atoms_impl(arena, id, strict)
}

fn extract_atoms_impl(arena: &Arena<Type>, id: TypeId, strict: bool) -> Option<Atoms> {
    match arena[id].clone() {
        Type::Apply(_) => unreachable!(),
        Type::Ref(id) => extract_atoms_impl(arena, id, strict),
        Type::Unresolved => Some(Atoms::Unrestricted),
        Type::Generic(_) | Type::Never | Type::Function(_) | Type::Pair(_) => None,
        Type::Atom(atom) => atom
            .restriction
            .map_or(Some(Atoms::Unrestricted), |restriction| {
                Some(Atoms::Restricted(indexset![restriction]))
            }),
        Type::Struct(ty) => extract_atoms_impl(arena, ty.inner, strict),
        Type::Alias(alias) => extract_atoms_impl(arena, alias.inner, strict),
        Type::Union(ty) => {
            let mut result = None;

            for ty in ty.types {
                let inner = extract_atoms_impl(arena, ty, strict);

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
    }
}

pub fn extract_pairs(arena: &mut Arena<Type>, id: TypeId, strict: bool) -> Vec<Pair> {
    let id = substitute(arena, id);
    extract_pairs_impl(arena, id, strict).unwrap_or_default()
}

fn extract_pairs_impl(arena: &Arena<Type>, id: TypeId, strict: bool) -> Option<Vec<Pair>> {
    match arena[id].clone() {
        Type::Apply(_) => unreachable!(),
        Type::Ref(id) => extract_pairs_impl(arena, id, strict),
        Type::Unresolved => Some(vec![]),
        Type::Generic(_) | Type::Never | Type::Atom(_) | Type::Function(_) => None,
        Type::Pair(pair) => Some(vec![pair]),
        Type::Struct(ty) => extract_pairs_impl(arena, ty.inner, strict),
        Type::Alias(alias) => extract_pairs_impl(arena, alias.inner, strict),
        Type::Union(ty) => {
            let mut pairs = Vec::new();

            for ty in ty.types {
                let inner = extract_pairs_impl(arena, ty, strict);

                if strict {
                    pairs.extend(inner?);
                } else {
                    pairs.extend(inner.unwrap_or_default());
                }
            }

            Some(pairs)
        }
    }
}

pub fn extract_functions(arena: &mut Arena<Type>, id: TypeId) -> Vec<FunctionType> {
    let id = substitute(arena, id);
    extract_functions_impl(arena, id).unwrap_or_default()
}

fn extract_functions_impl(arena: &Arena<Type>, id: TypeId) -> Option<Vec<FunctionType>> {
    match arena[id].clone() {
        Type::Apply(_) => unreachable!(),
        Type::Ref(id) => extract_functions_impl(arena, id),
        Type::Unresolved | Type::Never => Some(vec![]),
        Type::Generic(_) | Type::Atom(_) | Type::Pair(_) => None,
        Type::Function(function) => Some(vec![function]),
        Type::Struct(ty) => extract_functions_impl(arena, ty.inner),
        Type::Alias(alias) => extract_functions_impl(arena, alias.inner),
        Type::Union(ty) => {
            let mut pairs = Vec::new();

            for ty in ty.types {
                pairs.extend(extract_functions_impl(arena, ty)?);
            }

            Some(pairs)
        }
    }
}
