use id_arena::Arena;

use crate::{FunctionType, Pair, Type, TypeId, substitute};

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
