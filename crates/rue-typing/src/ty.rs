use std::collections::{HashMap, HashSet};

use crate::{Alias, Lazy, TypeId, TypePath, TypeSystem};

#[derive(Debug, Clone)]
pub enum Type {
    Unknown,
    Generic,
    Never,
    Bytes,
    Bytes32,
    PublicKey,
    Int,
    Bool,
    Nil,
    Pair(TypeId, TypeId),
    Union(Vec<TypeId>),
    Ref(TypeId),
    Lazy(Lazy),
    Alias(Alias),
}

pub(crate) struct ComparisonContext<'a> {
    pub visited: HashSet<(TypeId, TypeId)>,
    pub substitution_stack: &'a mut Vec<HashMap<TypeId, TypeId>>,
    pub initial_substitution_length: usize,
    pub generic_stack_frame: Option<usize>,
}

pub(crate) fn replace_type(
    types: &mut TypeSystem,
    type_id: TypeId,
    replace_type_id: TypeId,
    path: &[TypePath],
) -> TypeId {
    if path.is_empty() {
        return replace_type_id;
    }

    match types.get(type_id) {
        Type::Pair(first, rest) => match path[0] {
            TypePath::First => replace_type(types, *first, replace_type_id, &path[1..]),
            TypePath::Rest => replace_type(types, *rest, replace_type_id, &path[1..]),
        },
        _ => type_id,
    }
}

pub(crate) fn structural_type(
    types: &mut TypeSystem,
    type_id: TypeId,
    substitutions: &mut HashMap<TypeId, TypeId>,
    visited: &mut HashSet<TypeId>,
) -> TypeId {
    if let Some(new_type_id) = substitutions.get(&type_id) {
        return *new_type_id;
    }

    if !visited.insert(type_id) {
        return type_id;
    }

    let result = match types.get(type_id) {
        Type::Ref(..) => unreachable!(),
        Type::Unknown => type_id,
        Type::Generic => type_id,
        Type::Never => type_id,
        Type::Bytes => type_id,
        Type::Bytes32 => type_id,
        Type::PublicKey => type_id,
        Type::Int => type_id,
        Type::Bool => type_id,
        Type::Nil => type_id,
        Type::Pair(first, rest) => {
            let (first, rest) = (*first, *rest);

            let new_first = structural_type(types, first, substitutions, visited);
            let new_rest = structural_type(types, rest, substitutions, visited);

            if new_first == first && new_rest == rest {
                type_id
            } else {
                types.alloc(Type::Pair(new_first, new_rest))
            }
        }
        Type::Union(items) => {
            let items = items.clone();
            let mut result = Vec::new();

            for item in &items {
                result.push(structural_type(types, *item, substitutions, visited));
            }

            if result == items {
                type_id
            } else {
                types.alloc(Type::Union(result))
            }
        }
        Type::Lazy(lazy) => {
            substitutions.extend(lazy.substitutions.clone());
            structural_type(types, lazy.type_id, substitutions, visited)
        }
        Type::Alias(alias) => alias.type_id,
    };

    visited.remove(&type_id);

    result
}
