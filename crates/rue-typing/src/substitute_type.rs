use std::collections::{HashMap, HashSet};

use crate::{Alias, Type, TypeId, TypeSystem};

pub(crate) fn substitute_type(
    types: &mut TypeSystem,
    type_id: TypeId,
    substitutions: &mut HashMap<TypeId, TypeId>,
    preserve_semantics: bool,
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
        Type::Atom => type_id,
        Type::Bytes => type_id,
        Type::Bytes32 => type_id,
        Type::PublicKey => type_id,
        Type::Int => type_id,
        Type::Bool => type_id,
        Type::Nil => type_id,
        Type::Pair(first, rest) => {
            let (first, rest) = (*first, *rest);

            let new_first =
                substitute_type(types, first, substitutions, preserve_semantics, visited);
            let new_rest = substitute_type(types, rest, substitutions, preserve_semantics, visited);

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
                result.push(substitute_type(
                    types,
                    *item,
                    substitutions,
                    preserve_semantics,
                    visited,
                ));
            }

            if result == items {
                type_id
            } else {
                types.alloc(Type::Union(result))
            }
        }
        Type::Lazy(lazy) => {
            substitutions.extend(lazy.substitutions.clone());
            substitute_type(
                types,
                lazy.type_id,
                substitutions,
                preserve_semantics,
                visited,
            )
        }
        Type::Alias(alias) => {
            let alias = alias.clone();

            let new_type_id = substitute_type(
                types,
                alias.type_id,
                substitutions,
                preserve_semantics,
                visited,
            );

            if preserve_semantics {
                if new_type_id == alias.type_id {
                    type_id
                } else {
                    types.alloc(Type::Alias(Alias {
                        original_type_id: Some(alias.original_type_id.unwrap_or(type_id)),
                        type_id: new_type_id,
                        generic_types: alias.generic_types,
                    }))
                }
            } else {
                new_type_id
            }
        }
        Type::Struct(ty) => {
            let ty = ty.clone();

            let new_type_id = substitute_type(
                types,
                ty.type_id,
                substitutions,
                preserve_semantics,
                visited,
            );

            if preserve_semantics {
                if new_type_id == ty.type_id {
                    type_id
                } else {
                    types.alloc(Type::Struct(crate::Struct {
                        original_type_id: Some(ty.original_type_id.unwrap_or(type_id)),
                        type_id: new_type_id,
                        field_names: ty.field_names,
                        nil_terminated: ty.nil_terminated,
                        generic_types: ty.generic_types,
                    }))
                }
            } else {
                new_type_id
            }
        }
    };

    visited.remove(&type_id);

    result
}
