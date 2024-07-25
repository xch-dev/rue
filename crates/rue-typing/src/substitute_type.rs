use std::collections::HashMap;

use crate::{Alias, Callable, Enum, Struct, Type, TypeId, TypeSystem, Variant};

pub(crate) fn substitute_type(
    types: &mut TypeSystem,
    type_id: TypeId,
    substitutions: &mut HashMap<TypeId, TypeId>,
) -> TypeId {
    if let Some(new_type_id) = substitutions.get(&type_id) {
        return *new_type_id;
    }

    let placeholder = types.alloc(Type::Unknown);
    substitutions.insert(type_id, placeholder);

    let result = match types.get(type_id) {
        Type::Ref(..) => unreachable!(),
        Type::Unknown
        | Type::Generic
        | Type::Never
        | Type::Any
        | Type::Bytes
        | Type::Bytes32
        | Type::PublicKey
        | Type::Int
        | Type::Nil
        | Type::True
        | Type::False
        | Type::Value(..) => type_id,
        Type::Pair(first, rest) => {
            let (first, rest) = (*first, *rest);

            let new_first = substitute_type(types, first, substitutions);
            let new_rest = substitute_type(types, rest, substitutions);

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
                result.push(substitute_type(types, *item, substitutions));
            }

            if result == items {
                type_id
            } else {
                types.alloc(Type::Union(result))
            }
        }
        Type::Lazy(lazy) => {
            let mut old_substitutions = HashMap::new();
            for (key, val) in lazy.substitutions.clone() {
                if let Some(old) = substitutions.insert(key, val) {
                    old_substitutions.insert(key, old);
                }
            }
            substitutions.extend(lazy.substitutions.clone());
            let result = substitute_type(types, lazy.type_id, substitutions);
            substitutions.extend(old_substitutions);
            result
        }
        Type::Alias(alias) => {
            let alias = alias.clone();

            let new_type_id = substitute_type(types, alias.type_id, substitutions);

            if new_type_id == alias.type_id {
                type_id
            } else {
                types.alloc(Type::Alias(Alias {
                    original_type_id: alias.original_type_id,
                    type_id: new_type_id,
                    generic_types: alias.generic_types,
                }))
            }
        }
        Type::Struct(ty) => {
            let ty = ty.clone();

            let new_type_id = substitute_type(types, ty.type_id, substitutions);

            if new_type_id == ty.type_id {
                type_id
            } else {
                types.alloc(Type::Struct(Struct {
                    original_type_id: ty.original_type_id,
                    type_id: new_type_id,
                    field_names: ty.field_names,
                    rest: ty.rest,
                    generic_types: ty.generic_types,
                }))
            }
        }
        Type::Variant(ty) => {
            let ty = ty.clone();

            let new_type_id = substitute_type(types, ty.type_id, substitutions);

            if new_type_id == ty.type_id {
                type_id
            } else {
                types.alloc(Type::Variant(Variant {
                    original_type_id: ty.original_type_id,
                    original_enum_type_id: ty.original_enum_type_id,
                    type_id: new_type_id,
                    field_names: ty.field_names,
                    rest: ty.rest,
                    generic_types: ty.generic_types,
                    discriminant: ty.discriminant,
                }))
            }
        }
        Type::Enum(ty) => {
            let ty = ty.clone();

            let new_type_id = substitute_type(types, ty.type_id, substitutions);

            if new_type_id == ty.type_id {
                type_id
            } else {
                types.alloc(Type::Enum(Enum {
                    original_type_id: ty.original_type_id,
                    type_id: new_type_id,
                    has_fields: ty.has_fields,
                    variants: ty.variants,
                }))
            }
        }
        Type::Callable(callable) => {
            let callable = callable.clone();

            let new_return_type = substitute_type(types, callable.return_type, substitutions);

            let new_parameters = substitute_type(types, callable.parameters, substitutions);

            if new_return_type == callable.return_type && new_parameters == callable.parameters {
                type_id
            } else {
                types.alloc(Type::Callable(Callable {
                    original_type_id: callable.original_type_id,
                    parameter_names: callable.parameter_names,
                    parameters: new_parameters,
                    return_type: new_return_type,
                    rest: callable.rest,
                    generic_types: callable.generic_types,
                }))
            }
        }
    };

    *types.get_mut(placeholder) = Type::Ref(result);

    result
}
