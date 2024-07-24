use std::collections::HashMap;

use crate::{Alias, Callable, Enum, Struct, Type, TypeId, TypeSystem, Variant};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Semantics {
    Preserve,
    StructuralOnly { callable: TypeId },
}

pub(crate) fn substitute_type(
    types: &mut TypeSystem,
    type_id: TypeId,
    substitutions: &mut HashMap<TypeId, TypeId>,
    semantics: Semantics,
) -> TypeId {
    if let Some(new_type_id) = substitutions.get(&type_id) {
        return *new_type_id;
    }

    let placeholder = types.alloc(Type::Unknown);
    substitutions.insert(type_id, placeholder);

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
        Type::Nil => type_id,
        Type::True => type_id,
        Type::False => type_id,
        Type::Value(..) => type_id,
        Type::Pair(first, rest) => {
            let (first, rest) = (*first, *rest);

            let new_first = substitute_type(types, first, substitutions, semantics);
            let new_rest = substitute_type(types, rest, substitutions, semantics);

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
                result.push(substitute_type(types, *item, substitutions, semantics));
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
            let result = substitute_type(types, lazy.type_id, substitutions, semantics);
            substitutions.extend(old_substitutions);
            result
        }
        Type::Alias(alias) => {
            let alias = alias.clone();

            let new_type_id = substitute_type(types, alias.type_id, substitutions, semantics);

            if semantics == Semantics::Preserve {
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

            let new_type_id = substitute_type(types, ty.type_id, substitutions, semantics);

            if semantics == Semantics::Preserve {
                if new_type_id == ty.type_id {
                    type_id
                } else {
                    types.alloc(Type::Struct(Struct {
                        original_type_id: Some(ty.original_type_id.unwrap_or(type_id)),
                        type_id: new_type_id,
                        field_names: ty.field_names,
                        rest: ty.rest,
                        generic_types: ty.generic_types,
                    }))
                }
            } else {
                new_type_id
            }
        }
        Type::Variant(ty) => {
            let ty = ty.clone();

            let new_type_id = substitute_type(types, ty.type_id, substitutions, semantics);

            if semantics == Semantics::Preserve {
                if new_type_id == ty.type_id {
                    type_id
                } else {
                    types.alloc(Type::Variant(Variant {
                        original_type_id: Some(ty.original_type_id.unwrap_or(type_id)),
                        enum_type: ty.enum_type,
                        type_id: new_type_id,
                        field_names: ty.field_names,
                        rest: ty.rest,
                        generic_types: ty.generic_types,
                        discriminant: ty.discriminant,
                    }))
                }
            } else {
                new_type_id
            }
        }
        Type::Enum(ty) => {
            let ty = ty.clone();

            let new_type_id = substitute_type(types, ty.type_id, substitutions, semantics);

            if semantics == Semantics::Preserve {
                if new_type_id == ty.type_id {
                    type_id
                } else {
                    types.alloc(Type::Enum(Enum {
                        original_type_id: Some(ty.original_type_id.unwrap_or(type_id)),
                        type_id: new_type_id,
                        has_fields: ty.has_fields,
                        variants: ty.variants,
                    }))
                }
            } else {
                new_type_id
            }
        }
        Type::Callable(callable) => {
            let callable = callable.clone();

            let new_return_type =
                substitute_type(types, callable.return_type, substitutions, semantics);

            let new_parameters =
                substitute_type(types, callable.parameters, substitutions, semantics);

            match semantics {
                Semantics::Preserve => {
                    if new_return_type == callable.return_type
                        && new_parameters == callable.parameters
                    {
                        type_id
                    } else {
                        types.alloc(Type::Callable(Callable {
                            original_type_id: Some(callable.original_type_id.unwrap_or(type_id)),
                            parameter_names: callable.parameter_names,
                            parameters: new_parameters,
                            return_type: new_return_type,
                            rest: callable.rest,
                            generic_types: callable.generic_types,
                        }))
                    }
                }
                Semantics::StructuralOnly { callable } => callable,
            }
        }
    };

    *types.get_mut(placeholder) = Type::Ref(result);

    result
}
