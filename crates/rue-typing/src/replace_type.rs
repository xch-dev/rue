use crate::{Alias, Enum, Struct, Type, TypeId, TypePath, TypeSystem, Variant};

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
        Type::Alias(alias) => {
            let alias = alias.clone();
            let new_type_id = replace_type(types, alias.type_id, replace_type_id, path);
            types.alloc(Type::Alias(Alias {
                original_type_id: alias.original_type_id,
                type_id: new_type_id,
                generic_types: alias.generic_types,
            }))
        }
        Type::Struct(ty) => {
            let ty = ty.clone();
            let new_type_id = replace_type(types, ty.type_id, replace_type_id, path);
            types.alloc(Type::Struct(Struct {
                original_type_id: ty.original_type_id,
                field_names: ty.field_names,
                type_id: new_type_id,
                nil_terminated: ty.nil_terminated,
                generic_types: ty.generic_types,
            }))
        }
        Type::Variant(ty) => {
            let ty = ty.clone();
            let new_type_id = replace_type(types, ty.type_id, replace_type_id, path);
            types.alloc(Type::Variant(Variant {
                original_type_id: ty.original_type_id,
                original_enum_type_id: ty.original_enum_type_id,
                field_names: ty.field_names,
                type_id: new_type_id,
                nil_terminated: ty.nil_terminated,
                generic_types: ty.generic_types,
                discriminant: ty.discriminant,
            }))
        }
        Type::Enum(ty) => {
            let ty = ty.clone();
            let new_type_id = replace_type(types, ty.type_id, replace_type_id, path);
            types.alloc(Type::Enum(Enum {
                original_type_id: ty.original_type_id,
                type_id: new_type_id,
                has_fields: ty.has_fields,
                variants: ty.variants,
            }))
        }
        _ => type_id,
    }
}
