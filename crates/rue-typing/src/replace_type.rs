use crate::{Alias, Struct, Type, TypeId, TypePath, TypeSystem};

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
                original_type_id: Some(alias.original_type_id.unwrap_or(type_id)),
                type_id: new_type_id,
                generic_types: alias.generic_types,
            }))
        }
        Type::Struct(ty) => {
            let ty = ty.clone();
            let new_type_id = replace_type(types, ty.type_id, replace_type_id, path);
            types.alloc(Type::Struct(Struct {
                original_type_id: Some(ty.original_type_id.unwrap_or(type_id)),
                type_id: new_type_id,
                field_names: ty.field_names,
                rest: ty.rest,
                generic_types: ty.generic_types,
            }))
        }
        _ => type_id,
    }
}
