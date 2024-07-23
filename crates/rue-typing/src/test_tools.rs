use indexmap::IndexMap;

use crate::{Callable, Rest, StandardTypes, Struct, Type, TypeId, TypeSystem};

pub fn alloc_list(db: &mut TypeSystem, types: &StandardTypes, item_type_id: TypeId) -> TypeId {
    let list = db.alloc(Type::Unknown);
    let pair = db.alloc(Type::Pair(item_type_id, list));
    *db.get_mut(list) = Type::Union(vec![pair, types.nil]);
    list
}

pub fn alloc_callable(
    db: &mut TypeSystem,
    parameters: IndexMap<String, TypeId>,
    return_type: TypeId,
    rest: Rest,
) -> TypeId {
    db.alloc(Type::Callable(Callable {
        original_type_id: None,
        parameters,
        return_type,
        rest,
        generic_types: Vec::new(),
    }))
}

pub fn alloc_struct(
    db: &mut TypeSystem,
    types: &StandardTypes,
    fields: &IndexMap<String, TypeId>,
    rest: Rest,
) -> TypeId {
    let structure = match rest {
        Rest::Nil => alloc_list_of(db, types, fields.values().copied()),
        Rest::Spread => alloc_tuple_of(db, types, fields.values().copied()),
        Rest::Optional => {
            let fields: Vec<TypeId> = fields
                .values()
                .copied()
                .enumerate()
                .map(|(i, field)| {
                    if i == fields.len() - 1 {
                        db.alloc(Type::Union(vec![field, types.nil]))
                    } else {
                        field
                    }
                })
                .collect();
            alloc_tuple_of(db, types, fields.into_iter())
        }
    };

    db.alloc(Type::Struct(Struct {
        original_type_id: None,
        type_id: structure,
        field_names: fields.keys().cloned().collect(),
        rest,
        generic_types: Vec::new(),
    }))
}

pub fn alloc_list_of(
    db: &mut TypeSystem,
    types: &StandardTypes,
    items: impl DoubleEndedIterator<Item = TypeId>,
) -> TypeId {
    let mut tuple = types.nil;
    for item in items.rev() {
        tuple = db.alloc(Type::Pair(item, tuple));
    }
    tuple
}

pub fn alloc_tuple_of(
    db: &mut TypeSystem,
    types: &StandardTypes,
    items: impl DoubleEndedIterator<Item = TypeId>,
) -> TypeId {
    let mut tuple = types.nil;
    for (i, item) in items.rev().enumerate() {
        if i == 0 {
            tuple = item;
            continue;
        }
        tuple = db.alloc(Type::Pair(item, tuple));
    }
    tuple
}
