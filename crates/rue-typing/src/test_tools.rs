use indexmap::IndexMap;

use crate::{Callable, Rest, Struct, Type, TypeId, TypeSystem};

pub fn alloc_list(db: &mut TypeSystem, item_type_id: TypeId) -> TypeId {
    let list = db.alloc(Type::Unknown);
    let pair = db.alloc(Type::Pair(item_type_id, list));
    *db.get_mut(list) = Type::Union(vec![pair, db.std().nil]);
    list
}

pub fn alloc_callable(
    db: &mut TypeSystem,
    parameters: &IndexMap<String, TypeId>,
    return_type: TypeId,
    rest: Rest,
) -> TypeId {
    let structure = match rest {
        Rest::Nil => alloc_list_of(db, parameters.values().copied()),
        Rest::Spread => alloc_tuple_of(db, parameters.values().copied()),
        Rest::Optional => {
            let parameters: Vec<TypeId> = parameters
                .values()
                .copied()
                .enumerate()
                .map(|(i, field)| {
                    if i == parameters.len() - 1 {
                        db.alloc(Type::Union(vec![field, db.std().nil]))
                    } else {
                        field
                    }
                })
                .collect();
            alloc_tuple_of(db, parameters.into_iter())
        }
    };

    db.alloc(Type::Callable(Callable {
        original_type_id: None,
        parameter_names: parameters.keys().cloned().collect(),
        parameters: structure,
        return_type,
        rest,
        generic_types: Vec::new(),
    }))
}

pub fn alloc_struct(db: &mut TypeSystem, fields: &IndexMap<String, TypeId>, rest: Rest) -> TypeId {
    let structure = match rest {
        Rest::Nil => alloc_list_of(db, fields.values().copied()),
        Rest::Spread => alloc_tuple_of(db, fields.values().copied()),
        Rest::Optional => {
            let fields: Vec<TypeId> = fields
                .values()
                .copied()
                .enumerate()
                .map(|(i, field)| {
                    if i == fields.len() - 1 {
                        db.alloc(Type::Union(vec![field, db.std().nil]))
                    } else {
                        field
                    }
                })
                .collect();
            alloc_tuple_of(db, fields.into_iter())
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
    items: impl DoubleEndedIterator<Item = TypeId>,
) -> TypeId {
    let mut tuple = db.std().nil;
    for item in items.rev() {
        tuple = db.alloc(Type::Pair(item, tuple));
    }
    tuple
}

pub fn alloc_tuple_of(
    db: &mut TypeSystem,
    items: impl DoubleEndedIterator<Item = TypeId>,
) -> TypeId {
    let mut tuple = db.std().nil;
    for (i, item) in items.rev().enumerate() {
        if i == 0 {
            tuple = item;
            continue;
        }
        tuple = db.alloc(Type::Pair(item, tuple));
    }
    tuple
}
