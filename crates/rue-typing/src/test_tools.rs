use indexmap::{indexmap, IndexMap};

use crate::{Callable, Lazy, Rest, Struct, Type, TypeId, TypeSystem};

pub fn alloc_list(db: &mut TypeSystem, item_type_id: TypeId) -> TypeId {
    db.alloc(Type::Lazy(Lazy {
        type_id: db.std().unmapped_list,
        substitutions: indexmap! {
            db.std().generic_list_item => item_type_id,
        },
    }))
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

    let type_id = db.alloc(Type::Unknown);

    *db.get_mut(type_id) = Type::Callable(Callable {
        original_type_id: type_id,
        parameter_names: parameters.keys().cloned().collect(),
        parameters: structure,
        return_type,
        rest,
        generic_types: Vec::new(),
    });

    type_id
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

    let type_id = db.alloc(Type::Unknown);

    *db.get_mut(type_id) = Type::Struct(Struct {
        original_type_id: type_id,
        type_id: structure,
        field_names: fields.keys().cloned().collect(),
        rest,
        generic_types: Vec::new(),
    });

    type_id
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
