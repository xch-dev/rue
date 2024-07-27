use indexmap::{indexmap, IndexMap};

use crate::{Callable, Lazy, Struct, Type, TypeId, TypeSystem};

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
    nil_terminated: bool,
) -> TypeId {
    let structure = if nil_terminated {
        alloc_list_of(db, parameters.values().copied())
    } else {
        alloc_tuple_of(db, parameters.values().copied())
    };

    let type_id = db.alloc(Type::Unknown);

    *db.get_mut(type_id) = Type::Callable(Callable {
        original_type_id: type_id,
        parameter_names: parameters.keys().cloned().collect(),
        parameters: structure,
        return_type,
        nil_terminated,
        generic_types: Vec::new(),
    });

    type_id
}

pub fn alloc_struct(
    db: &mut TypeSystem,
    fields: &IndexMap<String, TypeId>,
    nil_terminated: bool,
) -> TypeId {
    let structure = if nil_terminated {
        alloc_list_of(db, fields.values().copied())
    } else {
        alloc_tuple_of(db, fields.values().copied())
    };

    let type_id = db.alloc(Type::Unknown);

    *db.get_mut(type_id) = Type::Struct(Struct {
        original_type_id: type_id,
        type_id: structure,
        field_names: fields.keys().cloned().collect(),
        nil_terminated,
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
