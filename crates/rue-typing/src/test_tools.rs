use indexmap::IndexMap;

use crate::{StandardTypes, Struct, Type, TypeId, TypeSystem};

pub fn setup() -> (TypeSystem, StandardTypes) {
    let mut db = TypeSystem::new();
    let types = StandardTypes::alloc(&mut db);
    (db, types)
}

pub fn alloc_list(db: &mut TypeSystem, types: &StandardTypes, item_type_id: TypeId) -> TypeId {
    let list = db.alloc(Type::Unknown);
    let pair = db.alloc(Type::Pair(item_type_id, list));
    *db.get_mut(list) = Type::Union(vec![pair, types.nil]);
    list
}

pub fn alloc_struct(
    db: &mut TypeSystem,
    types: &StandardTypes,
    fields: &IndexMap<String, TypeId>,
    nil_terminated: bool,
) -> TypeId {
    let structure = if nil_terminated {
        alloc_list_of(db, types, fields.values().copied())
    } else {
        alloc_tuple_of(db, types, fields.values().copied())
    };

    db.alloc(Type::Struct(Struct {
        original_type_id: None,
        type_id: structure,
        field_names: fields.keys().cloned().collect(),
        nil_terminated,
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
