use crate::{StandardTypes, Type, TypeId, TypeSystem};

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
