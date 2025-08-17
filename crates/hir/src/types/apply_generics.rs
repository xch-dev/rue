use std::collections::HashMap;

use crate::{Alias, Database, FunctionType, Type, TypeId};

pub fn apply_generics(db: &mut Database, ty: TypeId, map: &HashMap<TypeId, TypeId>) -> TypeId {
    if let Some(mapped) = map.get(&ty) {
        return *mapped;
    }

    match db.ty(ty).clone() {
        Type::Unresolved | Type::Atom(..) | Type::Generic(..) => ty,
        Type::Alias(alias) => {
            let inner = apply_generics(db, alias.inner, map);
            db.alloc_type(Type::Alias(Alias {
                name: alias.name,
                scope: alias.scope,
                vars: alias.vars,
                inner,
            }))
        }
        Type::Pair(first, rest) => {
            let first = apply_generics(db, first, map);
            let rest = apply_generics(db, rest, map);
            db.alloc_type(Type::Pair(first, rest))
        }
        Type::Union(ids) => {
            let ids = ids.iter().map(|id| apply_generics(db, *id, map)).collect();
            db.alloc_type(Type::Union(ids))
        }
        Type::Fn(function) => {
            let params = function
                .params
                .iter()
                .map(|id| apply_generics(db, *id, map))
                .collect();
            let ret = apply_generics(db, function.ret, map);
            db.alloc_type(Type::Fn(FunctionType { params, ret }))
        }
    }
}
