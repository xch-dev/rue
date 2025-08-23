use id_arena::Arena;

use crate::{Type, TypeId};

#[derive(Debug, Clone, Copy)]
pub struct PairInfo {
    pub id: Option<TypeId>,
    pub first: TypeId,
    pub rest: TypeId,
}

pub fn pairs_of(arena: &Arena<Type>, id: Option<TypeId>, ty: Type) -> Vec<PairInfo> {
    match ty {
        Type::Unresolved | Type::Apply(_) | Type::Generic => unreachable!(),
        Type::Ref(id) => pairs_of(arena, Some(id), arena[id].clone()),
        Type::Alias(alias) => pairs_of(arena, Some(alias.inner), arena[alias.inner].clone()),
        Type::Struct(ty) => pairs_of(arena, Some(ty.inner), arena[ty.inner].clone()),
        Type::Atom(_) => vec![],
        Type::Pair(pair) => vec![PairInfo {
            id,
            first: pair.first,
            rest: pair.rest,
        }],
        Type::Union(ty) => {
            let mut pairs = vec![];

            for &id in &ty.types {
                let inner = pairs_of(arena, Some(id), arena[id].clone());
                pairs.extend(inner);
            }

            pairs
        }
    }
}
