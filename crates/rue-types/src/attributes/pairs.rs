use id_arena::Arena;

use crate::{Type, TypeId};

#[derive(Debug, Clone, Copy)]
pub struct PairInfo {
    pub id: TypeId,
    pub first: TypeId,
    pub rest: TypeId,
}

pub fn pairs_of(arena: &Arena<Type>, id: TypeId) -> Vec<PairInfo> {
    match arena[id].clone() {
        Type::Unresolved | Type::Apply(_) | Type::Generic => unreachable!(),
        Type::Alias(alias) => pairs_of(arena, alias.inner),
        Type::Struct(ty) => pairs_of(arena, ty.inner),
        Type::Never => vec![],
        Type::Any => vec![PairInfo {
            id,
            first: id,
            rest: id,
        }],
        Type::List(_) => vec![],
        Type::Atom(_) | Type::Function(_) => vec![],
        Type::Pair(pair) => vec![PairInfo {
            id,
            first: pair.first,
            rest: pair.rest,
        }],
        Type::Union(ty) => {
            let mut pairs = vec![];

            for &id in &ty.types {
                let inner = pairs_of(arena, id);
                pairs.extend(inner);
            }

            pairs
        }
    }
}
