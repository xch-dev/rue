use id_arena::Arena;

use crate::{Pair, Type, TypeId};

pub fn pairs_of(arena: &Arena<Type>, id: TypeId) -> Vec<Pair> {
    match arena[id].clone() {
        Type::Unresolved | Type::Apply(_) => unreachable!(),
        Type::Alias(alias) => pairs_of(arena, alias.inner),
        Type::Struct(ty) => pairs_of(arena, ty.inner),
        Type::Never => vec![],
        Type::Any | Type::Generic => vec![Pair::new(id, id)],
        Type::List(_) => vec![],
        Type::Atom(_) | Type::Function(_) => vec![],
        Type::Pair(pair) => vec![pair],
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
