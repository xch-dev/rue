use crate::{Alias, Database, Type, TypeId, TypePath};

pub fn replace_type(db: &mut Database, from: TypeId, to: TypeId, path: &[TypePath]) -> TypeId {
    let Some(next) = path.first().copied() else {
        return to;
    };

    match db.ty(from).clone() {
        Type::Unresolved | Type::Atom(..) | Type::Generic(..) => from,
        Type::Alias(alias) => {
            let inner = replace_type(db, alias.inner, to, path);
            db.alloc_type(Type::Alias(Alias {
                inner,
                name: alias.name,
                scope: alias.scope,
                vars: alias.vars,
            }))
        }
        Type::Union(variants) => {
            let mut new_variants = Vec::with_capacity(variants.len());
            for variant in variants {
                let variant = replace_type(db, variant, to, path);
                new_variants.push(variant);
            }
            db.alloc_type(Type::Union(new_variants))
        }
        Type::Pair(first, rest) => match next {
            TypePath::First => {
                let first = replace_type(db, first, to, &path[1..]);
                db.alloc_type(Type::Pair(first, rest))
            }
            TypePath::Rest => {
                let rest = replace_type(db, rest, to, &path[1..]);
                db.alloc_type(Type::Pair(first, rest))
            }
        },
    }
}
