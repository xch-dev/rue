use rue_types::{Alias, Apply, Pair, Struct, Type, TypeId, Union};

use crate::{Database, TypePath};

pub fn replace_type(db: &mut Database, from: TypeId, to: TypeId, path: &[TypePath]) -> TypeId {
    let Some(next) = path.first().copied() else {
        return to;
    };

    match db.ty(from).clone() {
        Type::Unresolved | Type::Generic(_) | Type::Atom(..) | Type::Function(_) | Type::Never => {
            from
        }
        Type::Ref(id) => replace_type(db, id, to, path),
        Type::Apply(apply) => {
            let inner = replace_type(db, apply.inner, to, path);
            db.alloc_type(Type::Apply(Apply::new(inner, apply.generics)))
        }
        Type::Alias(alias) => {
            let inner = replace_type(db, alias.inner, to, path);
            db.alloc_type(Type::Alias(Alias { inner, ..alias }))
        }
        Type::Struct(ty) => {
            let inner = replace_type(db, ty.inner, to, path);
            db.alloc_type(Type::Struct(Struct { inner, ..ty }))
        }
        Type::Union(ty) => {
            let mut new_variants = Vec::with_capacity(ty.types.len());
            for variant in ty.types {
                let variant = replace_type(db, variant, to, path);
                new_variants.push(variant);
            }
            db.alloc_type(Type::Union(Union::new(new_variants)))
        }
        Type::Pair(pair) => match next {
            TypePath::First => {
                let first = replace_type(db, pair.first, to, &path[1..]);
                db.alloc_type(Type::Pair(Pair::new(first, pair.rest)))
            }
            TypePath::Rest => {
                let rest = replace_type(db, pair.rest, to, &path[1..]);
                db.alloc_type(Type::Pair(Pair::new(pair.first, rest)))
            }
        },
    }
}
