use rue_types::{Alias, BuiltinTypes, Pair, Type, TypeId, Union};

use crate::{Database, TypePath};

/// Narrows the type at the given path within `from`, given that the value at that path is known
/// to be of type `to`. The type at the path becomes the intersection of its current type and `to`,
/// so that narrowing which has already been applied isn't lost.
pub fn replace_type(
    db: &mut Database,
    builtins: &BuiltinTypes,
    from: TypeId,
    to: TypeId,
    path: &[TypePath],
) -> TypeId {
    let Some(next) = path.first().copied() else {
        return rue_types::intersect(db.types_mut(), builtins, from, to);
    };

    match db.ty(from).clone() {
        Type::Unresolved
        | Type::Generic(_)
        | Type::Atom(..)
        | Type::Function(_)
        | Type::Never
        | Type::Any => from,
        Type::Ref(id) => replace_type(db, builtins, id, to, path),
        // The instantiated type is narrowed, rather than the generic definition
        Type::Apply(_) => {
            let from = rue_types::substitute(db.types_mut(), from);
            replace_type(db, builtins, from, to, path)
        }
        Type::Alias(alias) => {
            let inner = replace_type(db, builtins, alias.inner, to, path);
            db.alloc_type(Type::Alias(Alias { inner, ..alias }))
        }
        Type::Struct(ty) => {
            let inner = replace_type(db, builtins, ty.inner, to, path);
            let ty = ty.narrow(db.types_mut(), builtins, inner);
            db.alloc_type(Type::Struct(ty))
        }
        Type::Union(ty) => {
            let mut new_variants = Vec::with_capacity(ty.types.len());
            for variant in ty.types {
                let variant = replace_type(db, builtins, variant, to, path);
                new_variants.push(variant);
            }
            db.alloc_type(Type::Union(Union::new(new_variants)))
        }
        Type::Pair(pair) => match next {
            TypePath::First => {
                let first = replace_type(db, builtins, pair.first, to, &path[1..]);
                db.alloc_type(Type::Pair(Pair::new(first, pair.rest)))
            }
            TypePath::Rest => {
                let rest = replace_type(db, builtins, pair.rest, to, &path[1..]);
                db.alloc_type(Type::Pair(Pair::new(pair.first, rest)))
            }
        },
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use indexmap::IndexSet;
    use rue_types::{Alias, Apply, BuiltinTypes, Generic, Pair, Struct, Type, TypeId, Union};

    use super::*;

    struct Ctx {
        db: Database,
        builtins: BuiltinTypes,
    }

    impl Ctx {
        fn new() -> Self {
            let mut db = Database::new();
            let builtins = BuiltinTypes::new(db.types_mut());
            Self { db, builtins }
        }

        fn pair(&mut self, first: TypeId, rest: TypeId) -> TypeId {
            self.db.alloc_type(Type::Pair(Pair::new(first, rest)))
        }

        fn union(&mut self, types: Vec<TypeId>) -> TypeId {
            self.db.alloc_type(Type::Union(Union::new(types)))
        }

        fn replace(&mut self, from: TypeId, to: TypeId, path: &[TypePath]) -> TypeId {
            replace_type(&mut self.db, &self.builtins, from, to, path)
        }

        fn name(&mut self, ty: TypeId) -> String {
            rue_types::stringify(self.db.types_mut(), ty)
        }
    }

    #[test]
    fn test_replace_root() {
        let mut ctx = Ctx::new();
        let b = ctx.builtins;

        // The root is narrowed to the intersection of the current type and the replacement
        let pair = ctx.pair(b.int, b.int);
        let from = ctx.union(vec![b.bytes32, b.nil, pair]);
        let ty = ctx.replace(from, b.atom, &[]);
        assert_eq!(ctx.name(ty), "Bytes32 | nil");

        let ty = ctx.replace(from, b.bytes32, &[]);
        assert_eq!(ctx.name(ty), "Bytes32");

        // If the current type is permissive, the replacement is used as is
        let ty = ctx.replace(b.permissive_any, b.int, &[]);
        assert_eq!(ctx.name(ty), "Int");
    }

    #[test]
    fn test_replace_pair() {
        let mut ctx = Ctx::new();
        let b = ctx.builtins;

        let first = ctx.union(vec![b.bytes32, b.nil]);
        let rest = ctx.union(vec![b.int, b.nil]);
        let from = ctx.pair(first, rest);

        let ty = ctx.replace(from, b.bytes32, &[TypePath::First]);
        assert_eq!(ctx.name(ty), "(Bytes32, Int | nil)");

        let ty = ctx.replace(from, b.nil, &[TypePath::Rest]);
        assert_eq!(ctx.name(ty), "(Bytes32 | nil, nil)");
    }

    #[test]
    fn test_replace_nested_pair() {
        let mut ctx = Ctx::new();
        let b = ctx.builtins;

        let inner_first = ctx.union(vec![b.bytes32, b.nil]);
        let inner = ctx.pair(inner_first, b.int);
        let from = ctx.pair(b.int, inner);

        let ty = ctx.replace(from, b.nil, &[TypePath::Rest, TypePath::First]);
        assert_eq!(ctx.name(ty), "(Int, (nil, Int))");
    }

    #[test]
    fn test_replace_union() {
        let mut ctx = Ctx::new();
        let b = ctx.builtins;

        // Every pair variant is narrowed, and atom variants are left alone
        let first = ctx.union(vec![b.bytes32, b.nil]);
        let pair_a = ctx.pair(first, b.int);
        let pair_b = ctx.pair(first, b.nil);
        let from = ctx.union(vec![pair_a, pair_b, b.nil]);

        let ty = ctx.replace(from, b.bytes32, &[TypePath::First]);
        assert_eq!(ctx.name(ty), "(Bytes32, Int) | (Bytes32, nil) | nil");
    }

    #[test]
    fn test_replace_alias() {
        let mut ctx = Ctx::new();
        let b = ctx.builtins;

        let first = ctx.union(vec![b.bytes32, b.nil]);
        let inner = ctx.pair(first, b.int);
        let from = ctx.db.alloc_type(Type::Alias(Alias {
            name: None,
            inner,
            generics: vec![],
        }));

        let ty = ctx.replace(from, b.nil, &[TypePath::First]);

        let Type::Alias(alias) = ctx.db.ty(ty).clone() else {
            panic!("expected alias");
        };

        assert_eq!(ctx.name(alias.inner), "(nil, Int)");
    }

    #[test]
    fn test_replace_struct() {
        let mut ctx = Ctx::new();
        let b = ctx.builtins;

        let first = ctx.union(vec![b.bytes32, b.nil]);
        let inner = ctx.pair(first, b.nil);
        let semantic = ctx.db.alloc_type(Type::Unresolved);
        *ctx.db.ty_mut(semantic) = Type::Struct(Struct {
            name: None,
            semantic,
            inner,
            generics: vec![],
            fields: IndexSet::from(["value".to_string()]),
            nil_terminated: true,
            narrowed: false,
        });

        let ty = ctx.replace(semantic, b.bytes32, &[TypePath::First]);

        let Type::Struct(ty) = ctx.db.ty(ty).clone() else {
            panic!("expected struct");
        };

        assert_eq!(ty.semantic, semantic);
        assert_eq!(ctx.name(ty.inner), "(Bytes32, nil)");
    }

    #[test]
    fn test_replace_generic() {
        let mut ctx = Ctx::new();
        let b = ctx.builtins;

        let generic = ctx.db.alloc_type(Type::Generic(Generic { name: None }));

        // There's no structure to narrow within a generic type
        let ty = ctx.replace(generic, b.int, &[TypePath::First]);
        assert_eq!(ty, generic);

        // But the generic type itself can be narrowed
        let ty = ctx.replace(generic, b.int, &[]);
        assert_eq!(ctx.name(ty), "Int");
    }

    #[test]
    fn test_replace_apply() {
        let mut ctx = Ctx::new();
        let b = ctx.builtins;

        let generic = ctx.db.alloc_type(Type::Generic(Generic { name: None }));
        let first = ctx.union(vec![b.bytes32, b.nil]);
        let inner = ctx.pair(first, generic);
        let from = ctx.db.alloc_type(Type::Apply(Apply::new(
            inner,
            HashMap::from([(generic, b.int)]),
        )));

        // The instantiated type is narrowed
        let ty = ctx.replace(from, b.bytes32, &[TypePath::First]);
        assert_eq!(ctx.name(ty), "(Bytes32, Int)");

        // The value at a generic position can be narrowed as well
        let ty = ctx.replace(from, b.nil, &[TypePath::Rest]);
        assert_eq!(ctx.name(ty), "(Bytes32 | nil, nil)");
    }

    #[test]
    fn test_replace_atom_path() {
        let mut ctx = Ctx::new();
        let b = ctx.builtins;

        // Atoms don't have a first or rest, so there's nothing to narrow
        let ty = ctx.replace(b.int, b.nil, &[TypePath::First]);
        assert_eq!(ty, b.int);
    }
}
