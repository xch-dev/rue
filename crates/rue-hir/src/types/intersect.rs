use std::collections::HashSet;

use rue_types::{BuiltinTypes, Check, Comparison, Type, TypeId, Union};

use crate::Database;

/// Computes a type which contains every value that is in both `a` and `b`.
///
/// The result may be wider than the exact intersection, but it is never narrower. This makes it
/// sound to narrow a value to the result whenever it's known to be of both types at once (for
/// example, if two type guards on the same path have both succeeded or both failed). If no better
/// approximation can be found, `b` is returned.
pub fn intersect_types(db: &mut Database, builtins: &BuiltinTypes, a: TypeId, b: TypeId) -> TypeId {
    if a == b {
        return a;
    }

    // `Any` is permissive, so it's assignable to everything and can't be used for comparisons
    if is_any(db, a) {
        return b;
    }

    if is_any(db, b) {
        return a;
    }

    if is_assignable(db, builtins, a, b) {
        return a;
    }

    if is_assignable(db, builtins, b, a) {
        return b;
    }

    if let Some(ty) = filter_disjoint_variants(db, builtins, a, b) {
        return ty;
    }

    if let Some(ty) = filter_disjoint_variants(db, builtins, b, a) {
        return ty;
    }

    b
}

fn is_any(db: &mut Database, ty: TypeId) -> bool {
    let ty = rue_types::unwrap_semantic(db.types_mut(), ty, true);
    matches!(db.ty(ty), Type::Any)
}

/// Removes the variants of `ty` which can't overlap with `other`. Returns `None` if no variants
/// could be removed, since there's nothing to gain in that case.
fn filter_disjoint_variants(
    db: &mut Database,
    builtins: &BuiltinTypes,
    ty: TypeId,
    other: TypeId,
) -> Option<TypeId> {
    let mut variants = Vec::new();
    flatten_union(db, ty, &mut variants, &mut HashSet::new());

    let mut kept = Vec::new();

    for &variant in &variants {
        if !is_disjoint(db, builtins, variant, other) {
            kept.push(variant);
        }
    }

    if kept.len() == variants.len() {
        return None;
    }

    Some(match kept.len() {
        0 => builtins.never,
        1 => kept[0],
        _ => db.alloc_type(Type::Union(Union::new(kept))),
    })
}

fn flatten_union(
    db: &mut Database,
    ty: TypeId,
    variants: &mut Vec<TypeId>,
    visited: &mut HashSet<TypeId>,
) {
    let ty = rue_types::unwrap_semantic(db.types_mut(), ty, true);

    if !visited.insert(ty) {
        // A recursive union can't be flattened any further, so keep it as a single variant
        variants.push(ty);
        return;
    }

    if let Type::Union(union) = db.ty(ty).clone() {
        for variant in union.types {
            flatten_union(db, variant, variants, visited);
        }
    } else {
        variants.push(ty);
    }
}

fn is_assignable(db: &mut Database, builtins: &BuiltinTypes, from: TypeId, to: TypeId) -> bool {
    rue_types::compare(db.types_mut(), builtins, from, to) == Comparison::Assign
}

/// Whether no value of type `ty` can also be a value of type `other`. If this can't be
/// determined, the types are conservatively assumed to overlap.
fn is_disjoint(db: &mut Database, builtins: &BuiltinTypes, ty: TypeId, other: TypeId) -> bool {
    rue_types::check(db.types_mut(), builtins, ty, other)
        .is_ok_and(|check| check.simplify() == Check::Impossible)
}

#[cfg(test)]
mod tests {
    use rue_types::{Generic, Pair};

    use super::*;

    fn setup() -> (Database, BuiltinTypes) {
        let mut db = Database::new();
        let builtins = BuiltinTypes::new(db.types_mut());
        (db, builtins)
    }

    fn name(db: &mut Database, ty: TypeId) -> String {
        rue_types::stringify(db.types_mut(), ty)
    }

    #[test]
    fn test_intersect_subset() {
        let (mut db, b) = setup();

        let ty = intersect_types(&mut db, &b, b.bytes32, b.bytes);
        assert_eq!(ty, b.bytes32);

        let ty = intersect_types(&mut db, &b, b.bytes, b.bytes32);
        assert_eq!(ty, b.bytes32);

        let ty = intersect_types(&mut db, &b, b.int, b.int);
        assert_eq!(ty, b.int);
    }

    #[test]
    fn test_intersect_unions() {
        let (mut db, b) = setup();

        let pair = db.alloc_type(Type::Pair(Pair::new(b.int, b.int)));
        let a = db.alloc_type(Type::Union(Union::new(vec![b.bytes32, b.nil, pair])));
        let c = db.alloc_type(Type::Union(Union::new(vec![b.nil, pair])));
        let d = db.alloc_type(Type::Union(Union::new(vec![b.bytes32, pair])));

        let ty = intersect_types(&mut db, &b, a, c);
        assert_eq!(name(&mut db, ty), "nil | (Int, Int)");

        let ty = intersect_types(&mut db, &b, c, d);
        assert_eq!(name(&mut db, ty), "(Int, Int)");

        let ty = intersect_types(&mut db, &b, a, b.atom);
        assert_eq!(name(&mut db, ty), "Bytes32 | nil");
    }

    #[test]
    fn test_intersect_disjoint() {
        let (mut db, b) = setup();

        let pair = db.alloc_type(Type::Pair(Pair::new(b.int, b.int)));
        let ty = intersect_types(&mut db, &b, b.bytes32, pair);
        assert_eq!(ty, b.never);
    }

    #[test]
    fn test_intersect_any() {
        let (mut db, b) = setup();

        let ty = intersect_types(&mut db, &b, b.permissive_any, b.int);
        assert_eq!(ty, b.int);

        let ty = intersect_types(&mut db, &b, b.int, b.permissive_any);
        assert_eq!(ty, b.int);
    }

    #[test]
    fn test_intersect_fallback() {
        let (mut db, b) = setup();

        // Nothing is known about how a generic type relates to the other type, so the second
        // type is used as is
        let generic = db.alloc_type(Type::Generic(Generic { name: None }));
        let ty = intersect_types(&mut db, &b, generic, b.int);
        assert_eq!(ty, b.int);
    }
}
