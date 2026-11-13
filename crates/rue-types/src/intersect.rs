use id_arena::Arena;
use indexmap::{IndexMap, IndexSet};
use log::{Level, log_enabled, trace};

use crate::{
    BuiltinTypes, Comparison, ComparisonContext, Intersection, Memo, OverlapMemo, Pair, Struct,
    Type, TypeId, Union, compare, compare_impl, intersection_members, is_generic,
    reaches_non_contractive_cycle, resolve_refs, step, stringify_impl, substitute,
    types_may_overlap, unwrap_semantic_without_substitution,
};

/// The maximum number of nested intersections before giving up on being more precise.
const MAX_DEPTH: usize = 25;

/// Computes a type which contains every value that is in both `lhs` and `rhs`.
///
/// The result may be wider than the exact intersection, but it is never narrower. This makes it
/// sound to narrow a value to the result whenever it's known to be of both types at once (for
/// example, if two type guards on the same path have both succeeded or both failed).
///
/// Unions are distributed over, and pairs, atoms, and instances of the same struct are intersected
/// element by element. If no better approximation can be found for some part of the type (such as
/// for generic types), `rhs` is used for that part.
pub fn intersect(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    lhs: TypeId,
    rhs: TypeId,
) -> TypeId {
    let lhs = substitute(arena, lhs);
    let rhs = substitute(arena, rhs);

    // These should be rejected before reaching this point, but they would recurse forever
    if reaches_non_contractive_cycle(arena, lhs) || reaches_non_contractive_cycle(arena, rhs) {
        return rhs;
    }

    let names = log_enabled!(Level::Trace).then(|| {
        (
            stringify_impl(arena, lhs, &mut IndexMap::new()),
            stringify_impl(arena, rhs, &mut IndexMap::new()),
        )
    });

    if let Some((lhs_name, rhs_name)) = &names {
        trace!("Intersecting {lhs_name} with {rhs_name}");
    }

    let result = intersect_impl(arena, builtins, &mut IntersectContext::default(), lhs, rhs);

    if let Some((lhs_name, rhs_name)) = &names {
        let new_name = stringify_impl(arena, result, &mut IndexMap::new());
        trace!("Intersection of {lhs_name} with {rhs_name} yielded {new_name}");
    }

    result
}

#[derive(Debug, Default)]
struct IntersectContext {
    stack: IndexSet<(TypeId, TypeId)>,
    /// Completed intersections, keyed by the types and the depth (since the result falls back to
    /// `rhs` past a certain depth). The same intersections are reached through every reference to
    /// a type, which is exponential in the depth of the type.
    memo: Memo<(TypeId, TypeId), (TypeId, TypeId, usize), TypeId>,
    /// Shared by every comparison and overlap check, since they all start from an empty stack.
    /// Otherwise each intersection would compare the whole of the types it's given again.
    comparison: ComparisonContext<'static>,
    overlap: OverlapMemo,
}

fn intersect_impl(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    ctx: &mut IntersectContext,
    lhs: TypeId,
    rhs: TypeId,
) -> TypeId {
    step();

    if lhs == rhs {
        return lhs;
    }

    let item = (lhs, rhs);

    // Recursive types would otherwise be intersected forever. Falling back to `rhs` is sound,
    // since it contains every value in the intersection. The checks below don't depend on the
    // stack, so they would have returned early the first time if they were going to.
    if let Some(index) = ctx.stack.get_index_of(&item) {
        ctx.memo.reached(index);
        return rhs;
    }

    let key = (lhs, rhs, ctx.stack.len());

    if let Some(result) = ctx.memo.get(&key, ctx.stack.iter().copied()) {
        return result;
    }

    let frame = ctx.memo.enter(ctx.stack.len());
    let result = intersect_uncached(arena, builtins, ctx, lhs, rhs);
    ctx.memo.exit(frame, item, Some(key), result);

    result
}

fn intersect_uncached(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    ctx: &mut IntersectContext,
    lhs: TypeId,
    rhs: TypeId,
) -> TypeId {
    let lhs = normalize_intersection(arena, builtins, ctx, lhs);
    let rhs = normalize_intersection(arena, builtins, ctx, rhs);

    // `Any` is permissive, so it's assignable to everything and can't be used for comparisons
    if is_any(arena, lhs) {
        return rhs;
    }

    if is_any(arena, rhs) {
        return lhs;
    }

    // The types have already been substituted, so they can be compared directly
    if compare_impl(arena, builtins, &mut ctx.comparison, lhs, rhs, None, None)
        == Comparison::Assign
    {
        return lhs;
    }

    if compare_impl(arena, builtins, &mut ctx.comparison, rhs, lhs, None, None)
        == Comparison::Assign
    {
        return rhs;
    }

    // This doesn't generate a check between the types, since that can be exponential in their
    // depth if they reference the same type more than once
    if !types_may_overlap(arena, builtins, lhs, rhs, &mut ctx.overlap) {
        return builtins.never;
    }

    // Falling back to `rhs` is sound for the same reason as for recursive types
    if ctx.stack.len() >= MAX_DEPTH {
        return rhs;
    }

    ctx.stack.insert((lhs, rhs));
    let result = intersect_structure(arena, builtins, ctx, lhs, rhs);
    ctx.stack.pop().unwrap();

    result
}

fn intersect_structure(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    ctx: &mut IntersectContext,
    lhs: TypeId,
    rhs: TypeId,
) -> TypeId {
    match (arena[lhs].clone(), arena[rhs].clone()) {
        (Type::Apply(_), _) | (_, Type::Apply(_)) => unreachable!(),
        (Type::Ref(lhs), _) => intersect_impl(arena, builtins, ctx, lhs, rhs),
        (_, Type::Ref(rhs)) => intersect_impl(arena, builtins, ctx, lhs, rhs),
        // There's no structure to intersect with, so the newer type is used as is
        (Type::Unresolved | Type::Any, _) | (_, Type::Unresolved | Type::Any) => rhs,
        // This is done before aliases and unions are unwrapped, so that they're kept intact in the
        // structure of the result (such as `T & Bool` rather than `T & true | T & false`)
        (Type::Generic(_) | Type::Intersection(_), _)
        | (_, Type::Generic(_) | Type::Intersection(_)) => {
            let mut members = intersection_members(arena, lhs);
            members.extend(intersection_members(arena, rhs));
            intersect_members(arena, builtins, ctx, members)
        }
        (Type::Alias(alias), _) => intersect_impl(arena, builtins, ctx, alias.inner, rhs),
        (_, Type::Alias(alias)) => intersect_impl(arena, builtins, ctx, lhs, alias.inner),
        (Type::Union(union), _) => {
            let variants = union
                .types
                .iter()
                .map(|&variant| intersect_impl(arena, builtins, ctx, variant, rhs))
                .collect();
            repackage_union(arena, builtins, lhs, &union, variants)
        }
        (_, Type::Union(union)) => {
            let variants = union
                .types
                .iter()
                .map(|&variant| intersect_impl(arena, builtins, ctx, lhs, variant))
                .collect();
            repackage_union(arena, builtins, rhs, &union, variants)
        }
        (Type::Function(_), _) | (_, Type::Function(_)) => rhs,
        (Type::Struct(lhs_struct), Type::Struct(rhs_struct)) => {
            let inner = intersect_impl(arena, builtins, ctx, lhs_struct.inner, rhs_struct.inner);

            if lhs_struct.semantic == rhs_struct.semantic {
                repackage_struct(arena, builtins, lhs, lhs_struct, inner)
            } else if is_never(arena, inner) {
                builtins.never
            } else {
                // There's no way to represent a value which is an instance of two different
                // structs at once, so the newer type is used as is
                rhs
            }
        }
        (Type::Struct(ty), _) => {
            let inner = intersect_impl(arena, builtins, ctx, ty.inner, rhs);
            repackage_struct(arena, builtins, lhs, ty, inner)
        }
        (_, Type::Struct(ty)) => {
            let inner = intersect_impl(arena, builtins, ctx, lhs, ty.inner);
            repackage_struct(arena, builtins, rhs, ty, inner)
        }
        (Type::Pair(lhs_pair), Type::Pair(rhs_pair)) => {
            let first = intersect_impl(arena, builtins, ctx, lhs_pair.first, rhs_pair.first);
            let rest = intersect_impl(arena, builtins, ctx, lhs_pair.rest, rhs_pair.rest);

            if is_never(arena, first) || is_never(arena, rest) {
                builtins.never
            } else if first == lhs_pair.first && rest == lhs_pair.rest {
                lhs
            } else if first == rhs_pair.first && rest == rhs_pair.rest {
                rhs
            } else {
                arena.alloc(Type::Pair(Pair::new(first, rest)))
            }
        }
        (Type::Atom(_), Type::Atom(_)) => {
            // An atom's restriction is either a single value or a fixed length, so if both atoms
            // overlap, one restriction contains the other and only the semantics can differ. The
            // atom with the narrower restriction is kept, preferring `lhs` if they're the same.
            if compare(arena, builtins, lhs, rhs) <= Comparison::Cast {
                lhs
            } else if compare(arena, builtins, rhs, lhs) <= Comparison::Cast {
                rhs
            } else {
                builtins.never
            }
        }
        (Type::Atom(_), Type::Pair(_)) | (Type::Pair(_), Type::Atom(_)) => builtins.never,
        (Type::Never, _) | (_, Type::Never) => builtins.never,
    }
}

/// Intersects the members of an intersection, where at least one is a generic. Nothing is known
/// about the values of a generic, so it can't be narrowed itself. Instead, the result is an
/// intersection of the generics with the intersection of everything else (the structure).
fn intersect_members(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    ctx: &mut IntersectContext,
    members: Vec<TypeId>,
) -> TypeId {
    let mut generics = Vec::new();
    let mut structure = None;

    for member in members {
        if is_generic(arena, member) {
            let generic = resolve_refs(arena, member);

            if !generics.contains(&generic) {
                generics.push(generic);
            }
        } else {
            structure = Some(match structure {
                Some(structure) => intersect_impl(arena, builtins, ctx, structure, member),
                None => member,
            });
        }
    }

    // A structure that every generic is already assignable to (such as `Atom | (Any, Any)`)
    // doesn't narrow them any further
    let structure = structure.filter(|&structure| {
        generics.is_empty()
            || generics.iter().any(|&generic| {
                compare_impl(
                    arena,
                    builtins,
                    &mut ctx.comparison,
                    generic,
                    structure,
                    None,
                    None,
                ) != Comparison::Assign
            })
    });

    if let Some(structure) = structure {
        if is_never(arena, structure) {
            return builtins.never;
        }

        generics.push(structure);
    }

    if generics.len() == 1 {
        generics[0]
    } else {
        arena.alloc(Type::Intersection(Intersection::new(generics)))
    }
}

/// Substituting the generics of an intersection can leave it with more than one member that isn't
/// a generic (such as `Int & Atom` for `T & Atom` with `T` mapped to `Int`), since substitution
/// can't intersect them. They're intersected here, before it's compared to anything.
fn normalize_intersection(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    ctx: &mut IntersectContext,
    id: TypeId,
) -> TypeId {
    let Type::Intersection(intersection) = &arena[resolve_refs(arena, id)] else {
        return id;
    };

    let members = intersection_members(arena, id);
    let generics = members
        .iter()
        .filter(|&&member| is_generic(arena, member))
        .count();

    if generics > 0 && members.len() - generics <= 1 && members.len() == intersection.types.len() {
        return id;
    }

    intersect_members(arena, builtins, ctx, members)
}

/// Rebuilds a union from the intersection of each of its variants, dropping the ones which were
/// found to be impossible.
fn repackage_union(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    original: TypeId,
    union: &Union,
    variants: Vec<TypeId>,
) -> TypeId {
    if variants == union.types {
        return original;
    }

    let mut kept = Vec::new();

    for variant in variants {
        if !is_never(arena, variant) && !kept.contains(&variant) {
            kept.push(variant);
        }
    }

    match kept.len() {
        0 => builtins.never,
        1 => kept[0],
        _ => arena.alloc(Type::Union(Union::new(kept))),
    }
}

/// Rebuilds a struct around its intersected inner type, keeping its semantic identity.
fn repackage_struct(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    original: TypeId,
    ty: Struct,
    inner: TypeId,
) -> TypeId {
    if is_never(arena, inner) {
        builtins.never
    } else if inner == ty.inner {
        original
    } else {
        let ty = ty.narrow(arena, builtins, inner);
        arena.alloc(Type::Struct(ty))
    }
}

// This and `is_never` don't substitute the type again, since it already has been and doing so
// for every intersection would make it quadratic in the size of the types
fn is_any(arena: &Arena<Type>, id: TypeId) -> bool {
    let id = unwrap_semantic_without_substitution(arena, id, true);
    matches!(arena[id], Type::Any)
}

fn is_never(arena: &Arena<Type>, id: TypeId) -> bool {
    let id = unwrap_semantic_without_substitution(arena, id, true);
    matches!(arena[id], Type::Never)
}

#[cfg(test)]
mod tests {
    use indexmap::IndexSet;
    use rue_diagnostic::Name;

    use crate::{Alias, Atom, AtomRestriction, AtomSemantic, Generic, stringify};

    use super::*;

    struct Db {
        arena: Arena<Type>,
        builtins: BuiltinTypes,
    }

    impl Db {
        fn new() -> Self {
            let mut arena = Arena::new();
            let builtins = BuiltinTypes::new(&mut arena);
            Self { arena, builtins }
        }

        fn intersect(&mut self, lhs: TypeId, rhs: TypeId) -> TypeId {
            intersect(&mut self.arena, &self.builtins, lhs, rhs)
        }

        fn name(&mut self, id: TypeId) -> String {
            stringify(&mut self.arena, id)
        }

        fn pair(&mut self, first: TypeId, rest: TypeId) -> TypeId {
            self.arena.alloc(Type::Pair(Pair::new(first, rest)))
        }

        fn union(&mut self, types: &[TypeId]) -> TypeId {
            self.arena.alloc(Type::Union(Union::new(types.to_vec())))
        }

        fn atom(&mut self, atom: Atom) -> TypeId {
            self.arena.alloc(Type::Atom(atom))
        }

        fn generic(&mut self, name: &str) -> TypeId {
            self.arena.alloc(Type::Generic(Generic {
                name: Some(Name::new(name, None)),
            }))
        }

        fn strukt(&mut self, name: &str, inner: TypeId) -> TypeId {
            let id = self.arena.alloc(Type::Unresolved);
            *self.arena.get_mut(id).unwrap() = Type::Struct(Struct {
                name: Some(Name::new(name, None)),
                semantic: id,
                inner,
                generics: vec![],
                fields: IndexSet::new(),
                nil_terminated: false,
                narrowed: false,
            });
            id
        }

        /// Creates `type Name = nil | (item, Name)`.
        fn list(&mut self, name: &str, item: TypeId) -> TypeId {
            let id = self.arena.alloc(Type::Unresolved);
            let pair = self.pair(item, id);
            let inner = self.union(&[self.builtins.nil, pair]);
            *self.arena.get_mut(id).unwrap() = Type::Alias(Alias {
                name: Some(Name::new(name, None)),
                inner,
                generics: vec![],
            });
            id
        }
    }

    #[test]
    fn test_intersect_subset() {
        let mut db = Db::new();
        let b = db.builtins;

        assert_eq!(db.intersect(b.bytes32, b.bytes), b.bytes32);
        assert_eq!(db.intersect(b.bytes, b.bytes32), b.bytes32);
        assert_eq!(db.intersect(b.int, b.int), b.int);
    }

    #[test]
    fn test_intersect_any() {
        let mut db = Db::new();
        let b = db.builtins;

        assert_eq!(db.intersect(b.permissive_any, b.int), b.int);
        assert_eq!(db.intersect(b.int, b.permissive_any), b.int);
    }

    #[test]
    fn test_intersect_generics() {
        let mut db = Db::new();
        let b = db.builtins;

        // Nothing is known about the values of a generic type, so it's kept along with the other
        // type, in either order
        let t = db.generic("T");
        let t_int = db.intersect(t, b.int);
        assert_eq!(db.name(t_int), "T & Int");
        let ty = db.intersect(b.int, t);
        assert_eq!(db.name(ty), "T & Int");

        // Unless the other type doesn't narrow it
        assert_eq!(db.intersect(t, b.permissive_any), t);
        assert_eq!(db.intersect(t, b.recursive_any), t);
        assert_eq!(db.intersect(t, t), t);

        // Aliases and unions are kept intact
        let bool = db.arena.alloc(Type::Alias(Alias {
            name: Some(Name::new("Bool", None)),
            inner: b.bool,
            generics: vec![],
        }));
        let ty = db.intersect(t, bool);
        assert_eq!(db.name(ty), "T & Bool");
        let int_or_nil = db.union(&[b.int, b.nil]);
        let ty = db.intersect(t, int_or_nil);
        assert_eq!(db.name(ty), "T & (Int | nil)");

        // But a union with a generic is distributed over
        let t_or_int = db.union(&[t, b.int]);
        let ty = db.intersect(t_or_int, b.atom);
        assert_eq!(db.name(ty), "T & Atom | Int");

        // Unless a narrowed generic is assignable to another variant
        let t_or_nil = db.union(&[t, b.nil]);
        assert_eq!(db.intersect(t_or_nil, b.nil), b.nil);

        // An intersection's structure is narrowed further
        let t_atom = db.intersect(t, b.atom);
        let ty = db.intersect(t_atom, b.int);
        assert_eq!(db.name(ty), "T & Int");
        let ty = db.intersect(t_atom, b.bytes32);
        assert_eq!(db.name(ty), "T & Bytes32");
        for ty in [
            db.intersect(t_atom, t),
            db.intersect(t, t_atom),
            db.intersect(t_atom, b.atom),
        ] {
            assert_eq!(db.name(ty), "T & Atom");
        }

        // And it's impossible if its structure is
        let pair = db.pair(b.permissive_any, b.permissive_any);
        let ty = db.intersect(t_atom, pair);
        assert_eq!(db.name(ty), "Never");
        let t_pair = db.intersect(t, pair);
        assert_eq!(db.name(t_pair), "T & (Any, Any)");
        let ty = db.intersect(t_pair, t_atom);
        assert_eq!(db.name(ty), "Never");

        // Different generics are all kept
        let u = db.generic("U");
        let ty = db.intersect(t_atom, u);
        assert_eq!(db.name(ty), "T & U & Atom");
        let ty = db.intersect(t, u);
        assert_eq!(db.name(ty), "T & U");

        // Within a pair, only the generic's position is an intersection
        let lhs = db.pair(t, b.permissive_any);
        let rhs = db.pair(b.int, b.bytes32);
        let ty = db.intersect(lhs, rhs);
        assert_eq!(db.name(ty), "(T & Int, Bytes32)");

        // A struct is kept as the structure
        let inner = db.pair(b.int, b.nil);
        let point = db.strukt("Point", inner);
        let ty = db.intersect(point, t);
        assert_eq!(db.name(ty), "T & Point");
    }

    #[test]
    fn test_intersect_fallback() {
        let mut db = Db::new();
        let b = db.builtins;

        // Nothing is known about how a function type relates to the other type, so the second
        // type is used as is
        let function = db.arena.alloc(Type::Function(crate::FunctionType {
            generics: vec![],
            params: vec![],
            nil_terminated: true,
            ret: b.int,
        }));
        assert_eq!(db.intersect(function, b.atom), b.atom);
    }

    #[test]
    fn test_intersect_atoms() {
        let mut db = Db::new();
        let b = db.builtins;

        // The narrower restriction is kept, even if the semantics differ
        assert_eq!(db.intersect(b.int, b.bytes32), b.bytes32);
        assert_eq!(db.intersect(b.bytes32, b.int), b.bytes32);
        assert_eq!(db.intersect(b.public_key, b.bytes), b.public_key);

        // A value fits within a length restriction of the same length
        let value = db.atom(Atom::new(
            AtomSemantic::Bytes,
            Some(AtomRestriction::Value(vec![0; 32].into())),
        ));
        assert_eq!(db.intersect(b.bytes32, value), value);

        // `nil` and `false` are the same value, so neither is unreachable
        assert_eq!(db.intersect(b.bool, b.nil), b.bool_false);

        // But atoms with different lengths or values can't overlap
        assert_eq!(db.intersect(b.bytes32, b.public_key), b.never);
        assert_eq!(db.intersect(b.bool_true, b.nil), b.never);
        assert_eq!(db.intersect(b.nil, b.bytes32), b.never);
        assert_eq!(db.intersect(value, b.public_key), b.never);
    }

    #[test]
    fn test_intersect_disjoint() {
        let mut db = Db::new();
        let b = db.builtins;

        let pair = db.pair(b.int, b.int);
        assert_eq!(db.intersect(b.bytes32, pair), b.never);
        assert_eq!(db.intersect(pair, b.bytes32), b.never);
    }

    #[test]
    fn test_intersect_pairs() {
        let mut db = Db::new();
        let b = db.builtins;

        // Each element is intersected separately
        let lhs = db.pair(b.int, b.permissive_any);
        let rhs = db.pair(b.permissive_any, b.bytes32);
        let ty = db.intersect(lhs, rhs);
        assert_eq!(db.name(ty), "(Int, Bytes32)");

        // Nested pairs are intersected as well
        let inner = db.pair(b.permissive_any, b.int);
        let lhs = db.pair(b.int, inner);
        let rhs_inner = db.pair(b.bytes32, b.permissive_any);
        let rhs = db.pair(b.permissive_any, rhs_inner);
        let ty = db.intersect(lhs, rhs);
        assert_eq!(db.name(ty), "(Int, (Bytes32, Int))");

        // If either element can't overlap, neither can the pair
        let lhs = db.pair(b.bytes32, b.permissive_any);
        let rhs = db.pair(b.public_key, b.int);
        assert_eq!(db.intersect(lhs, rhs), b.never);

        let lhs = db.pair(b.permissive_any, b.nil);
        let rhs = db.pair(b.int, inner);
        assert_eq!(db.intersect(lhs, rhs), b.never);

        // Only some of the values in each element overlap, so the pair is still reachable
        let first = db.union(&[b.bytes32, b.public_key]);
        let rest = db.union(&[b.nil, inner]);
        let lhs = db.pair(first, rest);
        let rhs = db.pair(b.public_key, inner);
        let ty = db.intersect(lhs, rhs);
        assert_eq!(db.name(ty), "(PublicKey, (Any, Int))");
    }

    #[test]
    fn test_intersect_unions() {
        let mut db = Db::new();
        let b = db.builtins;

        let pair = db.pair(b.int, b.int);
        let a = db.union(&[b.bytes32, b.nil, pair]);
        let c = db.union(&[b.nil, pair]);
        let d = db.union(&[b.bytes32, pair]);

        let ty = db.intersect(a, c);
        assert_eq!(db.name(ty), "nil | (Int, Int)");

        let ty = db.intersect(c, d);
        assert_eq!(db.name(ty), "(Int, Int)");

        let ty = db.intersect(a, b.atom);
        assert_eq!(db.name(ty), "Bytes32 | nil");

        // Each variant is intersected with each variant of the other side
        let any_bytes32 = db.pair(b.permissive_any, b.bytes32);
        let int_any = db.pair(b.int, b.permissive_any);
        let lhs = db.union(&[b.nil, int_any]);
        let rhs = db.union(&[b.public_key, any_bytes32]);
        let ty = db.intersect(lhs, rhs);
        assert_eq!(db.name(ty), "(Int, Bytes32)");

        // No variant overlaps
        let lhs = db.union(&[b.nil, pair]);
        let rhs = db.union(&[b.bytes32, b.public_key]);
        assert_eq!(db.intersect(lhs, rhs), b.never);

        // Only some of the variants overlap
        let lhs = db.union(&[b.nil, pair, b.bytes32]);
        let rhs = db.union(&[b.int, int_any]);
        let ty = db.intersect(lhs, rhs);
        assert_eq!(db.name(ty), "nil | (Int, Int) | Bytes32");
    }

    #[test]
    fn test_intersect_structs() {
        let mut db = Db::new();
        let b = db.builtins;

        let first = db.union(&[b.int, b.nil]);
        let rest = db.union(&[b.bytes32, b.nil]);
        let inner = db.pair(first, rest);
        let point = db.strukt("Point", inner);

        // The struct is kept, with its fields intersected
        let any_bytes32 = db.pair(b.permissive_any, b.bytes32);
        let ty = db.intersect(point, any_bytes32);
        let Type::Struct(ty_struct) = db.arena[ty].clone() else {
            panic!("expected a struct");
        };
        assert_eq!(ty_struct.semantic, point);
        assert_eq!(db.name(ty_struct.inner), "(Int | nil, Bytes32)");

        // Intersecting two instances of the same struct intersects their fields
        let Type::Struct(point_struct) = db.arena[point].clone() else {
            unreachable!();
        };
        let narrowed_inner = db.pair(b.int, rest);
        let narrowed_first = db.arena.alloc(Type::Struct(Struct {
            inner: narrowed_inner,
            ..point_struct
        }));
        let ty = db.intersect(narrowed_first, ty);
        let Type::Struct(ty_struct) = db.arena[ty].clone() else {
            panic!("expected a struct");
        };
        assert_eq!(ty_struct.semantic, point);
        assert_eq!(db.name(ty_struct.inner), "(Int, Bytes32)");

        // The struct's fields can't overlap with the other type
        let pair_any = db.pair(b.recursive_any_pair, b.permissive_any);
        assert_eq!(db.intersect(point, pair_any), b.never);
        assert_eq!(db.intersect(pair_any, point), b.never);

        // Only some of the struct's fields overlap with the other type
        let nil_any = db.pair(b.nil, b.permissive_any);
        let maybe_point = db.union(&[point, b.nil]);
        let ty = db.intersect(maybe_point, nil_any);
        let Type::Struct(ty_struct) = db.arena[ty].clone() else {
            panic!("expected a struct");
        };
        assert_eq!(ty_struct.semantic, point);
        assert_eq!(db.name(ty_struct.inner), "(nil, Bytes32 | nil)");

        // Different structs with disjoint fields can't overlap
        let other_inner = db.pair(b.recursive_any_pair, b.nil);
        let other = db.strukt("Other", other_inner);
        assert_eq!(db.intersect(point, other), b.never);

        // But different structs with overlapping fields fall back to the newer type
        let other_inner = db.pair(b.int, b.nil);
        let other = db.strukt("Other", other_inner);
        let ty = db.intersect(point, other);
        let Type::Struct(ty_struct) = db.arena[ty].clone() else {
            panic!("expected a struct");
        };
        assert_eq!(ty_struct.semantic, other);
        assert_eq!(db.name(ty_struct.inner), "(Int, nil)");
    }

    #[test]
    fn test_intersect_aliases() {
        let mut db = Db::new();
        let b = db.builtins;

        let inner = db.union(&[b.int, b.nil]);
        let alias = db.arena.alloc(Type::Alias(Alias {
            name: Some(Name::new("Maybe", None)),
            inner,
            generics: vec![],
        }));

        // The alias is unwrapped to intersect its inner type
        let ty = db.intersect(alias, b.bytes32);
        assert_eq!(db.name(ty), "Bytes32");

        let ty = db.intersect(alias, b.nil);
        assert_eq!(ty, b.nil);

        let pair = db.pair(b.int, b.int);
        assert_eq!(db.intersect(alias, pair), b.never);
    }

    #[test]
    fn test_intersect_recursive() {
        let mut db = Db::new();
        let b = db.builtins;

        let ints = db.list("Ints", b.int);
        let bytes = db.list("Bytes32s", b.bytes32);
        let pairs_item = db.pair(b.int, b.int);
        let pairs = db.list("Pairs", pairs_item);

        // The first item of the list is narrowed, and the rest falls back to the newer type
        // since it would recurse forever otherwise
        let ty = db.intersect(ints, bytes);
        assert_eq!(db.name(ty), "nil | (Bytes32, Bytes32s)");

        // Only an empty list is in both
        let ty = db.intersect(ints, pairs);
        assert_eq!(ty, b.nil);

        // A single item list
        let single = db.pair(b.permissive_any, b.nil);
        let ty = db.intersect(ints, single);
        assert_eq!(db.name(ty), "(Int, nil)");

        // A list of pairs can't contain atoms
        let atom_items = db.pair(b.bytes32, b.permissive_any);
        let ty = db.intersect(pairs, atom_items);
        assert_eq!(db.name(ty), "Never");

        // The recursive `Any` type overlaps with everything
        let ty = db.intersect(b.recursive_any, ints);
        assert_eq!(db.name(ty), "Ints");
    }
}
