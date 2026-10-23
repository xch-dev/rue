//! Tests for comparison, inference, and substitution across the different kinds of types.

use std::collections::HashMap;

use id_arena::Arena;
use indexmap::{IndexMap, IndexSet};
use rue_diagnostic::Name;

use crate::{
    Alias, Apply, Atom, AtomRestriction, AtomSemantic, BuiltinTypes, Comparison, FunctionType,
    Generic, Inference, InferenceCandidates, Pair, Struct, Type, TypeId, Union,
    add_expected_bounds, check, compare, compare_with_inference, extract_atoms, extract_functions,
    extract_pairs, extract_structs, infer_from_expected, is_non_contractive, resolve_inference,
    stringify, substitute, substitute_with_mappings, subtract, unwrap_semantic,
};

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

    fn alloc(&mut self, ty: Type) -> TypeId {
        self.arena.alloc(ty)
    }

    fn placeholder(&mut self) -> TypeId {
        self.alloc(Type::Unresolved)
    }

    fn set(&mut self, id: TypeId, ty: Type) {
        *self.arena.get_mut(id).unwrap() = ty;
    }

    fn pair(&mut self, first: TypeId, rest: TypeId) -> TypeId {
        self.alloc(Type::Pair(Pair::new(first, rest)))
    }

    fn union(&mut self, types: &[TypeId]) -> TypeId {
        self.alloc(Type::Union(Union::new(types.to_vec())))
    }

    fn generic(&mut self, name: &str) -> TypeId {
        self.alloc(Type::Generic(Generic {
            name: Some(Name::new(name, None)),
        }))
    }

    fn function(&mut self, params: &[TypeId], ret: TypeId) -> TypeId {
        self.function_with(&[], params, true, ret)
    }

    fn function_with(
        &mut self,
        generics: &[TypeId],
        params: &[TypeId],
        nil_terminated: bool,
        ret: TypeId,
    ) -> TypeId {
        self.alloc(Type::Function(FunctionType {
            generics: generics.to_vec(),
            params: params
                .iter()
                .enumerate()
                .map(|(i, &ty)| (format!("p{i}"), ty))
                .collect::<IndexMap<_, _>>(),
            nil_terminated,
            ret,
        }))
    }

    /// Creates an alias whose inner type is a `Ref`, like the compiler does.
    fn alias(&mut self, name: &str, generics: &[TypeId], inner: TypeId) -> TypeId {
        let inner = self.alloc(Type::Ref(inner));
        self.alloc(Type::Alias(Alias {
            name: Some(Name::new(name, None)),
            inner,
            generics: generics.to_vec(),
        }))
    }

    /// Declares an alias whose inner type can be filled in later, to allow recursion.
    fn declare_alias(&mut self, name: &str, generics: &[TypeId]) -> TypeId {
        let inner = self.placeholder();
        self.alloc(Type::Alias(Alias {
            name: Some(Name::new(name, None)),
            inner,
            generics: generics.to_vec(),
        }))
    }

    /// Declares a struct whose inner type can be filled in later, to allow recursion.
    fn declare_struct(&mut self, name: &str, generics: &[TypeId]) -> TypeId {
        let id = self.placeholder();
        let inner = self.placeholder();
        self.set(
            id,
            Type::Struct(Struct {
                name: Some(Name::new(name, None)),
                semantic: id,
                inner,
                generics: generics.to_vec(),
                fields: IndexSet::new(),
                nil_terminated: true,
                narrowed: false,
            }),
        );
        id
    }

    fn define(&mut self, id: TypeId, resolved: TypeId) {
        let (Type::Alias(Alias { inner, .. }) | Type::Struct(Struct { inner, .. })) =
            self.arena[id].clone()
        else {
            panic!("expected an alias or struct");
        };
        self.set(inner, Type::Ref(resolved));
    }

    fn strukt(&mut self, name: &str, inner: TypeId) -> TypeId {
        let id = self.declare_struct(name, &[]);
        self.define(id, inner);
        id
    }

    fn apply(&mut self, inner: TypeId, generics: &[(TypeId, TypeId)]) -> TypeId {
        self.alloc(Type::Apply(Apply::new(
            inner,
            generics.iter().copied().collect::<HashMap<_, _>>(),
        )))
    }

    fn list(&mut self, item: TypeId) -> TypeId {
        let (list, generic) = (self.builtins.list, self.builtins.list_generic);
        self.apply(list, &[(generic, item)])
    }

    fn cmp(&mut self, lhs: TypeId, rhs: TypeId) -> Comparison {
        compare(&mut self.arena, &self.builtins, lhs, rhs)
    }

    fn infer(&mut self, lhs: TypeId, rhs: TypeId, infer: &mut Inference) -> Comparison {
        compare_with_inference(&mut self.arena, &self.builtins, lhs, rhs, Some(infer))
    }

    fn name(&mut self, id: TypeId) -> String {
        stringify(&mut self.arena, id)
    }

    fn literal(&mut self, semantic: AtomSemantic, value: &[u8]) -> TypeId {
        self.alloc(Type::Atom(Atom::new(
            semantic,
            Some(AtomRestriction::Value(value.to_vec().into())),
        )))
    }
}

mod functions {
    use super::*;

    #[test]
    fn identical() {
        let mut db = Db::new();
        let (int, bytes32) = (db.builtins.int, db.builtins.bytes32);
        let a = db.function(&[int], bytes32);
        let b = db.function(&[int], bytes32);
        assert_eq!(db.cmp(a, b), Comparison::Assign);
    }

    #[test]
    fn params_are_contravariant() {
        let mut db = Db::new();
        let (bytes, bytes32) = (db.builtins.bytes, db.builtins.bytes32);
        let takes_bytes = db.function(&[bytes], bytes32);
        let takes_bytes32 = db.function(&[bytes32], bytes32);

        // A function that accepts any bytes can be used where only 32 bytes will be passed
        assert_eq!(db.cmp(takes_bytes, takes_bytes32), Comparison::Assign);

        // But not the other way around, since it could be called with arbitrary bytes
        assert_eq!(db.cmp(takes_bytes32, takes_bytes), Comparison::Invalid);
    }

    #[test]
    fn return_is_covariant() {
        let mut db = Db::new();
        let (bytes, bytes32, int) = (db.builtins.bytes, db.builtins.bytes32, db.builtins.int);
        let returns_bytes = db.function(&[int], bytes);
        let returns_bytes32 = db.function(&[int], bytes32);
        assert_eq!(db.cmp(returns_bytes32, returns_bytes), Comparison::Assign);
        assert_eq!(db.cmp(returns_bytes, returns_bytes32), Comparison::Invalid);
    }

    #[test]
    fn cast_params_and_return() {
        let mut db = Db::new();
        let (bytes, int) = (db.builtins.bytes, db.builtins.int);

        // Reinterpreting the arguments or return value with the same representation is a cast
        let takes_int = db.function(&[int], int);
        let takes_bytes = db.function(&[bytes], int);
        assert_eq!(db.cmp(takes_int, takes_bytes), Comparison::Cast);
        assert_eq!(db.cmp(takes_bytes, takes_int), Comparison::Cast);

        let returns_bytes = db.function(&[int], bytes);
        assert_eq!(db.cmp(takes_int, returns_bytes), Comparison::Cast);
    }

    #[test]
    fn cast_cannot_narrow_params() {
        let mut db = Db::new();
        let (bytes, bytes32) = (db.builtins.bytes, db.builtins.bytes32);
        let takes_bytes32 = db.function(&[bytes32], bytes32);
        let takes_bytes = db.function(&[bytes], bytes32);

        // This would let the function be called with a value that isn't 32 bytes long
        assert_eq!(db.cmp(takes_bytes32, takes_bytes), Comparison::Invalid);
    }

    #[test]
    fn arity() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let one = db.function(&[int], int);
        let two = db.function(&[int, int], int);
        let zero = db.function(&[], int);
        assert_eq!(db.cmp(one, two), Comparison::Invalid);
        assert_eq!(db.cmp(two, one), Comparison::Invalid);
        assert_eq!(db.cmp(zero, one), Comparison::Invalid);
        assert_eq!(db.cmp(zero, zero), Comparison::Assign);
    }

    #[test]
    fn spread() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let bytes32 = db.builtins.bytes32;
        let bytes = db.builtins.bytes;
        let list_bytes = db.list(bytes);
        let list_bytes32 = db.list(bytes32);
        let spread_bytes = db.function_with(&[], &[int, list_bytes], false, int);
        let spread_bytes32 = db.function_with(&[], &[int, list_bytes32], false, int);
        let fixed = db.function_with(&[], &[int, list_bytes], true, int);

        assert_eq!(db.cmp(spread_bytes, spread_bytes32), Comparison::Assign);
        assert_eq!(db.cmp(spread_bytes32, spread_bytes), Comparison::Invalid);
        assert_eq!(db.cmp(spread_bytes, fixed), Comparison::Invalid);
        assert_eq!(db.cmp(fixed, spread_bytes), Comparison::Invalid);
    }

    #[test]
    fn unions_of_functions() {
        let mut db = Db::new();
        let (bytes, bytes32, nil, int) = (
            db.builtins.bytes,
            db.builtins.bytes32,
            db.builtins.nil,
            db.builtins.int,
        );
        let takes_bytes = db.function(&[bytes], int);
        let takes_bytes32 = db.function(&[bytes32], int);
        let optional_bytes32 = db.union(&[nil, takes_bytes32]);
        let optional_bytes = db.union(&[nil, takes_bytes]);

        assert_eq!(db.cmp(takes_bytes, optional_bytes32), Comparison::Assign);
        assert_eq!(db.cmp(takes_bytes32, optional_bytes), Comparison::Invalid);
        assert_eq!(db.cmp(optional_bytes, optional_bytes32), Comparison::Assign);
        assert_eq!(
            db.cmp(optional_bytes32, optional_bytes),
            Comparison::Invalid
        );
        assert_eq!(db.cmp(optional_bytes32, takes_bytes32), Comparison::Invalid);

        let either = db.union(&[takes_bytes, takes_bytes32]);
        assert_eq!(db.cmp(either, takes_bytes32), Comparison::Assign);
        assert_eq!(db.cmp(either, takes_bytes), Comparison::Invalid);
    }

    #[test]
    fn functions_and_other_types() {
        let mut db = Db::new();
        let (int, atom, any, recursive_any) = (
            db.builtins.int,
            db.builtins.atom,
            db.builtins.permissive_any,
            db.builtins.recursive_any,
        );
        let function = db.function(&[int], int);
        let pair = db.pair(int, int);

        assert_eq!(db.cmp(function, any), Comparison::Assign);
        assert_eq!(db.cmp(function, recursive_any), Comparison::Assign);
        assert_eq!(db.cmp(any, function), Comparison::Invalid);
        assert_eq!(db.cmp(recursive_any, function), Comparison::Invalid);
        assert_eq!(db.cmp(function, atom), Comparison::Invalid);
        assert_eq!(db.cmp(atom, function), Comparison::Invalid);
        assert_eq!(db.cmp(function, pair), Comparison::Invalid);
        assert_eq!(db.cmp(pair, function), Comparison::Invalid);
    }

    #[test]
    fn generic_function_types_are_instantiated() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let t = db.generic("T");
        let u = db.generic("U");
        let identity_t = db.function_with(&[t], &[t], true, t);
        let identity_u = db.function_with(&[u], &[u], true, u);
        let concrete = db.function(&[int], int);

        // A generic function can be instantiated to a concrete or renamed function type, but a
        // concrete function doesn't work for every type
        assert_eq!(db.cmp(identity_t, identity_t), Comparison::Assign);
        assert_eq!(db.cmp(concrete, identity_t), Comparison::Invalid);
        assert_eq!(db.cmp(identity_t, concrete), Comparison::Assign);
        assert_eq!(db.cmp(identity_t, identity_u), Comparison::Assign);
    }

    #[test]
    fn free_generics_are_not_bound_by_function_types() {
        let mut db = Db::new();
        let u = db.generic("U");

        // `fn(p0: U) -> U` with a free `U` (such as in the body of `fn foo<U>`), and the type
        // of `foo` itself, which is generic over the same `U`
        let free = db.function(&[u], u);
        let identity = db.function_with(&[u], &[u], true, u);

        // The free `U` is a single type, but the generic function can be instantiated with it
        assert_eq!(db.cmp(free, identity), Comparison::Invalid);
        assert_eq!(db.cmp(identity, free), Comparison::Assign);
    }

    #[test]
    fn nested_function_generics_are_distinct() {
        let mut db = Db::new();
        let int = db.builtins.int;

        // `fn<U>(p0: U) -> (U, fn<U>(p0: U) -> (U, Int))`, where the inner `U` shadows the outer
        let u = db.generic("U");
        let inner = db.pair(u, int);
        let inner = db.function_with(&[u], &[u], true, inner);
        let outer = db.pair(u, inner);
        let expected = db.function_with(&[u], &[u], true, outer);

        // `fn<V>(p0: V) -> (V, fn(p0: V) -> (V, Int))` only takes the outer argument's type again
        let v = db.generic("V");
        let inner = db.pair(v, int);
        let inner = db.function(&[v], inner);
        let outer = db.pair(v, inner);
        let same = db.function_with(&[v], &[v], true, outer);
        assert_eq!(db.cmp(same, expected), Comparison::Invalid);

        // `fn<V>(p0: V) -> (V, fn<W>(p0: W) -> (W, Int))` takes any type
        let w = db.generic("W");
        let inner = db.pair(w, int);
        let inner = db.function_with(&[w], &[w], true, inner);
        let outer = db.pair(v, inner);
        let distinct = db.function_with(&[v], &[v], true, outer);
        assert_eq!(db.cmp(distinct, expected), Comparison::Assign);
    }

    #[test]
    fn recursive_instantiations_are_checked() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let pair = db.pair(int, int);

        // type F = fn<U>(x: U) -> (U, F);
        let f = db.declare_alias("F", &[]);
        let u = db.generic("U");
        let ret = db.pair(u, f);
        let function = db.function_with(&[u], &[u], true, ret);
        db.define(f, function);

        // type R = (Int, G2); type G2 = fn(x: X) -> R; type G = fn(x: Int) -> R;
        for (param, expected) in [(int, Comparison::Assign), (pair, Comparison::Invalid)] {
            let r = db.declare_alias("R", &[]);
            let g2 = db.declare_alias("G2", &[]);
            let inner = db.pair(int, g2);
            db.define(r, inner);
            let function = db.function(&[param], r);
            db.define(g2, function);
            let g = db.function(&[int], r);

            // `U` is instantiated again for `G2`, where it must also be returned as an `Int`
            assert_eq!(db.cmp(f, g), expected);
        }
    }

    #[test]
    fn recursive_function_type() {
        let mut db = Db::new();
        let int = db.builtins.int;

        // type F = fn(f: F) -> Int;
        let f = db.declare_alias("F", &[]);
        let function = db.function(&[f], int);
        db.define(f, function);

        // type G = fn(g: G) -> Int;
        let g = db.declare_alias("G", &[]);
        let function = db.function(&[g], int);
        db.define(g, function);

        assert!(!is_non_contractive(&mut db.arena, f));
        assert_eq!(db.cmp(f, f), Comparison::Assign);
        assert_eq!(db.cmp(f, g), Comparison::Assign);
    }
}

mod structs {
    use super::*;

    #[test]
    fn nominal() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);
        let shape = db.pair(int, nil);
        let a = db.strukt("A", shape);
        let b = db.strukt("B", shape);

        assert_eq!(db.cmp(a, a), Comparison::Assign);

        // Structs with the same shape are semantically different
        assert_eq!(db.cmp(a, b), Comparison::Cast);
        assert_eq!(db.cmp(b, a), Comparison::Cast);

        // Structs can be cast to and from their shape
        assert_eq!(db.cmp(a, shape), Comparison::Cast);
        assert_eq!(db.cmp(shape, a), Comparison::Cast);

        // But not to an incompatible shape
        let bytes32 = db.builtins.bytes32;
        let other = db.pair(bytes32, nil);
        assert_eq!(db.cmp(a, other), Comparison::Invalid);
        assert_eq!(db.cmp(a, int), Comparison::Invalid);
    }

    #[test]
    fn unions_of_structs() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);
        let shape = db.pair(int, nil);
        let a = db.strukt("A", shape);
        let b = db.strukt("B", shape);
        let either = db.union(&[a, b]);
        let optional = db.union(&[a, nil]);

        assert_eq!(db.cmp(a, either), Comparison::Assign);
        assert_eq!(db.cmp(either, a), Comparison::Cast);
        assert_eq!(db.cmp(a, optional), Comparison::Assign);
        assert_eq!(db.cmp(nil, optional), Comparison::Assign);
        assert_eq!(db.cmp(optional, a), Comparison::Invalid);
    }

    #[test]
    fn generic() {
        let mut db = Db::new();
        let (int, nil, bytes, bytes32) = (
            db.builtins.int,
            db.builtins.nil,
            db.builtins.bytes,
            db.builtins.bytes32,
        );

        // struct Box<T> { value: T }
        let t = db.generic("T");
        let boxed = db.declare_struct("Box", &[t]);
        let inner = db.pair(t, nil);
        db.define(boxed, inner);

        let box_int = db.apply(boxed, &[(t, int)]);
        let box_int_2 = db.apply(boxed, &[(t, int)]);
        let box_bytes = db.apply(boxed, &[(t, bytes)]);
        let box_bytes32 = db.apply(boxed, &[(t, bytes32)]);

        assert_eq!(db.name(box_int), "Box<Int>");
        assert_eq!(db.cmp(box_int, box_int_2), Comparison::Assign);
        assert_eq!(db.cmp(box_bytes32, box_bytes), Comparison::Assign);
        assert_eq!(db.cmp(box_int, box_bytes), Comparison::Cast);
        assert_eq!(db.cmp(box_bytes, box_bytes32), Comparison::Invalid);
        assert_eq!(db.cmp(box_int, box_bytes32), Comparison::Invalid);
    }

    #[test]
    fn rest_field() {
        let mut db = Db::new();
        let (int, bytes, bytes32) = (db.builtins.int, db.builtins.bytes, db.builtins.bytes32);

        // struct R { x: Int, ...rest: Bytes32 }
        let shape = db.pair(int, bytes32);
        let r = db.strukt("R", shape);

        let pair = db.pair(int, bytes32);
        let wider = db.pair(int, bytes);
        assert_eq!(db.cmp(r, pair), Comparison::Cast);
        assert_eq!(db.cmp(pair, r), Comparison::Cast);
        assert_eq!(db.cmp(wider, r), Comparison::Invalid);
    }

    #[test]
    fn nested() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);

        // struct Inner { ...v: Int }
        // struct Outer { ...i: Inner }
        let inner = db.strukt("Inner", int);
        let outer = db.strukt("Outer", inner);

        // The same shape, but semantically different at either level
        let other_inner = db.strukt("OtherInner", int);
        let other_outer = db.strukt("OtherOuter", other_inner);
        let rewrapped = db.strukt("Rewrapped", inner);

        let t = db.generic("T");
        let boxed = db.declare_struct("Box", &[t]);
        let shape = db.pair(t, nil);
        db.define(boxed, shape);

        // Positions to compare the structs in, where the first three hold only the struct itself
        let positions = |db: &mut Db, ty: TypeId| {
            let alias = db.alias("A", &[], ty);
            let nested_alias = db.alias("B", &[], alias);
            let optional = db.union(&[ty, nil]);
            let optional_alias = db.union(&[alias, nil]);
            let alias_optional = db.alias("C", &[], optional);
            let pair = db.pair(optional, int);
            let applied = db.apply(boxed, &[(t, optional)]);
            [
                ty,
                alias,
                nested_alias,
                optional,
                optional_alias,
                alias_optional,
                pair,
                applied,
            ]
        };

        let lhs = positions(&mut db, outer);
        let same = positions(&mut db, outer);
        let different = [other_outer, rewrapped, inner].map(|ty| positions(&mut db, ty));

        for (i, &lhs) in lhs.iter().enumerate() {
            let targets = match i {
                0..3 => 0..6,
                3..6 => 3..6,
                _ => i..i + 1,
            };

            for j in targets {
                assert_eq!(db.cmp(lhs, same[j]), Comparison::Assign);

                for different in &different {
                    assert_eq!(db.cmp(lhs, different[j]), Comparison::Cast);
                }
            }
        }

        // An identical copy of an alias of nested structs
        let copy = db.alias("A", &[], outer);
        assert_eq!(db.cmp(same[1], copy), Comparison::Assign);
        assert_eq!(db.cmp(copy, same[1]), Comparison::Assign);

        // A union of the struct and an alias of it
        let union = db.union(&[outer, same[1]]);
        assert_eq!(db.cmp(union, outer), Comparison::Assign);

        for different in &different {
            assert_eq!(db.cmp(union, different[0]), Comparison::Cast);
        }
    }

    #[test]
    fn unions_of_instances() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);

        // struct Box<T> { ...value: T }
        let t = db.generic("T");
        let boxed = db.declare_struct("Box", &[t]);
        db.define(boxed, t);

        // Like pairs, structs aren't distributed over unions
        let optional = db.union(&[int, nil]);
        let box_optional = db.apply(boxed, &[(t, optional)]);
        let box_int = db.apply(boxed, &[(t, int)]);
        let box_nil = db.apply(boxed, &[(t, nil)]);
        let either = db.union(&[box_int, box_nil]);
        assert_eq!(db.cmp(box_optional, either), Comparison::Cast);
        assert_eq!(db.cmp(either, box_optional), Comparison::Assign);

        // But they can still be cast to a union of their shape
        let pair = db.pair(int, int);
        let shape = db.union(&[int, pair]);
        let w = db.strukt("W", shape);
        assert_eq!(db.cmp(w, shape), Comparison::Cast);
        assert_eq!(db.cmp(box_optional, optional), Comparison::Cast);

        // Instances of different structs with the same shape
        let a = db.strukt("A", int);
        let b = db.strukt("B", int);
        let box_a = db.apply(boxed, &[(t, a)]);
        let box_b = db.apply(boxed, &[(t, b)]);
        let optional_box_b = db.union(&[box_b, nil]);
        assert_eq!(db.cmp(box_a, optional_box_b), Comparison::Cast);
    }

    #[test]
    fn recursive_through_pair() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);

        // struct Node { value: Int, ...next: Node | nil }
        let node = db.declare_struct("Node", &[]);
        let next = db.union(&[node, nil]);
        let inner = db.pair(int, next);
        db.define(node, inner);

        // struct Other { value: Int, ...next: Other | nil }
        let other = db.declare_struct("Other", &[]);
        let next = db.union(&[other, nil]);
        let inner = db.pair(int, next);
        db.define(other, inner);

        assert!(!is_non_contractive(&mut db.arena, node));
        assert_eq!(db.cmp(node, node), Comparison::Assign);
        assert_eq!(db.cmp(node, other), Comparison::Cast);
        assert_eq!(db.cmp(node, int), Comparison::Invalid);
        assert_eq!(db.name(node), "Node");
    }

    #[test]
    fn extract_narrowed_variants() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);

        // struct A { x: Int | nil }, and a copy of it with `x` narrowed to `Int`
        let field = db.union(&[int, nil]);
        let shape = db.pair(field, nil);
        let a = db.strukt("A", shape);
        let Type::Struct(declared) = db.arena[a].clone() else {
            panic!("expected a struct");
        };
        let narrowed_shape = db.pair(int, nil);
        let narrowed = db.alloc(Type::Struct(Struct {
            inner: narrowed_shape,
            ..declared.clone()
        }));

        // Every instance of the same struct is extracted, even through aliases and nested unions
        let alias = db.alias("Alias", &[], narrowed);
        let nested = db.union(&[a, db.builtins.never]);
        let union = db.union(&[alias, nested]);
        let structs = extract_structs(&mut db.arena, union).unwrap();
        assert_eq!(structs.len(), 2);
        assert!(structs.iter().all(|ty| ty.semantic == declared.semantic));
        assert_eq!(db.name(structs[0].inner), "(Int, nil)");
        assert_eq!(db.name(structs[1].inner), "(Int | nil, nil)");

        // But not if any variant is a different struct, or not a struct at all
        let b = db.strukt("B", shape);
        let different = db.union(&[a, b]);
        assert!(extract_structs(&mut db.arena, different).is_none());

        let optional = db.union(&[a, nil]);
        assert!(extract_structs(&mut db.arena, optional).is_none());
    }
}

mod aliases {
    use super::*;

    #[test]
    fn transparent() {
        let mut db = Db::new();
        let (int, bytes) = (db.builtins.int, db.builtins.bytes);
        let alias = db.alias("Amount", &[], int);
        assert_eq!(db.cmp(alias, int), Comparison::Assign);
        assert_eq!(db.cmp(int, alias), Comparison::Assign);
        assert_eq!(db.cmp(alias, bytes), Comparison::Cast);
    }

    #[test]
    fn generic_apply() {
        let mut db = Db::new();
        let (int, bytes, bytes32) = (db.builtins.int, db.builtins.bytes, db.builtins.bytes32);

        // type Twice<T> = (T, T);
        let t = db.generic("T");
        let pair = db.pair(t, t);
        let twice = db.alias("Twice", &[t], pair);

        let twice_int = db.apply(twice, &[(t, int)]);
        let int_int = db.pair(int, int);
        let int_bytes32 = db.pair(int, bytes32);
        assert_eq!(db.name(twice_int), "Twice<Int>");
        assert_eq!(db.cmp(int_int, twice_int), Comparison::Assign);
        assert_eq!(db.cmp(twice_int, int_int), Comparison::Assign);
        assert_eq!(db.cmp(int_bytes32, twice_int), Comparison::Cast);

        let twice_bytes32 = db.apply(twice, &[(t, bytes32)]);
        let twice_bytes = db.apply(twice, &[(t, bytes)]);
        assert_eq!(db.cmp(twice_bytes32, twice_bytes), Comparison::Assign);
        assert_eq!(db.cmp(twice_bytes, twice_bytes32), Comparison::Invalid);
    }
}

mod unions_and_pairs {
    use super::*;

    #[test]
    fn unions() {
        let mut db = Db::new();
        let (int, nil, bytes, bytes32) = (
            db.builtins.int,
            db.builtins.nil,
            db.builtins.bytes,
            db.builtins.bytes32,
        );
        let optional = db.union(&[int, nil]);
        let mixed = db.union(&[int, bytes32]);

        assert_eq!(db.cmp(int, optional), Comparison::Assign);
        assert_eq!(db.cmp(nil, optional), Comparison::Assign);
        assert_eq!(db.cmp(optional, int), Comparison::Cast);
        assert_eq!(db.cmp(optional, bytes32), Comparison::Invalid);
        assert_eq!(db.cmp(mixed, bytes), Comparison::Cast);
        assert_eq!(db.cmp(mixed, bytes32), Comparison::Invalid);
        assert_eq!(db.cmp(mixed, mixed), Comparison::Assign);

        let empty = db.union(&[]);
        assert_eq!(db.cmp(empty, int), Comparison::Assign);
        assert_eq!(db.cmp(int, empty), Comparison::Invalid);
    }

    #[test]
    fn pairs() {
        let mut db = Db::new();
        let (int, bytes, bytes32) = (db.builtins.int, db.builtins.bytes, db.builtins.bytes32);
        let narrow = db.pair(int, bytes32);
        let wide = db.pair(int, bytes);
        let cast = db.pair(bytes, bytes);

        assert_eq!(db.cmp(narrow, wide), Comparison::Assign);
        assert_eq!(db.cmp(wide, narrow), Comparison::Invalid);
        assert_eq!(db.cmp(narrow, cast), Comparison::Cast);
        assert_eq!(db.cmp(narrow, int), Comparison::Invalid);
        assert_eq!(db.cmp(int, narrow), Comparison::Invalid);
    }

    #[test]
    fn special_types() {
        let mut db = Db::new();
        let BuiltinTypes {
            int,
            never,
            unresolved,
            permissive_any: any,
            recursive_any,
            ..
        } = db.builtins;
        let pair = db.pair(int, int);

        assert_eq!(db.cmp(never, int), Comparison::Assign);
        assert_eq!(db.cmp(never, pair), Comparison::Assign);
        assert_eq!(db.cmp(int, never), Comparison::Invalid);
        assert_eq!(db.cmp(int, any), Comparison::Assign);
        assert_eq!(db.cmp(pair, any), Comparison::Assign);
        assert_eq!(db.cmp(any, int), Comparison::Invalid);
        assert_eq!(db.cmp(any, recursive_any), Comparison::Assign);
        assert_eq!(db.cmp(recursive_any, any), Comparison::Assign);
        assert_eq!(db.cmp(pair, recursive_any), Comparison::Assign);
        assert_eq!(db.cmp(unresolved, int), Comparison::Assign);
        assert_eq!(db.cmp(int, unresolved), Comparison::Assign);
    }
}

mod recursive {
    use super::*;

    #[test]
    fn list() {
        let mut db = Db::new();
        let (int, nil, bytes, bytes32) = (
            db.builtins.int,
            db.builtins.nil,
            db.builtins.bytes,
            db.builtins.bytes32,
        );
        let list_int = db.list(int);
        let list_bytes = db.list(bytes);
        let list_bytes32 = db.list(bytes32);
        let one = db.pair(int, nil);
        let two = db.pair(int, one);
        let bad = db.pair(int, int);

        assert!(!is_non_contractive(&mut db.arena, list_int));
        assert_eq!(db.cmp(list_int, list_int), Comparison::Assign);
        assert_eq!(db.cmp(list_bytes32, list_bytes), Comparison::Assign);
        assert_eq!(db.cmp(list_int, list_bytes), Comparison::Cast);
        assert_eq!(db.cmp(list_bytes, list_bytes32), Comparison::Invalid);
        assert_eq!(db.cmp(nil, list_int), Comparison::Assign);
        assert_eq!(db.cmp(two, list_int), Comparison::Assign);
        assert_eq!(db.cmp(bad, list_int), Comparison::Invalid);
        assert_eq!(db.cmp(list_int, two), Comparison::Invalid);
        assert_eq!(db.name(list_int), "List<Int>");
    }

    #[test]
    fn alternating_list() {
        let mut db = Db::new();
        let BuiltinTypes {
            int,
            nil,
            bytes,
            bytes32,
            alternating_list,
            alternating_list_generic_a: a,
            alternating_list_generic_b: b,
            ..
        } = db.builtins;
        let list = db.apply(alternating_list, &[(a, int), (b, bytes32)]);
        let wider = db.apply(alternating_list, &[(a, int), (b, bytes)]);

        let tail = db.pair(bytes32, nil);
        let good = db.pair(int, tail);
        let tail = db.pair(int, nil);
        let bad = db.pair(int, tail);
        let tail = db.pair(int, good);
        let odd = db.pair(bytes32, tail);

        assert!(!is_non_contractive(&mut db.arena, list));
        assert_eq!(db.cmp(good, list), Comparison::Assign);
        assert_eq!(db.cmp(bad, list), Comparison::Invalid);
        assert_eq!(db.cmp(odd, list), Comparison::Invalid);
        assert_eq!(db.cmp(list, wider), Comparison::Assign);
        assert_eq!(db.cmp(wider, list), Comparison::Invalid);
    }

    #[test]
    fn recursive_any() {
        let mut db = Db::new();
        let recursive_any = db.builtins.recursive_any;
        assert!(!is_non_contractive(&mut db.arena, recursive_any));
    }

    /// `type A = B; type B = A;`
    fn alias_cycle(db: &mut Db) -> (TypeId, TypeId) {
        let a = db.declare_alias("A", &[]);
        let b = db.declare_alias("B", &[]);
        db.define(a, b);
        db.define(b, a);
        (a, b)
    }

    /// `type Loop = Loop | Int;`
    fn union_cycle(db: &mut Db) -> TypeId {
        let int = db.builtins.int;
        let looped = db.declare_alias("Loop", &[]);
        let union = db.union(&[looped, int]);
        db.define(looped, union);
        looped
    }

    /// `struct S { ...inner: S }`
    fn struct_cycle(db: &mut Db) -> TypeId {
        let s = db.declare_struct("S", &[]);
        db.define(s, s);
        s
    }

    /// `type Id<T> = T; type X = Id<X>;`
    fn generic_cycle(db: &mut Db) -> TypeId {
        let t = db.generic("T");
        let id = db.alias("Id", &[t], t);
        let x = db.declare_alias("X", &[]);
        let apply = db.apply(id, &[(t, x)]);
        db.define(x, apply);
        x
    }

    /// `type R<T> = R<T>;`
    fn generic_self_cycle(db: &mut Db) -> TypeId {
        let t = db.generic("T");
        let r = db.declare_alias("R", &[t]);
        let apply = db.apply(r, &[(t, t)]);
        db.define(r, apply);
        r
    }

    fn non_contractive_types(db: &mut Db) -> Vec<TypeId> {
        let (a, b) = alias_cycle(db);
        vec![
            a,
            b,
            union_cycle(db),
            struct_cycle(db),
            generic_cycle(db),
            generic_self_cycle(db),
        ]
    }

    #[test]
    fn detects_non_contractive_types() {
        let mut db = Db::new();

        for ty in non_contractive_types(&mut db) {
            assert!(is_non_contractive(&mut db.arena, ty));
        }

        // A type that merely refers to a non-contractive type isn't reported itself
        let (a, _) = alias_cycle(&mut db);
        let refers = db.alias("Refers", &[], a);
        assert!(!is_non_contractive(&mut db.arena, refers));

        // A struct containing itself through a pair is fine
        let nil = db.builtins.nil;
        let s = db.declare_struct("S", &[]);
        let inner = db.pair(s, nil);
        db.define(s, inner);
        assert!(!is_non_contractive(&mut db.arena, s));
    }

    #[test]
    fn non_contractive_types_are_not_universally_assignable() {
        let mut db = Db::new();
        let BuiltinTypes {
            int, bytes32, nil, ..
        } = db.builtins;
        let pair = db.pair(int, int);

        for ty in non_contractive_types(&mut db) {
            assert_ne!(db.cmp(pair, ty), Comparison::Assign);
            assert_ne!(db.cmp(ty, bytes32), Comparison::Assign);
            assert_ne!(db.cmp(ty, nil), Comparison::Assign);
        }

        let (a, b) = alias_cycle(&mut db);
        assert_eq!(db.cmp(pair, a), Comparison::Invalid);
        assert_eq!(db.cmp(a, bytes32), Comparison::Invalid);
        assert_eq!(db.cmp(a, b), Comparison::Invalid);

        let s = struct_cycle(&mut db);
        assert_eq!(db.cmp(s, bytes32), Comparison::Invalid);
        assert_eq!(db.cmp(pair, s), Comparison::Invalid);
    }

    #[test]
    fn non_contractive_types_do_not_overflow() {
        let mut db = Db::new();
        let BuiltinTypes {
            int, bytes32, nil, ..
        } = db.builtins;

        for ty in non_contractive_types(&mut db) {
            let _ = db.name(ty);
            let _ = substitute(&mut db.arena, ty);
            let _ = unwrap_semantic(&mut db.arena, ty, false);
            let _ = unwrap_semantic(&mut db.arena, ty, true);
            let _ = extract_atoms(&mut db.arena, ty, false);
            let _ = extract_atoms(&mut db.arena, ty, true);
            let _ = extract_pairs(&mut db.arena, ty, false);
            let _ = extract_pairs(&mut db.arena, ty, true);
            let _ = extract_functions(&mut db.arena, ty);
            let _ = extract_structs(&mut db.arena, ty);
            let _ = check(&mut db.arena, &db.builtins, ty, bytes32);
            let _ = check(&mut db.arena, &db.builtins, int, ty);
            let _ = subtract(&mut db.arena, &db.builtins, ty, int);

            let pair = db.pair(ty, int);
            let int_pair = db.pair(int, int);
            let _ = check(&mut db.arena, &db.builtins, pair, int_pair);
            let _ = check(&mut db.arena, &db.builtins, int_pair, pair);
            let _ = subtract(&mut db.arena, &db.builtins, pair, int_pair);
            let _ = db.cmp(ty, ty);
            let _ = db.cmp(int, ty);
            let union = db.union(&[ty, nil]);
            let _ = db.cmp(union, bytes32);

            let mut infer = Inference::new([]);
            let _ = db.infer(ty, int, &mut infer);
        }
    }

    /// These functions don't track cycles themselves, so they rely on non-contractive types being
    /// rejected (and their cycles broken) by the compiler before they're used.
    #[test]
    fn broken_cycles_are_safe() {
        let mut db = Db::new();
        let BuiltinTypes {
            int,
            bytes32,
            unresolved,
            ..
        } = db.builtins;

        let (a, b) = alias_cycle(&mut db);
        let looped = union_cycle(&mut db);
        let s = struct_cycle(&mut db);

        // This is what the compiler does after reporting the type
        db.define(b, unresolved);
        db.define(looped, unresolved);
        db.define(s, unresolved);

        for ty in [a, b, looped, s] {
            assert!(!is_non_contractive(&mut db.arena, ty));
            let _ = check(&mut db.arena, &db.builtins, ty, bytes32);
            let _ = check(&mut db.arena, &db.builtins, int, ty);
            let _ = subtract(&mut db.arena, &db.builtins, ty, int);

            let pair = db.pair(ty, int);
            let int_pair = db.pair(int, int);
            let _ = check(&mut db.arena, &db.builtins, pair, int_pair);
            let _ = check(&mut db.arena, &db.builtins, int_pair, pair);
            let _ = subtract(&mut db.arena, &db.builtins, pair, int_pair);
            let _ = extract_atoms(&mut db.arena, ty, false);
            let _ = extract_pairs(&mut db.arena, ty, false);
            let _ = extract_functions(&mut db.arena, ty);
            let _ = extract_structs(&mut db.arena, ty);
            let _ = unwrap_semantic(&mut db.arena, ty, true);
        }
    }
}

mod inference {
    use super::*;

    fn candidates(infer: &Inference, generic: TypeId) -> InferenceCandidates {
        infer.candidates(generic).unwrap().clone()
    }

    #[test]
    fn covariant() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        assert_eq!(db.infer(int, t, &mut infer), Comparison::Assign);
        assert_eq!(
            candidates(&infer, t),
            InferenceCandidates {
                lower: vec![int],
                upper: vec![],
                held: vec![],
            }
        );

        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings[&t], int);
    }

    #[test]
    fn widens_covariant_candidates() {
        let mut db = Db::new();
        let (int, bytes32) = (db.builtins.int, db.builtins.bytes32);
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        let value = db.pair(int, bytes32);
        let param = db.pair(t, t);
        assert_eq!(db.infer(value, param, &mut infer), Comparison::Assign);

        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(db.name(mappings[&t]), "Int | Bytes32");
    }

    #[test]
    fn contravariant() {
        let mut db = Db::new();
        let (bytes32, int) = (db.builtins.bytes32, db.builtins.int);
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        let value = db.function(&[bytes32], int);
        let param = db.function(&[t], int);
        assert_eq!(db.infer(value, param, &mut infer), Comparison::Assign);
        assert_eq!(
            candidates(&infer, t),
            InferenceCandidates {
                lower: vec![],
                upper: vec![bytes32],
                held: vec![],
            }
        );

        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings[&t], bytes32);
    }

    #[test]
    fn most_specific_upper_bound() {
        let mut db = Db::new();
        let (bytes, bytes32, int) = (db.builtins.bytes, db.builtins.bytes32, db.builtins.int);
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        let takes_bytes = db.function(&[bytes], int);
        let takes_bytes32 = db.function(&[bytes32], int);
        let value = db.pair(takes_bytes, takes_bytes32);
        let takes_t = db.function(&[t], int);
        let param = db.pair(takes_t, takes_t);
        db.infer(value, param, &mut infer);

        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings[&t], bytes32);
    }

    #[test]
    fn nested_function_parameters_are_covariant() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        // fn(f: fn(x: Int) -> Int) -> Int to fn(f: fn(x: T) -> Int) -> Int
        let inner_value = db.function(&[int], int);
        let value = db.function(&[inner_value], int);
        let inner_param = db.function(&[t], int);
        let param = db.function(&[inner_param], int);
        db.infer(value, param, &mut infer);

        assert_eq!(
            candidates(&infer, t),
            InferenceCandidates {
                lower: vec![int],
                upper: vec![],
                held: vec![],
            }
        );
    }

    #[test]
    fn mixed_variance_must_be_rechecked() {
        let mut db = Db::new();
        let (int, bytes32) = (db.builtins.int, db.builtins.bytes32);
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        // (Int, fn(x: Bytes32) -> Bytes32) to (T, fn(x: T) -> Bytes32)
        let takes_bytes32 = db.function(&[bytes32], bytes32);
        let value = db.pair(int, takes_bytes32);
        let takes_t = db.function(&[t], bytes32);
        let param = db.pair(t, takes_t);
        db.infer(value, param, &mut infer);

        assert_eq!(
            candidates(&infer, t),
            InferenceCandidates {
                lower: vec![int],
                upper: vec![bytes32],
                held: vec![],
            }
        );

        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings[&t], int);

        let param = substitute_with_mappings(&mut db.arena, param, &mappings);
        assert_eq!(db.cmp(value, param), Comparison::Invalid);
    }

    #[test]
    fn rigid_function_generics_are_not_inferred() {
        let mut db = Db::new();
        let t = db.generic("T");
        let u = db.generic("U");
        let v = db.generic("V");
        let mut infer = Inference::new([t]);

        // fn<V>(p0: V) -> V to fn<U>(p0: U) -> T
        let identity = db.function_with(&[v], &[v], true, v);
        let param = db.function_with(&[u], &[u], true, t);
        db.infer(identity, param, &mut infer);

        // `T` can't be inferred as `U`, since it's only in scope of the parameter's type
        assert_eq!(candidates(&infer, t), InferenceCandidates::default());

        // But a function returned to where `fn<U>(p0: U) -> U` is expected can be instantiated
        // with `U`, since the expected type binds it for the whole value
        let mut infer = Inference::new([t]);
        let ret = db.function_with(&[], &[t], true, t);
        let expected = db.function_with(&[u], &[u], true, u);
        infer_from_expected(&mut db.arena, &db.builtins, ret, expected, &mut infer);
        assert_eq!(
            candidates(&infer, t),
            InferenceCandidates {
                lower: vec![u],
                upper: vec![u],
                held: vec![],
            }
        );
    }

    #[test]
    fn only_targets_are_inferred() {
        let mut db = Db::new();
        let (int, bytes32) = (db.builtins.int, db.builtins.bytes32);
        let t = db.generic("T");
        let u = db.generic("U");
        let mut infer = Inference::new([t]);

        // U isn't being inferred, so it's rigid
        assert_eq!(db.infer(int, u, &mut infer), Comparison::Invalid);
        assert!(infer.candidates(u).is_none());

        // The generic of a generic function type in a parameter isn't inferred either
        let takes_bytes32 = db.function(&[bytes32], bytes32);
        let identity_u = db.function_with(&[u], &[u], true, u);
        assert_eq!(
            db.infer(takes_bytes32, identity_u, &mut infer),
            Comparison::Invalid
        );
        assert_eq!(candidates(&infer, t), InferenceCandidates::default());

        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert!(mappings.is_empty());
    }

    #[test]
    fn widens_literal_candidates() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        // Each argument of `pick(1, 2)` contributes a candidate, and both widen to `Int`
        let one = db.literal(AtomSemantic::Int, &[1]);
        let two = db.literal(AtomSemantic::Int, &[2]);
        assert_eq!(db.infer(one, t, &mut infer), Comparison::Assign);
        assert_eq!(db.infer(two, t, &mut infer), Comparison::Assign);

        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings[&t], int);
    }

    #[test]
    fn widens_literals_to_base_type() {
        let mut db = Db::new();

        for (semantic, len, expected) in [
            (AtomSemantic::Bytes, 1, db.builtins.bytes),
            (AtomSemantic::Bytes, 32, db.builtins.bytes32),
            (AtomSemantic::String, 1, db.builtins.string),
            (AtomSemantic::PublicKey, 48, db.builtins.public_key),
            (AtomSemantic::Bool, 1, db.builtins.bool),
        ] {
            let t = db.generic("T");
            let mut infer = Inference::new([t]);
            let literal = db.literal(semantic, &vec![1; len]);
            db.infer(literal, t, &mut infer);

            let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
            assert_eq!(mappings[&t], expected);
        }
    }

    #[test]
    fn keeps_nil_candidate() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        db.infer(nil, t, &mut infer);
        db.infer(int, t, &mut infer);

        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(db.name(mappings[&t]), "nil | Int");
    }

    #[test]
    fn keeps_literal_required_by_upper_bound() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        // (5, fn(x: 5) -> Int) to (T, fn(x: T) -> Int)
        let five = db.literal(AtomSemantic::Int, &[5]);
        let takes_five = db.function(&[five], int);
        let value = db.pair(five, takes_five);
        let takes_t = db.function(&[t], int);
        let param = db.pair(t, takes_t);
        db.infer(value, param, &mut infer);

        // Widening to `Int` would make the function argument invalid
        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings[&t], five);

        let param = substitute_with_mappings(&mut db.arena, param, &mappings);
        assert_eq!(db.cmp(value, param), Comparison::Assign);
    }

    #[test]
    fn through_list() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        let list_t = db.list(t);
        let list_t = substitute(&mut db.arena, list_t);
        let one = db.pair(int, nil);
        assert_eq!(db.infer(one, list_t, &mut infer), Comparison::Assign);

        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings[&t], int);
    }

    #[test]
    fn keeps_literal_required_by_expected_type() {
        let mut db = Db::new();
        let t = db.generic("T");
        let (five, six) = (
            db.literal(AtomSemantic::Int, &[5]),
            db.literal(AtomSemantic::Int, &[6]),
        );

        // `let x: 5 = id(5);` keeps the literal
        let mut expected = Inference::new([t]);
        infer_from_expected(&mut db.arena, &db.builtins, t, five, &mut expected);

        let mut infer = Inference::new([t]);
        db.infer(five, t, &mut infer);
        add_expected_bounds(&mut db.arena, &db.builtins, &mut infer, &expected);

        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings[&t], five);

        // `let x: 5 = id(6);` doesn't satisfy the expected type, so it's left to be reported
        let mut infer = Inference::new([t]);
        db.infer(six, t, &mut infer);
        add_expected_bounds(&mut db.arena, &db.builtins, &mut infer, &expected);
        assert_eq!(candidates(&infer, t).upper, vec![]);

        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings[&t], db.builtins.int);
    }

    #[test]
    fn skips_union_members_held_by_others() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        // `Int | nil` to `T | nil` only needs `T` to hold `Int`
        let value = db.union(&[int, nil]);
        let param = db.union(&[t, nil]);
        assert_eq!(db.infer(value, param, &mut infer), Comparison::Assign);
        assert_eq!(
            candidates(&infer, t),
            InferenceCandidates {
                lower: vec![int],
                upper: vec![],
                held: vec![nil],
            }
        );

        // A value that is only castable to `nil` (such as `0`) still has to be held by `T`
        let zero = db.literal(AtomSemantic::Int, &[]);
        assert_eq!(db.infer(zero, param, &mut infer), Comparison::Assign);
        assert_eq!(candidates(&infer, t).lower, vec![int, zero]);
    }

    #[test]
    fn held_union_members_are_a_last_resort() {
        let mut db = Db::new();
        let nil = db.builtins.nil;
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        // `nil` to `T | nil` doesn't constrain `T`, but it's better than nothing
        let param = db.union(&[t, nil]);
        assert_eq!(db.infer(nil, param, &mut infer), Comparison::Assign);
        assert_eq!(candidates(&infer, t).lower, vec![]);

        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings[&t], nil);
    }

    #[test]
    fn struct_contents_held_by_other_members() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);
        let generic = db.generic("T");
        let boxed = db.declare_struct("Box", &[generic]);
        let inner = db.union(&[generic, nil]);
        db.define(boxed, inner);

        // `Box<Int>` to `Box<T>` infers `T` as `Int` rather than the `Int | nil` contents
        let t = db.generic("T");
        let mut infer = Inference::new([t]);
        let value = db.apply(boxed, &[(generic, int)]);
        let param = db.apply(boxed, &[(generic, t)]);
        assert_eq!(db.infer(value, param, &mut infer), Comparison::Assign);
        assert_eq!(candidates(&infer, t).lower, vec![int]);
    }

    #[test]
    fn nested_generic_held_by_other_members() {
        let mut db = Db::new();
        let (int, bytes32, nil) = (db.builtins.int, db.builtins.bytes32, db.builtins.nil);
        let generic = db.generic("T");
        let inner = db.union(&[generic, bytes32]);
        let maybe = db.alias("Maybe", &[generic], inner);

        // `Int | nil` to `Maybe<T> | nil` only needs `T` to hold `Int`, even though `nil` would
        // also reach `T` through the alias
        let t = db.generic("T");
        let mut infer = Inference::new([t]);
        let maybe_t = db.apply(maybe, &[(generic, t)]);
        let param = db.union(&[maybe_t, nil]);
        let value = db.union(&[int, nil]);
        assert_eq!(db.infer(value, param, &mut infer), Comparison::Assign);
        assert_eq!(
            candidates(&infer, t),
            InferenceCandidates {
                lower: vec![int],
                upper: vec![],
                held: vec![nil],
            }
        );
    }
}

mod substitution {
    use super::*;

    #[test]
    fn apply() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let t = db.generic("T");
        let pair = db.pair(t, t);
        let twice = db.alias("Twice", &[t], pair);
        let applied = db.apply(twice, &[(t, int)]);

        let result = substitute(&mut db.arena, applied);
        let Type::Alias(alias) = db.arena[result].clone() else {
            panic!("expected an alias");
        };
        assert_eq!(alias.generics, vec![int]);
        assert_eq!(db.name(result), "Twice<Int>");
        assert_eq!(stringify(&mut db.arena, alias.inner), "(Int, Int)");
    }

    #[test]
    fn nested_generics() {
        let mut db = Db::new();
        let int = db.builtins.int;

        // type Twice<T> = (T, T); type Nested<U> = Twice<List<U>>;
        let t = db.generic("T");
        let pair = db.pair(t, t);
        let twice = db.alias("Twice", &[t], pair);
        let u = db.generic("U");
        let list_u = db.list(u);
        let twice_list = db.apply(twice, &[(t, list_u)]);
        let nested = db.alias("Nested", &[u], twice_list);

        let applied = db.apply(nested, &[(u, int)]);
        assert_eq!(db.name(applied), "Nested<Int>");

        let list_int = db.list(int);
        let expected = db.pair(list_int, list_int);
        assert_eq!(db.cmp(applied, expected), Comparison::Assign);
        assert_eq!(db.cmp(expected, applied), Comparison::Assign);
    }

    #[test]
    fn function_generics_are_shadowed() {
        let mut db = Db::new();
        let (int, bytes32) = (db.builtins.int, db.builtins.bytes32);
        let t = db.generic("T");
        let u = db.generic("U");
        let function = db.function_with(&[u], &[t], true, u);

        // `fn<U>(p0: T) -> U` with `T := Int` and `U := Bytes32`
        let mappings = HashMap::from_iter([(t, int), (u, bytes32)]);
        let result = substitute_with_mappings(&mut db.arena, function, &mappings);
        let Type::Function(function) = db.arena[result].clone() else {
            panic!("expected a function");
        };

        // The function's own generic is bound by it, so it isn't substituted
        assert_eq!(function.generics, vec![u]);
        assert_eq!(db.cmp(function.params[0], int), Comparison::Assign);
        assert_eq!(function.ret, u);
    }

    #[test]
    fn function_generics_are_not_captured() {
        let mut db = Db::new();
        let t = db.generic("T");
        let u = db.generic("U");
        let function = db.function_with(&[u], &[u], true, t);

        // `fn<U>(p0: U) -> T` with `T := U` refers to the outer `U` in its return type
        let mappings = HashMap::from_iter([(t, u)]);
        let result = substitute_with_mappings(&mut db.arena, function, &mappings);
        let Type::Function(function) = db.arena[result].clone() else {
            panic!("expected a function");
        };

        assert_ne!(function.generics, vec![u]);
        assert_eq!(function.params[0], function.generics[0]);
        assert_eq!(function.ret, u);

        // So it isn't the type of the identity function
        let v = db.generic("V");
        let identity = db.function_with(&[v], &[v], true, v);
        assert_eq!(db.cmp(identity, result), Comparison::Invalid);
    }

    #[test]
    fn recursive_function_generics_are_not_captured() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let any = db.alloc(Type::Any);

        // type P<T> = fn<U>(x: U) -> (T, P<U>);
        let t = db.generic("T");
        let u = db.generic("U");
        let alias = db.declare_alias("P", &[t]);
        let p_u = db.apply(alias, &[(t, u)]);
        let ret = db.pair(t, p_u);
        let function = db.function_with(&[u], &[u], true, ret);
        db.define(alias, function);
        let p_int = db.apply(alias, &[(t, int)]);

        // Calling the result of `P<Int>` returns a `P` of the argument's type
        let v = db.generic("V");
        let w = db.generic("W");
        let inner = db.pair(v, any);
        let inner = db.function_with(&[w], &[w], true, inner);
        let outer = db.pair(int, inner);
        let expected = db.function_with(&[v], &[v], true, outer);
        assert_eq!(db.cmp(p_int, expected), Comparison::Assign);

        // Not a `P` of the next argument's type
        let v = db.generic("V");
        let w = db.generic("W");
        let inner = db.pair(w, any);
        let inner = db.function_with(&[w], &[w], true, inner);
        let outer = db.pair(int, inner);
        let captured = db.function_with(&[v], &[v], true, outer);
        assert_eq!(db.cmp(p_int, captured), Comparison::Invalid);
    }

    #[test]
    fn recursion() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let t = db.generic("T");
        let list_t = db.list(t);

        let mappings = HashMap::from_iter([(t, int)]);
        let list_int = substitute_with_mappings(&mut db.arena, list_t, &mappings);
        assert_eq!(db.name(list_int), "List<Int>");

        let expected = db.list(int);
        assert_eq!(db.cmp(list_int, expected), Comparison::Assign);
        assert_eq!(db.cmp(expected, list_int), Comparison::Assign);
    }
}
